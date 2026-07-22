# ARGO-style weekly dengue nowcast (Yang et al. 2017 adapted).
#
# Protocol (reporting lag L = 4):
#   At calendar week t, cases are known only through origin = t - L.
#   Google Trends are available through t (real time).
#   From each origin, issue a recursive L-week nowcast of weeks origin+1..t.
#
# Model (log scale, as in the paper):
#   y_t = log(cases_t + 1)
#   X_k,t = log(search_k,t + 1)
#   Case lags: 1, 2, 3, 52, 104
#   AR:      y_t ~ case lags (OLS)
#   ARGO:    y_t ~ case lags + all contemporaneous search terms (L1 / glmnet)
#            with time-ordered CV for lambda at each origin.
#            Short case lags 1:3 are left unpenalised (paper-like).
#
# Training: expanding window, refit at every origin.
#
# Usage (from repo root):
#   Rscript day1-1100-dengue-exercise/scripts/simulate_argo_nowcast.R MX
#   Rscript day1-1100-dengue-exercise/scripts/simulate_argo_nowcast.R BR
#
# Outputs under outputs/simulate_argo_nowcast/{GEO}/:
#   forecasts_long.csv, metrics_by_lead.csv, metrics_by_origin.csv
#   selected_coefs_by_origin.csv
#   lead_decay_R2.png / lead_decay_MAPE.png
#   origin_performance_R2.png / origin_performance_MAPE.png
#   nowcast_vs_observed_by_lead.png
#   rolling_R2_by_origin_and_lead.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(glmnet)
})

source("day1-1100-dengue-exercise/scripts/functions/load_most_recent_file.R")
source("day1-1100-dengue-exercise/scripts/functions/geo_weekly_helpers.R")
source("day1-1100-dengue-exercise/scripts/functions/ar_deployment_functions.R")

geo <- parse_geo_arg("MX")
reporting_lag <- 4L
horizon <- reporting_lag
initial_train_weeks <- 156L
case_lags <- c(1L, 2L, 3L, 52L, 104L)
n_cv_folds <- 5L
set.seed(20260721)

out_dir <- file.path(
  "day1-1100-dengue-exercise/outputs/simulate_argo_nowcast",
  geo
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------

gtrends_data <- load_most_recent_file(
  "day1-1100-dengue-exercise/downloads",
  "googletrends_dengue.csv",
  geo = geo
) %>%
  mutate(start_date = as.Date(start_date))

case_data <- load_most_recent_file(
  "day1-1100-dengue-exercise/downloads",
  paste0("dengue_cases_", geo, "_weekly.csv"),
  geo = NULL
) %>%
  mutate(start_date = as.Date(start_date))

search_terms <- setdiff(names(gtrends_data), "start_date")
if (length(search_terms) < 1L) {
  stop("No Google Trends columns found for geo=", geo)
}

combined_dat <- left_join(gtrends_data, case_data, by = "start_date") %>%
  filter(!is.na(dengue_total)) %>%
  arrange(start_date) %>%
  mutate(
    across(all_of(search_terms), ~ as.numeric(.x)),
    dengue_total = as.numeric(dengue_total)
  )

n <- nrow(combined_dat)
if (n < initial_train_weeks + horizon) {
  stop(
    "Series too short for ARGO nowcast: n=", n,
    " needs >= ", initial_train_weeks + horizon
  )
}

message(
  "Geo: ", geo,
  " | weeks: ", n,
  " | initial train: ", initial_train_weeks,
  " | reporting lag L: ", reporting_lag,
  " | case lags: ", paste(case_lags, collapse = ","),
  " | search terms: ", length(search_terms)
)
message("Search terms: ", paste(search_terms, collapse = ", "))
message(
  "Date range: ", min(combined_dat$start_date), " to ",
  max(combined_dat$start_date),
  " | origins: ", n - initial_train_weeks - horizon + 1L
)

# ---------------------------------------------------------------------------
# Feature construction / fitting helpers
# ---------------------------------------------------------------------------

#' Time-ordered block fold IDs for glmnet CV (no random shuffling).
make_time_foldid <- function(n_obs, nfolds = 5L) {
  nfolds <- max(2L, min(as.integer(nfolds), n_obs))
  foldid <- ceiling(seq_len(n_obs) / (n_obs / nfolds))
  pmin(as.integer(foldid), nfolds)
}

#' Build log-scale design matrix for indices in `target_idx`.
#'
#' @param y_log Full log case series (length n).
#' @param x_log_mat Optional n x K matrix of log search (contemporaneous).
#' @param target_idx Integer indices of response times.
#' @param lags Integer vector of case lags.
build_design <- function(y_log, target_idx, lags, x_log_mat = NULL) {
  n_row <- length(target_idx)
  x_list <- list()
  for (lag in lags) {
    x_list[[paste0("case_lag_", lag)]] <- y_log[target_idx - lag]
  }
  if (!is.null(x_log_mat)) {
    for (j in seq_len(ncol(x_log_mat))) {
      nm <- colnames(x_log_mat)[[j]]
      x_list[[paste0("search_", nm)]] <- x_log_mat[target_idx, j]
    }
  }
  x <- do.call(cbind, x_list)
  y <- y_log[target_idx]
  ok <- is.finite(y) & apply(x, 1L, function(r) all(is.finite(r)))
  list(
    y = y[ok],
    x = x[ok, , drop = FALSE],
    target_idx = target_idx[ok],
    feature_names = colnames(x)
  )
}

#' Fit AR (OLS) or ARGO (L1 glmnet with temporal CV).
fit_argo_family <- function(
  y_hist,
  x_hist_mat = NULL,
  lags = case_lags,
  model = c("ar", "argo"),
  nfolds = n_cv_folds
) {
  model <- match.arg(model)
  y_log <- log1p(pmax(as.numeric(y_hist), 0))
  first_t <- max(lags) + 1L
  if (length(y_log) < first_t + 10L) {
    stop("Training series too short for lags up to ", max(lags))
  }

  x_log_mat <- NULL
  if (identical(model, "argo")) {
    if (is.null(x_hist_mat)) stop("ARGO requires search matrix")
    x_log_mat <- log1p(pmax(as.matrix(x_hist_mat), 0))
    storage.mode(x_log_mat) <- "double"
  }

  des <- build_design(
    y_log = y_log,
    target_idx = seq.int(first_t, length(y_log)),
    lags = lags,
    x_log_mat = x_log_mat
  )
  if (nrow(des$x) < 20L) {
    stop("Too few complete training rows: ", nrow(des$x))
  }

  if (identical(model, "ar")) {
    fit_df <- as.data.frame(des$x)
    fit_df$y <- des$y
    lm_fit <- stats::lm(y ~ ., data = fit_df)
    coefs <- stats::coef(lm_fit)
    return(list(
      model = "ar",
      type = "lm",
      fit = lm_fit,
      feature_names = des$feature_names,
      lags = lags,
      intercept = unname(coefs[["(Intercept)"]]),
      beta = setNames(unname(coefs[des$feature_names]), des$feature_names),
      lambda = NA_real_,
      n_nonzero_search = 0L
    ))
  }

  # Penalty: leave short AR lags 1:3 unpenalised (paper-like for Mexico/Brazil).
  pf <- rep(1, ncol(des$x))
  names(pf) <- colnames(des$x)
  for (lag in c(1L, 2L, 3L)) {
    nm <- paste0("case_lag_", lag)
    if (nm %in% names(pf)) pf[[nm]] <- 0
  }

  foldid <- make_time_foldid(nrow(des$x), nfolds = nfolds)
  cv_fit <- glmnet::cv.glmnet(
    x = des$x,
    y = des$y,
    family = "gaussian",
    alpha = 1,
    foldid = foldid,
    penalty.factor = pf,
    standardize = TRUE,
    intercept = TRUE
  )
  lambda_hat <- cv_fit$lambda.1se
  beta_mat <- as.matrix(stats::coef(cv_fit, s = "lambda.1se"))
  intercept <- unname(beta_mat["(Intercept)", 1])
  beta <- setNames(as.numeric(beta_mat[des$feature_names, 1]), des$feature_names)
  n_search <- sum(
    grepl("^search_", names(beta)) & is.finite(beta) & abs(beta) > 0
  )

  list(
    model = "argo",
    type = "glmnet",
    fit = cv_fit,
    feature_names = des$feature_names,
    lags = lags,
    intercept = intercept,
    beta = beta,
    lambda = lambda_hat,
    n_nonzero_search = as.integer(n_search)
  )
}

#' Recursive nowcast on log scale; return case-count predictions.
nowcast_argo_family <- function(y_hist, fit_obj, horizon, x_all_mat = NULL) {
  y_hist_log <- log1p(pmax(as.numeric(y_hist), 0))
  n0 <- length(y_hist_log)
  pred_log <- rep(NA_real_, horizon)

  x_log_all <- NULL
  if (identical(fit_obj$model, "argo")) {
    if (is.null(x_all_mat)) stop("ARGO nowcast needs full search matrix")
    x_log_all <- log1p(pmax(as.matrix(x_all_mat), 0))
  }

  for (h in seq_len(horizon)) {
    target_t <- n0 + h
    x_row <- numeric(length(fit_obj$feature_names))
    names(x_row) <- fit_obj$feature_names

    for (lag in fit_obj$lags) {
      nm <- paste0("case_lag_", lag)
      lag_t <- target_t - lag
      x_row[[nm]] <- if (lag_t <= n0) {
        y_hist_log[[lag_t]]
      } else {
        pred_log[[lag_t - n0]]
      }
    }

    if (identical(fit_obj$model, "argo")) {
      if (target_t > nrow(x_log_all)) {
        stop("Search unavailable at target index ", target_t)
      }
      for (nm in grep("^search_", fit_obj$feature_names, value = TRUE)) {
        term <- sub("^search_", "", nm)
        x_row[[nm]] <- x_log_all[target_t, term]
      }
    }

    pred_log[[h]] <- fit_obj$intercept +
      sum(fit_obj$beta[fit_obj$feature_names] * x_row[fit_obj$feature_names])
  }

  predictions <- expm1(pred_log)
  predictions[is.finite(predictions)] <- pmax(predictions[is.finite(predictions)], 0)
  predictions
}

# ---------------------------------------------------------------------------
# Expanding-window simulation
# ---------------------------------------------------------------------------

origins <- seq.int(initial_train_weeks, n - horizon)
x_all_mat <- as.matrix(combined_dat[, search_terms, drop = FALSE])
y_all <- combined_dat$dengue_total

all_forecasts <- list()
all_coefs <- list()
k <- 0L
kc <- 0L

for (oi in seq_along(origins)) {
  origin_idx <- origins[[oi]]
  origin_week <- combined_dat$start_date[[origin_idx]]
  calendar_week <- combined_dat$start_date[[origin_idx + horizon]]
  train_n <- origin_idx

  if (oi == 1L || oi %% 25L == 0L) {
    message(
      "Origin ", oi, "/", length(origins),
      " | cases through ", origin_week,
      " | nowcast through ", calendar_week,
      " | train_n=", train_n
    )
  }

  y_train <- y_all[seq_len(train_n)]
  x_train <- x_all_mat[seq_len(train_n), , drop = FALSE]
  y_future <- y_all[(train_n + 1L):(train_n + horizon)]
  dates_future <- combined_dat$start_date[(train_n + 1L):(train_n + horizon)]

  for (model_name in c("ar", "argo")) {
    fit_obj <- tryCatch(
      fit_argo_family(
        y_hist = y_train,
        x_hist_mat = if (identical(model_name, "argo")) x_train else NULL,
        lags = case_lags,
        model = model_name
      ),
      error = function(e) {
        warning(
          "Fit failed at ", origin_week, " / ", model_name, ": ",
          conditionMessage(e)
        )
        NULL
      }
    )
    if (is.null(fit_obj)) next

    preds <- tryCatch(
      nowcast_argo_family(
        y_hist = y_train,
        fit_obj = fit_obj,
        horizon = horizon,
        x_all_mat = if (identical(model_name, "argo")) x_all_mat else NULL
      ),
      error = function(e) {
        warning("Nowcast failed at ", origin_week, " / ", model_name, ": ",
                conditionMessage(e))
        rep(NA_real_, horizon)
      }
    )

    k <- k + 1L
    all_forecasts[[k]] <- tibble(
      geo = geo,
      origin_week = origin_week,
      calendar_week = calendar_week,
      prediction_week = dates_future,
      prediction_lead = seq_len(horizon),
      actual = y_future,
      predicted = as.numeric(preds),
      APE = ape(y_future, preds),
      model = model_name,
      lambda = fit_obj$lambda,
      n_nonzero_search = fit_obj$n_nonzero_search,
      train_n = train_n,
      reporting_lag = reporting_lag,
      transform = "log1p",
      case_lags = paste(case_lags, collapse = ",")
    )

    kc <- kc + 1L
    all_coefs[[kc]] <- tibble(
      geo = geo,
      origin_week = origin_week,
      model = model_name,
      feature = names(fit_obj$beta),
      coefficient = as.numeric(fit_obj$beta),
      intercept = fit_obj$intercept,
      lambda = fit_obj$lambda,
      train_n = train_n
    )
  }
}

forecasts_long <- bind_rows(all_forecasts)
coefs_long <- bind_rows(all_coefs)
stopifnot(nrow(forecasts_long) > 0L)

metrics_by_lead <- forecasts_long %>%
  group_by(geo, model, prediction_lead) %>%
  summarise(
    n_origins = sum(is.finite(actual) & is.finite(predicted)),
    R2 = r2_score(actual, predicted),
    MAPE = mape(actual, predicted),
    .groups = "drop"
  ) %>%
  arrange(model, prediction_lead)

metrics_by_origin <- forecasts_long %>%
  group_by(geo, origin_week, calendar_week, model, train_n, reporting_lag) %>%
  summarise(
    n_leads = sum(is.finite(actual) & is.finite(predicted)),
    R2 = r2_score(actual, predicted),
    MAPE = mape(actual, predicted),
    lambda = dplyr::first(lambda),
    n_nonzero_search = dplyr::first(n_nonzero_search),
    .groups = "drop"
  ) %>%
  arrange(origin_week, model)

write_csv(forecasts_long, file.path(out_dir, "forecasts_long.csv"))
write_csv(metrics_by_lead, file.path(out_dir, "metrics_by_lead.csv"))
write_csv(metrics_by_origin, file.path(out_dir, "metrics_by_origin.csv"))
write_csv(coefs_long, file.path(out_dir, "selected_coefs_by_origin.csv"))

message("Wrote ", nrow(forecasts_long), " nowcast rows")
message("Wrote metrics_by_lead (", nrow(metrics_by_lead), " rows)")
message("Wrote metrics_by_origin (", nrow(metrics_by_origin), " rows)")
message("Wrote selected_coefs_by_origin (", nrow(coefs_long), " rows)")

# ---------------------------------------------------------------------------
# Figures (AR vs ARGO)
# ---------------------------------------------------------------------------

lead_breaks <- seq_len(horizon)
series_cols <- c("AR" = "blue", "ARGO" = "#e41a1c")
series_levels <- c("AR", "ARGO")

forecasts_plot <- forecasts_long %>%
  mutate(
    origin_week = as.Date(origin_week),
    prediction_week = as.Date(prediction_week),
    series = if_else(model == "ar", "AR", "ARGO"),
    series = factor(series, levels = series_levels)
  )

metrics_lead_plot <- metrics_by_lead %>%
  mutate(
    series = if_else(model == "ar", "AR", "ARGO"),
    series = factor(series, levels = series_levels)
  )

common_theme <- theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey92", colour = NA),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

plot_lead_decay <- function(df, metric, ylab, ylim = NULL) {
  p <- ggplot(
    df,
    aes(prediction_lead, .data[[metric]], colour = series, group = series)
  ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    scale_colour_manual(values = series_cols, name = NULL) +
    scale_x_continuous(breaks = lead_breaks) +
    labs(
      x = "Nowcast lead (weeks after last observed case)",
      y = ylab,
      title = paste0(
        geo, ": ARGO nowcast skill vs lead (", ylab,
        "; reporting lag L=", reporting_lag, ")"
      ),
      subtitle = paste0(
        "Case lags ", paste(case_lags, collapse = ", "),
        "; ARGO uses all ", length(search_terms),
        " search terms with L1 + time-ordered CV"
      )
    ) +
    common_theme
  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p
}

ggsave(
  file.path(out_dir, "lead_decay_R2.png"),
  plot_lead_decay(metrics_lead_plot, "R2", expression(R^2), ylim = c(NA, 1)),
  width = 8,
  height = 5,
  dpi = 150
)
ggsave(
  file.path(out_dir, "lead_decay_MAPE.png"),
  plot_lead_decay(metrics_lead_plot, "MAPE", "MAPE (%)"),
  width = 8,
  height = 5,
  dpi = 150
)

rolling_r2 <- function(df, window = 8L, boot_reps = 300L) {
  df <- arrange(df, origin_week)
  n_row <- nrow(df)
  out <- tibble(
    origin_week = df$origin_week,
    R2 = NA_real_,
    R2_low = NA_real_,
    R2_high = NA_real_
  )
  if (n_row < window) return(out)

  for (i in seq.int(window, n_row)) {
    idx <- seq.int(i - window + 1L, i)
    y <- df$actual[idx]
    yhat <- df$predicted[idx]
    out$R2[i] <- r2_score(y, yhat)
    boot <- replicate(boot_reps, {
      b <- sample.int(window, window, replace = TRUE)
      r2_score(y[b], yhat[b])
    })
    boot <- boot[is.finite(boot)]
    if (length(boot) >= 20L) {
      ci <- quantile(boot, c(0.025, 0.975), names = FALSE)
      out$R2_low[i] <- ci[[1]]
      out$R2_high[i] <- ci[[2]]
    }
  }
  out
}

origin_data <- forecasts_plot %>%
  mutate(
    prediction_lead = factor(
      prediction_lead,
      levels = lead_breaks,
      labels = paste("Lead", lead_breaks)
    )
  )

set.seed(20260721)
rolling_r2_data <- origin_data %>%
  group_by(prediction_lead, series) %>%
  group_modify(~ rolling_r2(.x, window = 8L, boot_reps = 300L)) %>%
  ungroup()

write_csv(
  rolling_r2_data,
  file.path(out_dir, "rolling_R2_by_origin_and_lead.csv")
)

common_origin_theme <- theme_bw(base_size = 9) +
  theme(
    strip.background = element_rect(fill = "grey92", colour = NA),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_origin_r2 <- ggplot(
  rolling_r2_data,
  aes(origin_week, R2, colour = series, fill = series, group = series)
) +
  geom_ribbon(aes(ymin = R2_low, ymax = R2_high), alpha = 0.10, colour = NA) +
  geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.3) +
  geom_line(linewidth = 0.6, alpha = 0.85) +
  facet_wrap(vars(prediction_lead), ncol = horizon, scales = "free_y") +
  scale_colour_manual(values = series_cols, name = NULL) +
  scale_fill_manual(values = series_cols, name = NULL) +
  labs(
    x = "Origin week (last observed case week)",
    y = expression(R^2),
    title = paste0(geo, ": rolling ARGO nowcast performance by lead"),
    subtitle = paste0(
      "8-origin rolling predictive R² with bootstrap 95% CIs; reporting lag L = ",
      reporting_lag
    )
  ) +
  common_origin_theme

p_origin_mape <- ggplot(
  origin_data,
  aes(origin_week, APE, colour = series, group = series)
) +
  geom_line(linewidth = 0.55, alpha = 0.8) +
  facet_wrap(vars(prediction_lead), ncol = horizon, scales = "free_y") +
  scale_colour_manual(values = series_cols, name = NULL) +
  labs(
    x = "Origin week (last observed case week)",
    y = "Absolute percentage error (%)",
    title = paste0(geo, ": pointwise ARGO nowcast error by lead"),
    subtitle = paste0(
      "Pointwise APE (reporting lag L = ", reporting_lag, "); lower is better"
    )
  ) +
  common_origin_theme

ggsave(
  file.path(out_dir, "origin_performance_R2.png"),
  p_origin_r2,
  width = 12,
  height = 4.5,
  dpi = 150
)
ggsave(
  file.path(out_dir, "origin_performance_MAPE.png"),
  p_origin_mape,
  width = 12,
  height = 4.5,
  dpi = 150
)

# Nowcast vs observed by lead
obs_long <- forecasts_plot %>%
  distinct(prediction_week, prediction_lead, actual) %>%
  transmute(
    prediction_week,
    prediction_lead,
    series = "Observed",
    value = actual
  )

pred_long <- forecasts_plot %>%
  transmute(
    prediction_week,
    prediction_lead,
    series = as.character(series),
    value = predicted
  )

vs_obs <- bind_rows(obs_long, pred_long) %>%
  mutate(
    prediction_lead = factor(
      prediction_lead,
      levels = lead_breaks,
      labels = paste("Lead", lead_breaks)
    ),
    series = factor(series, levels = c("Observed", "AR", "ARGO"))
  )

line_cols <- c("Observed" = "#000000", "AR" = "blue", "ARGO" = "#e41a1c")
line_w <- c("Observed" = 0.65, "AR" = 0.4, "ARGO" = 0.4)

p_vs_obs <- ggplot(
  vs_obs,
  aes(
    prediction_week, value,
    colour = series, linewidth = series, group = series
  )
) +
  geom_line(alpha = 0.55) +
  facet_wrap(vars(prediction_lead), ncol = horizon, scales = "free_y") +
  scale_colour_manual(values = line_cols, name = NULL) +
  scale_linewidth_manual(values = line_w, name = NULL) +
  labs(
    x = "Prediction week (week being nowcast)",
    y = "Dengue cases",
    title = paste0(geo, ": ARGO nowcast vs observed by prediction lead"),
    subtitle = paste0(
      "Black = observed; blue = AR; red = ARGO (all search terms + L1). ",
      "Reporting lag L = ", reporting_lag, "."
    )
  ) +
  common_origin_theme

ggsave(
  file.path(out_dir, "nowcast_vs_observed_by_lead.png"),
  p_vs_obs,
  width = 12,
  height = 4.5,
  dpi = 150
)

message("Wrote lead_decay_R2.png / lead_decay_MAPE.png")
message("Wrote origin_performance_R2.png / origin_performance_MAPE.png")
message("Wrote nowcast_vs_observed_by_lead.png")
message("Wrote rolling_R2_by_origin_and_lead.csv")
message("Done.")
