# Simulate reporting-lag nowcasts with weekly AR / ARX + Google Trends.
#
# Protocol (reporting lag L = 4):
#   At calendar week t, official cases are known only through origin = t - L.
#   Google Trends are treated as available through t (real time).
#   From each origin, issue a recursive L-week nowcast of the intervening
#   unobserved weeks (origin+1, ..., origin+L = t).
#
# Two different "lag" concepts (do not confuse them):
#   max_lag (short-term AR order p = 3)
#     The model uses case lags 1:3 plus seasonal case lags 52 and 104 weeks.
#     ARX additionally uses the Google Trends value at the target week (lag 0),
#     because Trends are assumed available in real time.
#   prediction_lead (nowcast horizon h = 1..L)
#     How many weeks after the last *observed* case week the target sits.
#     Lead 1 = origin+1 (least delayed); lead L = calendar week t (most delayed).
#     This is an evaluation axis, not a model coefficient.
#
# Expanding window:
#   1. Start with the first `initial_train_weeks` (156 ≈ 3 years), leaving
#      one year of estimable rows after applying the 104-week seasonal lag.
#   2. At each origin, refit models and nowcast L weeks ahead.
#   3. Advance one week: the newly observed case week is appended to training.
#   4. Repeat until fewer than L weeks remain after the origin.
#
# Error propagation (recursive):
#   Lead 1 uses only observed case history through the origin.
#   Lead h>1 replaces unknown intervening case lags with earlier predictions ŷ.
#   For ARX, observed Trends are used at each nowcast week (only cases recurse).
#
# Usage (from repo root):
#   Rscript day1-1100-dengue-exercise/scripts/simulate_gtrends_nowcast.R MX
#   Rscript day1-1100-dengue-exercise/scripts/simulate_gtrends_nowcast.R BR
#
# Outputs (under outputs/simulate_gtrends_nowcast/{GEO}/):
#   forecasts_long.csv, metrics_by_lead.csv, metrics_by_origin.csv
#   lead_decay_R2.png / lead_decay_MAPE.png
#   origin_performance_R2.png / origin_performance_MAPE.png
#   nowcast_vs_observed_by_lead_p3.png
#   rolling_R2_by_origin_and_lead.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(urca)
})

source("day1-1100-dengue-exercise/scripts/functions/load_most_recent_file.R")
source("day1-1100-dengue-exercise/scripts/functions/geo_weekly_helpers.R")
source("day1-1100-dengue-exercise/scripts/functions/ar_deployment_functions.R")
source("day1-1530-ai-agents-data-scraping/scripts/functions/signal_preprocess.R")

geo <- parse_geo_arg("MX")
reporting_lag <- 4L
max_lags <- 3L
horizon <- reporting_lag
# Three years gives the first fit one year of usable rows after applying lag 104.
initial_train_weeks <- 156L
seasonal_case_lags <- c(52L, 104L)

out_dir <- file.path(
  "day1-1100-dengue-exercise/outputs/simulate_gtrends_nowcast",
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

combined_dat <- left_join(gtrends_data, case_data, by = "start_date") %>%
  filter(!is.na(dengue_total))

n <- nrow(combined_dat)
if (n < initial_train_weeks + horizon) {
  stop(
    "Series too short for nowcast sim: n=", n,
    " needs >= ", initial_train_weeks + horizon
  )
}

# Match the paper's transformation: model log(cases + 1) and log(search + 1).
# For the processed path, denoise/detrend the log-transformed search series.
log_search_dat <- combined_dat %>%
  mutate(across(all_of(predictors), ~ log1p(pmax(as.numeric(.x), 0))))

# Freeze preprocessing parameters on the initial window (deployment-realistic:
# only information available at the first origin is used for trend/denoise fits).
processed_df <- preprocess_search_columns(
  log_search_dat,
  predictors,
  train_n = initial_train_weeks
)
# Keep cases aligned for processed path (preprocess returns start_date + search only)
processed_df <- processed_df %>%
  left_join(
    combined_dat %>% select(start_date, dengue_total),
    by = "start_date"
  )

message(
  "Geo: ", geo,
  " | weeks: ", n,
  " | initial train: ", initial_train_weeks,
  " | reporting lag L: ", reporting_lag,
  " | nowcast horizon: ", horizon,
  " | short AR orders: ", paste(max_lags, collapse = ","),
  " | seasonal case lags: ", paste(seasonal_case_lags, collapse = ",")
)
message(
  "Date range: ", min(combined_dat$start_date), " to ", max(combined_dat$start_date),
  " | origins: ", n - initial_train_weeks - horizon + 1L
)

# ---------------------------------------------------------------------------
# Expanding-window nowcast simulation
# ---------------------------------------------------------------------------

#' Fit a log-linear AR or ARX model for the nowcast.
#'
#' Cases enter at short lags 1:p and seasonal lags 52 and 104. For ARX, search
#' enters contemporaneously (x_t): at each target week the model uses the most
#' recently available Google Trends value for that same week.
fit_nowcast_model <- function(
  y_train,
  max_lag,
  seasonal_lags = c(52L, 104L),
  x_train = NULL
) {
  y_log <- log1p(pmax(as.numeric(y_train), 0))
  case_lags <- sort(unique(c(seq_len(max_lag), seasonal_lags)))
  first_t <- max(case_lags) + 1L
  if (length(y_log) < first_t) {
    stop("Training series too short for case lag ", max(case_lags))
  }

  target_idx <- seq.int(first_t, length(y_log))
  fit_df <- data.frame(y = y_log[target_idx])
  for (lag in case_lags) {
    fit_df[[paste0("case_lag_", lag)]] <- y_log[target_idx - lag]
  }
  if (!is.null(x_train)) {
    fit_df$search_current <- as.numeric(x_train)[target_idx]
  }
  fit_df <- fit_df[stats::complete.cases(fit_df), , drop = FALSE]
  if (nrow(fit_df) <= ncol(fit_df)) {
    stop("Too few complete training rows after constructing model features")
  }

  list(
    model = stats::lm(y ~ ., data = fit_df),
    case_lags = case_lags,
    uses_search = !is.null(x_train)
  )
}

#' Recursive h-step nowcast on the log scale, returned on the case-count scale.
nowcast_log_linear <- function(y_hist, fit, horizon, x_all = NULL) {
  y_hist_log <- log1p(pmax(as.numeric(y_hist), 0))
  n0 <- length(y_hist_log)
  pred_log <- rep(NA_real_, horizon)

  for (h in seq_len(horizon)) {
    target_t <- n0 + h
    newdata <- data.frame(row.names = 1L)
    for (lag in fit$case_lags) {
      lag_t <- target_t - lag
      lag_value <- if (lag_t <= n0) {
        y_hist_log[[lag_t]]
      } else {
        pred_log[[lag_t - n0]]
      }
      newdata[[paste0("case_lag_", lag)]] <- lag_value
    }
    if (fit$uses_search) {
      if (is.null(x_all) || target_t > length(x_all)) {
        stop("Contemporaneous Google Trends unavailable at target index ", target_t)
      }
      newdata$search_current <- as.numeric(x_all)[[target_t]]
    }
    pred_log[[h]] <- as.numeric(stats::predict(fit$model, newdata = newdata))
  }

  predictions <- expm1(pred_log)
  predictions[is.finite(predictions)] <- pmax(predictions[is.finite(predictions)], 0)
  predictions
}

configs <- tidyr::expand_grid(
  processing = c("raw", "processed"),
  model = c("ar", "arx"),
  predictor = predictors,
  max_lag = max_lags
) %>%
  # AR-only does not depend on predictor; keep one AR row per processing/lag
  filter(!(model == "ar" & predictor != predictors[[1]])) %>%
  mutate(
    predictor = if_else(model == "ar", NA_character_, predictor)
  )

# origin_idx = last week with observed cases (= calendar time t - L)
origins <- seq.int(initial_train_weeks, n - horizon)

all_forecasts <- list()
k <- 0L

for (oi in seq_along(origins)) {
  origin_idx <- origins[[oi]]
  origin_week <- combined_dat$start_date[[origin_idx]]
  # Calendar "now" under reporting lag L: last Trends-available week
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

  for (ci in seq_len(nrow(configs))) {
    cfg <- configs[ci, ]
    p <- as.integer(cfg$max_lag)

    search_df <- if (identical(cfg$processing, "raw")) {
      combined_dat
    } else {
      processed_df
    }
    y_train <- search_df$dengue_total[seq_len(train_n)]
    y_future <- search_df$dengue_total[(train_n + 1L):(train_n + horizon)]
    dates_future <- search_df$start_date[(train_n + 1L):(train_n + horizon)]

    # Raw Trends follow the paper's log(x + 1) transform. The processed path
    # was already denoised/detrended after log transformation above.
    x_all_model <- if (identical(cfg$model, "arx")) {
      x_values <- search_df[[cfg$predictor]]
      if (identical(cfg$processing, "raw")) {
        log1p(pmax(as.numeric(x_values), 0))
      } else {
        as.numeric(x_values)
      }
    } else {
      NULL
    }

    fit <- tryCatch(
      {
        fit_nowcast_model(
          y_train = y_train,
          max_lag = p,
          seasonal_lags = seasonal_case_lags,
          x_train = if (identical(cfg$model, "arx")) {
            x_all_model[seq_len(train_n)]
          } else {
            NULL
          }
        )
      },
      error = function(e) {
        warning(
          "Fit failed at ", origin_week, " / ",
          cfg$model, " / ", cfg$processing, " / lag=", p,
          ": ", conditionMessage(e)
        )
        NULL
      }
    )
    if (is.null(fit)) next

    preds <- tryCatch(
      nowcast_log_linear(
        y_hist = y_train,
        fit = fit,
        horizon = horizon,
        x_all = x_all_model
      ),
      error = function(e) {
        warning("Nowcast failed at ", origin_week, ": ", conditionMessage(e))
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
      model = cfg$model,
      predictor = cfg$predictor,
      processing = cfg$processing,
      max_lag = p,
      case_lags = paste(c(seq_len(p), seasonal_case_lags), collapse = ","),
      search_lag = if_else(cfg$model == "arx", 0L, NA_integer_),
      transform = "log1p",
      train_n = train_n,
      reporting_lag = reporting_lag
    )
  }
}

forecasts_long <- bind_rows(all_forecasts)
stopifnot(nrow(forecasts_long) > 0L)

# ---------------------------------------------------------------------------
# Secondary metrics tables
# ---------------------------------------------------------------------------

metrics_by_lead <- forecasts_long %>%
  group_by(geo, model, predictor, processing, max_lag, prediction_lead) %>%
  summarise(
    n_origins = sum(is.finite(actual) & is.finite(predicted)),
    R2 = r2_score(actual, predicted),
    MAPE = mape(actual, predicted),
    .groups = "drop"
  ) %>%
  arrange(model, predictor, processing, max_lag, prediction_lead)

metrics_by_origin <- forecasts_long %>%
  group_by(
    geo, origin_week, calendar_week, model, predictor, processing, max_lag,
    train_n, reporting_lag
  ) %>%
  summarise(
    n_leads = sum(is.finite(actual) & is.finite(predicted)),
    R2 = r2_score(actual, predicted),
    MAPE = mape(actual, predicted),
    .groups = "drop"
  ) %>%
  arrange(origin_week, model, predictor, processing, max_lag)

write_csv(forecasts_long, file.path(out_dir, "forecasts_long.csv"))
write_csv(metrics_by_lead, file.path(out_dir, "metrics_by_lead.csv"))
write_csv(metrics_by_origin, file.path(out_dir, "metrics_by_origin.csv"))

message("Wrote ", nrow(forecasts_long), " nowcast rows -> ", file.path(out_dir, "forecasts_long.csv"))
message("Wrote metrics_by_lead (", nrow(metrics_by_lead), " rows)")
message("Wrote metrics_by_origin (", nrow(metrics_by_origin), " rows)")

# ---------------------------------------------------------------------------
# Figures (mirror plot_ar_deployment layout; leads = 1..L, AR order = 3)
# ---------------------------------------------------------------------------

term_levels <- c("dengue", "mosquito", "sintomas de dengue")
lag_levels <- as.character(max_lags)
lag_cols <- setNames(
  c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")[seq_along(max_lags)],
  lag_levels
)
lead_breaks <- seq_len(horizon)

metrics_by_lead_plot <- metrics_by_lead %>%
  mutate(
    max_lag = factor(max_lag, levels = max_lags),
    predictor_lab = case_when(
      predictor == "dengue" ~ "dengue",
      predictor == "mosquito" ~ "mosquito",
      predictor == "sintomas_de_dengue" ~ "sintomas de dengue",
      TRUE ~ as.character(predictor)
    )
  )

forecasts_plot <- forecasts_long %>%
  mutate(
    origin_week = as.Date(origin_week),
    prediction_week = as.Date(prediction_week),
    predictor_lab = case_when(
      predictor == "dengue" ~ "dengue",
      predictor == "mosquito" ~ "mosquito",
      predictor == "sintomas_de_dengue" ~ "sintomas de dengue",
      TRUE ~ as.character(predictor)
    )
  )

lead_panel_data <- function(df, metric) {
  ar <- df %>%
    filter(model == "ar", processing == "raw") %>%
    transmute(
      prediction_lead,
      max_lag,
      value = .data[[metric]],
      panel = "AR",
      predictor_lab = NA_character_
    )

  ar_grid <- tidyr::expand_grid(
    predictor_lab = term_levels,
    ar %>% select(-predictor_lab)
  ) %>%
    mutate(panel = "AR")

  arx <- df %>%
    filter(model == "arx") %>%
    transmute(
      predictor_lab,
      prediction_lead,
      max_lag,
      value = .data[[metric]],
      panel = if_else(processing == "raw", "ARX (raw)", "ARX (processed)")
    )

  bind_rows(ar_grid, arx) %>%
    mutate(
      predictor_lab = factor(predictor_lab, levels = term_levels),
      panel = factor(panel, levels = c("AR", "ARX (raw)", "ARX (processed)"))
    )
}

plot_lead_decay <- function(df, metric, ylab, ylim = NULL) {
  d <- lead_panel_data(df, metric)
  p <- ggplot(d, aes(prediction_lead, value, colour = max_lag, group = max_lag)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.6) +
    facet_grid(predictor_lab ~ panel) +
    scale_colour_manual(values = lag_cols, name = "max lag") +
    scale_x_continuous(breaks = lead_breaks) +
    labs(
      x = "Nowcast lead (weeks after last observed case)",
      y = ylab,
      title = paste0(
        geo, ": nowcast skill vs lead (", ylab, "; reporting lag L=",
        reporting_lag, ")"
      )
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "grey92", colour = NA),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p
}

p_r2 <- plot_lead_decay(metrics_by_lead_plot, "R2", expression(R^2), ylim = c(NA, 1))
p_mape <- plot_lead_decay(metrics_by_lead_plot, "MAPE", "MAPE (%)")

ggsave(file.path(out_dir, "lead_decay_R2.png"), p_r2, width = 10, height = 8, dpi = 150)
ggsave(file.path(out_dir, "lead_decay_MAPE.png"), p_mape, width = 10, height = 8, dpi = 150)

series_cols <- c(
  "AR" = "#333333",
  "ARX (raw)" = "#e41a1c",
  "ARX (processed)" = "#2ca02c"
)

# Use largest max_lag for origin-through-time panels (mirrors deployment lag=3 choice)
lag_keep <- max(max_lags)

origin_plot_data <- function(df, lag_for_plot) {
  ar <- df %>%
    filter(model == "ar", processing == "raw", max_lag == lag_for_plot) %>%
    transmute(
      origin_week, prediction_lead, actual, predicted, APE,
      series = "AR"
    )

  ar_grid <- tidyr::expand_grid(
    predictor_lab = term_levels,
    ar
  )

  arx <- df %>%
    filter(model == "arx", max_lag == lag_for_plot) %>%
    transmute(
      origin_week, prediction_lead, predictor_lab, actual, predicted, APE,
      series = paste0("ARX (", processing, ")")
    )

  bind_rows(ar_grid, arx) %>%
    mutate(
      predictor_lab = factor(predictor_lab, levels = term_levels),
      prediction_lead = factor(
        prediction_lead,
        levels = lead_breaks,
        labels = paste("Lead", lead_breaks)
      ),
      series = factor(
        series,
        levels = c("AR", "ARX (raw)", "ARX (processed)")
      )
    )
}

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

origin_data <- origin_plot_data(forecasts_plot, lag_for_plot = lag_keep)

set.seed(20260721)
rolling_r2_data <- origin_data %>%
  group_by(predictor_lab, prediction_lead, series) %>%
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
  geom_ribbon(
    aes(ymin = R2_low, ymax = R2_high),
    alpha = 0.10,
    colour = NA
  ) +
  geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.3) +
  geom_line(
    data = rolling_r2_data %>% filter(series != "AR"),
    linewidth = 0.55
  ) +
  geom_line(
    data = rolling_r2_data %>% filter(series == "AR"),
    linewidth = 0.65,
    alpha = 0.5
  ) +
  facet_wrap(
    vars(predictor_lab, prediction_lead),
    ncol = horizon,
    scales = "free_y"
  ) +
  scale_colour_manual(values = series_cols, name = NULL) +
  scale_fill_manual(values = series_cols, name = NULL) +
  labs(
    x = "Origin week (last observed case week)",
    y = expression(R^2),
    title = paste0(geo, ": rolling nowcast performance by lead"),
    subtitle = paste0(
      "8-origin rolling predictive R² with bootstrap 95% CIs ",
      "(max lag = ", lag_keep, "; reporting lag L = ", reporting_lag, ")"
    )
  ) +
  common_origin_theme

p_origin_mape <- ggplot(
  origin_data,
  aes(origin_week, APE, colour = series, group = series)
) +
  geom_line(
    data = origin_data %>% filter(series != "AR"),
    linewidth = 0.5,
    alpha = 0.85
  ) +
  geom_line(
    data = origin_data %>% filter(series == "AR"),
    linewidth = 0.65,
    alpha = 0.5
  ) +
  facet_wrap(
    vars(predictor_lab, prediction_lead),
    ncol = horizon,
    scales = "free_y"
  ) +
  scale_colour_manual(values = series_cols, name = NULL) +
  labs(
    x = "Origin week (last observed case week)",
    y = "Absolute percentage error (%)",
    title = paste0(geo, ": pointwise nowcast error by lead"),
    subtitle = paste0(
      "Pointwise APE at each lead (max lag = ", lag_keep,
      "; reporting lag L = ", reporting_lag, "); lower is better"
    )
  ) +
  common_origin_theme

ggsave(
  file.path(out_dir, "origin_performance_R2.png"),
  p_origin_r2,
  width = 14,
  height = 9,
  dpi = 150
)
ggsave(
  file.path(out_dir, "origin_performance_MAPE.png"),
  p_origin_mape,
  width = 14,
  height = 9,
  dpi = 150
)

# ---------------------------------------------------------------------------
# Nowcast vs observed time series by prediction lead
# ---------------------------------------------------------------------------
# One figure per AR / ARX order p (max_lag). Within each figure:
#   rows  = search term (AR baseline repeated in each row)
#   cols  = prediction lead h = 1..L
#   lines = Observed | AR(p) | ARX(p) raw | ARX(p) processed

#' Long data for nowcast-vs-observed lines at a fixed AR order p.
nowcast_vs_observed_data <- function(df, lag_for_plot) {
  ar <- df %>%
    filter(model == "ar", processing == "raw", max_lag == lag_for_plot) %>%
    transmute(
      prediction_week, prediction_lead, actual, predicted,
      series = "AR"
    )

  ar_grid <- tidyr::expand_grid(
    predictor_lab = term_levels,
    ar
  )

  arx <- df %>%
    filter(model == "arx", max_lag == lag_for_plot) %>%
    transmute(
      prediction_week, prediction_lead, predictor_lab, actual, predicted,
      series = paste0("ARX (", processing, ")")
    )

  pred_long <- bind_rows(ar_grid, arx) %>%
    transmute(
      prediction_week,
      prediction_lead,
      predictor_lab,
      series,
      value = predicted
    )

  obs_long <- bind_rows(ar_grid, arx) %>%
    distinct(prediction_week, prediction_lead, predictor_lab, actual) %>%
    transmute(
      prediction_week,
      prediction_lead,
      predictor_lab,
      series = "Observed",
      value = actual
    )

  bind_rows(obs_long, pred_long) %>%
    mutate(
      predictor_lab = factor(predictor_lab, levels = term_levels),
      prediction_lead = factor(
        prediction_lead,
        levels = lead_breaks,
        labels = paste("Lead", lead_breaks)
      ),
      series = factor(
        series,
        levels = c("Observed", "AR", "ARX (raw)", "ARX (processed)")
      )
    )
}

#' Plot observed cases vs nowcasts: rows = search term, cols = prediction lead.
#'
#' @param df forecasts_long-style tibble.
#' @param lag_for_plot Integer AR / ARX order p to hold fixed.
#' @return ggplot object.
plot_nowcast_vs_observed <- function(df, lag_for_plot) {
  d <- nowcast_vs_observed_data(df, lag_for_plot = lag_for_plot)
  line_cols <- c(
    "Observed" = "#000000",
    "AR" = "blue",
    "ARX (raw)" = "#e41a1c",
    "ARX (processed)" = "#2ca02c"
  )
  line_lty <- c(
    "Observed" = "solid",
    "AR" = "solid",
    "ARX (raw)" = "solid",
    "ARX (processed)" = "solid"
  )
  line_w <- c(
    "Observed" = 0.65,
    "AR" = 0.4,
    "ARX (raw)" = 0.4,
    "ARX (processed)" = 0.4
  )

  ggplot(d, aes(
    prediction_week, value,
    colour = series, linetype = series, linewidth = series,
    group = series
  )) +
    geom_line(alpha = 0.55) +
    facet_grid(predictor_lab ~ prediction_lead, scales = "free_y") +
    scale_colour_manual(values = line_cols, name = NULL) +
    scale_linetype_manual(values = line_lty, name = NULL) +
    scale_linewidth_manual(values = line_w, name = NULL) +
    labs(
      x = "Prediction week (week being nowcast)",
      y = "Dengue cases",
      title = paste0(
        geo, ": nowcast vs observed by prediction lead (AR/ARX order p = ",
        lag_for_plot, ")"
      ),
      subtitle = paste0(
        "Rows = search term; columns = prediction lead h. ",
        "Black = observed; coloured = AR(p) / ARX(p) nowcasts. ",
        "Reporting lag L = ", reporting_lag, "."
      )
    ) +
    common_origin_theme
}

for (p_order in max_lags) {
  p_nowcast_vs_obs <- plot_nowcast_vs_observed(
    forecasts_plot,
    lag_for_plot = p_order
  )
  out_png <- file.path(
    out_dir,
    paste0("nowcast_vs_observed_by_lead_p", p_order, ".png")
  )
  ggsave(out_png, p_nowcast_vs_obs, width = 14, height = 9, dpi = 150)
  message("Wrote ", out_png)
}

message("Wrote lead_decay_R2.png / lead_decay_MAPE.png")
message("Wrote origin_performance_R2.png / origin_performance_MAPE.png")
message("Wrote rolling_R2_by_origin_and_lead.csv")
message("Done.")
