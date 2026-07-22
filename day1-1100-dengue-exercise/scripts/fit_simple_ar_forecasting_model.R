# Autoregressive weekly forecasts of dengue cases (train 2015-2017, test 2018-2019),
# one-step-ahead on the test set.
#
# Models (via stats::arima):
#   AR:  cases ~ lagged cases (order = max_lag weeks)
#   ARX: cases ~ lagged cases + lagged search (same 1:max_lag on search)
#
# Usage:
#   Rscript day1-1100-dengue-exercise/scripts/fit_simple_ar_forecasting_model.R MX
#   Rscript day1-1100-dengue-exercise/scripts/fit_simple_ar_forecasting_model.R BR

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(stringr)
  library(lubridate)
  library(patchwork)
  library(urca)
})

source("day1-1100-dengue-exercise/scripts/functions/load_most_recent_file.R")
source("day1-1100-dengue-exercise/scripts/functions/geo_weekly_helpers.R")
source("day1-1530-ai-agents-data-scraping/scripts/functions/signal_preprocess.R")

geo <- parse_geo_arg("MX")
max_lags <- 1:3

out_dir <- file.path(
  "day1-1100-dengue-exercise/outputs/fit_simple_ar_forecasting_model",
  geo
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

r2_score <- function(y, yhat) {
  ok <- is.finite(y) & is.finite(yhat)
  y <- y[ok]
  yhat <- yhat[ok]
  if (length(y) < 2L) return(NA_real_)
  1 - sum((y - yhat)^2) / sum((y - mean(y))^2)
}

# Mean absolute percentage error (%). Drops non-finite or zero actuals.
mape <- function(y, yhat) {
  ok <- is.finite(y) & is.finite(yhat) & (y != 0)
  if (!any(ok)) return(NA_real_)
  mean(abs((y[ok] - yhat[ok]) / y[ok])) * 100
}

#' Lag matrix for a numeric vector (columns lag1..lagp), aligned to x[(p+1):n].
lag_matrix <- function(x, p) {
  x <- as.numeric(x)
  n <- length(x)
  if (n <= p) stop("Series shorter than max_lag=", p)
  mat <- sapply(seq_len(p), function(k) x[(p + 1L - k):(n - k)])
  colnames(mat) <- paste0("lag", seq_len(p))
  mat
}

#' Mean / intercept from an arima() fit (R reports the series mean as "intercept").
arima_mean <- function(fit) {
  cf <- coef(fit)
  if ("intercept" %in% names(cf)) unname(cf[["intercept"]]) else 0
}

arima_phi <- function(fit, p) {
  cf <- coef(fit)
  nm <- paste0("ar", seq_len(p))
  if (!all(nm %in% names(cf))) {
    # fall back to any ^ar coefficients
    nm <- grep("^ar[0-9]+$", names(cf), value = TRUE)
  }
  unname(cf[nm])
}

#' One-step AR prediction at time t using observed history y[1:(t-1)].
one_step_ar <- function(y, t, phi, mu) {
  p <- length(phi)
  if (t <= p) return(NA_real_)
  mu + sum(phi * (y[t - seq_len(p)] - mu))
}

#' One-step ARX prediction: AR on y plus xreg = lagged x at time t.
one_step_arx <- function(y, x, t, phi, mu, beta) {
  p <- length(phi)
  if (t <= 2L * p) return(NA_real_)

  # stats::arima(..., xreg=...) applies AR dynamics to residuals:
  #   y_t = m_t + sum_i phi_i * (y_{t-i} - m_{t-i}) + error_t
  # where m_t = mu + beta' xreg_t.
  regression_mean <- function(time_index) {
    mu + sum(beta * x[time_index - seq_len(p)])
  }

  mean_now <- regression_mean(t)
  past_times <- t - seq_len(p)
  mean_past <- vapply(past_times, regression_mean, numeric(1))

  mean_now + sum(phi * (y[past_times] - mean_past))
}

# ---------------------------------------------------------------------------
# Core fitting function (parameterised by max_lag)
# ---------------------------------------------------------------------------

#' Fit AR or ARX (arima) for a given maximum lag; one-step-ahead on test.
#'
#' @param max_lag Integer AR / search-lag order.
#' @param y_train,y_test Numeric case series for train / test.
#' @param x_train,x_test Optional search series (same length as y_*). If NULL, AR only.
#' @return list with model, metrics, train/test tibbles of actual vs fitted/predicted.
fit_simple_ar <- function(max_lag,
                          y_train,
                          y_test,
                          x_train = NULL,
                          x_test = NULL) {
  max_lag <- as.integer(max_lag)
  stopifnot(max_lag >= 1L)
  y_train <- as.numeric(y_train)
  y_test <- as.numeric(y_test)
  use_x <- !is.null(x_train)

  if (use_x) {
    x_train <- as.numeric(x_train)
    x_test <- as.numeric(x_test)
    stopifnot(length(x_train) == length(y_train), length(x_test) == length(y_test))
  }

  # Align training so lag-p rows are available
  if (use_x) {
    xreg_tr <- lag_matrix(x_train, max_lag)
    y_tr <- y_train[(max_lag + 1L):length(y_train)]
    fit <- arima(
      y_tr,
      order = c(max_lag, 0L, 0L),
      xreg = xreg_tr,
      include.mean = TRUE,
      method = "ML"
    )
    beta_nm <- colnames(xreg_tr)
    beta <- unname(coef(fit)[beta_nm])
  } else {
    fit <- arima(
      y_train,
      order = c(max_lag, 0L, 0L),
      include.mean = TRUE,
      method = "ML"
    )
    beta <- NULL
  }

  phi <- arima_phi(fit, max_lag)
  mu <- arima_mean(fit)

  # --- Train fitted (one-step in-sample on train, observed lags) ---
  if (use_x) {
    train_idx <- (2L * max_lag + 1L):length(y_train)
    train_hat <- vapply(
      train_idx,
      function(t) one_step_arx(y_train, x_train, t, phi, mu, beta),
      numeric(1)
    )
    train_actual <- y_train[train_idx]
  } else {
    train_idx <- (max_lag + 1L):length(y_train)
    train_hat <- vapply(
      train_idx,
      function(t) one_step_ar(y_train, t, phi, mu),
      numeric(1)
    )
    train_actual <- y_train[train_idx]
  }

  # --- Test one-step-ahead using observed history (no recursive feedback) ---
  y_all <- c(y_train, y_test)
  n_tr <- length(y_train)
  test_times <- n_tr + seq_along(y_test)

  if (use_x) {
    x_all <- c(x_train, x_test)
    test_hat <- vapply(
      test_times,
      function(t) one_step_arx(y_all, x_all, t, phi, mu, beta),
      numeric(1)
    )
  } else {
    test_hat <- vapply(
      test_times,
      function(t) one_step_ar(y_all, t, phi, mu),
      numeric(1)
    )
  }
  test_actual <- y_test

  metrics <- bind_rows(
    tibble(
      set = "train",
      intercept = mu,
      correlation = cor(train_actual, train_hat, use = "complete.obs"),
      R2 = r2_score(train_actual, train_hat),
      MAPE = mape(train_actual, train_hat)
    ),
    tibble(
      set = "test",
      intercept = NA_real_,
      correlation = cor(test_actual, test_hat, use = "complete.obs"),
      R2 = r2_score(test_actual, test_hat),
      MAPE = mape(test_actual, test_hat)
    )
  )

  list(
    fit = fit,
    max_lag = max_lag,
    model = if (use_x) "arx" else "ar",
    phi = phi,
    mu = mu,
    beta = beta,
    metrics = metrics,
    train = tibble(actual = train_actual, predicted = train_hat),
    test = tibble(actual = test_actual, predicted = test_hat)
  )
}

pred_actual_scatter <- function(df, title, subtitle) {
  ggplot(df, aes(x = predicted, y = actual)) +
    geom_point(alpha = 0.75, colour = "#2c7fb8") +
    geom_abline(intercept = 0, slope = 1, colour = "#e34a33", linewidth = 0.8) +
    labs(title = title, subtitle = subtitle, x = "Predicted", y = "Actual") +
    theme_minimal()
}

panel_subtitle <- function(metrics_row) {
  sprintf(
    "cor=%.3f; R²=%.3f; MAPE=%.1f%%",
    metrics_row$correlation,
    metrics_row$R2,
    metrics_row$MAPE
  )
}

# ---------------------------------------------------------------------------
# Data: weekly cases + raw / processed search
# ---------------------------------------------------------------------------

dat <- load_weekly_dengue_geo(geo)
raw_search <- dat$dengue_df %>%
  transmute(
    start_date,
    y = dengue_total,
    dengue, mosquito, sintomas_de_dengue
  )

n_train <- sum(raw_search$start_date <= train_end)
message(
  "Geo: ", geo, " | Series: ", min(raw_search$start_date), " to ",
  max(raw_search$start_date),
  " (", nrow(raw_search), " weeks; train n=", n_train, ")"
)

preprocess_search_columns <- function(df, predictors, train_n) {
  out <- dplyr::tibble(start_date = df$start_date)
  for (col in predictors) {
    one <- df %>%
      select(start_date, all_of(col)) %>%
      rename(date = start_date)
    den <- denoise_frame(one, window = 20L, spar = 0.8, max_knots = 3L)
    det <- detrend_frame(den, train_end = train_n)
    out[[col]] <- det$data[[col]]
  }
  out
}

processed_search <- preprocess_search_columns(raw_search, predictors, n_train)

make_xy <- function(search_df, predictor) {
  dat <- raw_search %>%
    select(start_date, y) %>%
    left_join(
      search_df %>% select(start_date, all_of(predictor)),
      by = "start_date"
    ) %>%
    arrange(start_date)
  list(
    train = dat %>% filter(start_date <= train_end),
    test = dat %>% filter(start_date >= test_start, start_date <= test_end)
  )
}

# ---------------------------------------------------------------------------
# Run all combinations and build figures
# ---------------------------------------------------------------------------

all_metrics <- list()
metric_i <- 0L

for (processing in c("raw", "processed")) {
  search_df <- if (identical(processing, "raw")) {
    raw_search
  } else {
    processed_search %>%
      left_join(raw_search %>% select(start_date, y), by = "start_date")
  }

  for (predictor in predictors) {
    message("=== ", processing, " | ", predictor, " ===")
    xy <- make_xy(
      if (identical(processing, "raw")) raw_search else processed_search,
      predictor
    )
    y_tr <- xy$train$y
    y_te <- xy$test$y
    x_tr <- xy$train[[predictor]]
    x_te <- xy$test[[predictor]]

    ar_by_lag <- lapply(max_lags, function(L) {
      fit_simple_ar(L, y_tr, y_te, x_train = NULL, x_test = NULL)
    })
    arx_by_lag <- lapply(max_lags, function(L) {
      fit_simple_ar(L, y_tr, y_te, x_train = x_tr, x_test = x_te)
    })

    for (j in seq_along(max_lags)) {
      L <- max_lags[[j]]
      for (res in list(ar_by_lag[[j]], arx_by_lag[[j]])) {
        metric_i <- metric_i + 1L
        all_metrics[[metric_i]] <- res$metrics %>%
          mutate(
            geo = geo,
            predictor = predictor,
            processing = processing,
            model = res$model,
            max_lag = L,
            .before = 1L
          )
      }
    }

    # ---- Figure A: AR | ARX side by side (test pred vs actual), rows = lag ----
    panels_a <- list()
    for (j in seq_along(max_lags)) {
      L <- max_lags[[j]]
      ar_m <- ar_by_lag[[j]]$metrics %>% filter(set == "test")
      arx_m <- arx_by_lag[[j]]$metrics %>% filter(set == "test")
      panels_a[[length(panels_a) + 1L]] <- pred_actual_scatter(
        ar_by_lag[[j]]$test,
        title = paste0("AR(", L, ") — test"),
        subtitle = panel_subtitle(ar_m)
      )
      panels_a[[length(panels_a) + 1L]] <- pred_actual_scatter(
        arx_by_lag[[j]]$test,
        title = paste0("ARX(", L, ") + ", predictor, " — test"),
        subtitle = panel_subtitle(arx_m)
      )
    }
    fig_a <- wrap_plots(panels_a, ncol = 2) +
      plot_annotation(
        title = paste0(
          geo, " one-step test: AR vs ARX | predictor=", predictor,
          " | search=", processing
        ),
        subtitle = "Rows = max_lag 1:3 weeks; train 2015–2017, test 2018–2019"
      )
    ggsave(
      file.path(
        out_dir,
        paste0(predictor, "_", processing, "_ar_vs_arx_test.png")
      ),
      fig_a,
      width = 11,
      height = 12,
      dpi = 150
    )

    # ---- Figure B1: AR train | test, rows = lag ----
    panels_b_ar <- list()
    for (j in seq_along(max_lags)) {
      L <- max_lags[[j]]
      tr_m <- ar_by_lag[[j]]$metrics %>% filter(set == "train")
      te_m <- ar_by_lag[[j]]$metrics %>% filter(set == "test")
      panels_b_ar[[length(panels_b_ar) + 1L]] <- pred_actual_scatter(
        ar_by_lag[[j]]$train,
        title = paste0("AR(", L, ") — train"),
        subtitle = panel_subtitle(tr_m)
      )
      panels_b_ar[[length(panels_b_ar) + 1L]] <- pred_actual_scatter(
        ar_by_lag[[j]]$test,
        title = paste0("AR(", L, ") — test"),
        subtitle = panel_subtitle(te_m)
      )
    }
    fig_b_ar <- wrap_plots(panels_b_ar, ncol = 2) +
      plot_annotation(
        title = paste0("AR train | test | search=", processing, " (", predictor, " figure set)"),
        subtitle = "Rows = max_lag 1:3; one-step-ahead with observed lags"
      )
    ggsave(
      file.path(out_dir, paste0(predictor, "_", processing, "_ar_train_test.png")),
      fig_b_ar,
      width = 11,
      height = 12,
      dpi = 150
    )

    # ---- Figure B2: ARX train | test, rows = lag ----
    panels_b_arx <- list()
    for (j in seq_along(max_lags)) {
      L <- max_lags[[j]]
      tr_m <- arx_by_lag[[j]]$metrics %>% filter(set == "train")
      te_m <- arx_by_lag[[j]]$metrics %>% filter(set == "test")
      panels_b_arx[[length(panels_b_arx) + 1L]] <- pred_actual_scatter(
        arx_by_lag[[j]]$train,
        title = paste0("ARX(", L, ") + ", predictor, " — train"),
        subtitle = panel_subtitle(tr_m)
      )
      panels_b_arx[[length(panels_b_arx) + 1L]] <- pred_actual_scatter(
        arx_by_lag[[j]]$test,
        title = paste0("ARX(", L, ") + ", predictor, " — test"),
        subtitle = panel_subtitle(te_m)
      )
    }
    fig_b_arx <- wrap_plots(panels_b_arx, ncol = 2) +
      plot_annotation(
        title = paste0(
          "ARX train | test | predictor=", predictor, " | search=", processing
        ),
        subtitle = "Rows = max_lag 1:3; lagged cases + lagged search; one-step-ahead"
      )
    ggsave(
      file.path(out_dir, paste0(predictor, "_", processing, "_arx_train_test.png")),
      fig_b_arx,
      width = 11,
      height = 12,
      dpi = 150
    )
  }
}

metrics_long <- bind_rows(all_metrics) %>%
  select(geo, predictor, processing, model, max_lag, set, intercept, correlation, R2, MAPE)

metrics_file <- file.path(out_dir, "metrics_long.csv")
write_csv(metrics_long, metrics_file)
message("Wrote ", metrics_file)

# ---------------------------------------------------------------------------
# Heatmaps: test R2 and MAPE by preprocessing, model/predictor, and max lag
# ---------------------------------------------------------------------------

# AR-only rows are duplicated once per predictor in the long model loop.
# Keep a single baseline per processing/lag, then append every ARX predictor.
heatmap_metrics <- bind_rows(
  metrics_long %>%
    filter(set == "test", model == "ar") %>%
    distinct(geo, processing, max_lag, .keep_all = TRUE) %>%
    mutate(model_predictor = "AR only"),
  metrics_long %>%
    filter(set == "test", model == "arx") %>%
    mutate(model_predictor = paste0("ARX + ", predictor))
) %>%
  mutate(
    model_predictor = factor(
      model_predictor,
      levels = c(
        "AR only",
        "ARX + dengue",
        "ARX + mosquito",
        "ARX + sintomas_de_dengue"
      )
    ),
    processing = factor(processing, levels = c("raw", "processed")),
    lag_label = paste0("lag ", max_lag)
  )

heatmap_long <- heatmap_metrics %>%
  select(
    geo, processing, model_predictor, max_lag, lag_label,
    R2, MAPE
  ) %>%
  pivot_longer(
    cols = c(R2, MAPE),
    names_to = "metric",
    values_to = "value"
  )

write_csv(
  heatmap_long,
  file.path(out_dir, "heatmap_metrics_long.csv")
)

make_metric_heatmap <- function(metric_name) {
  plot_df <- heatmap_long %>% filter(metric == metric_name)
  label_format <- if (identical(metric_name, "R2")) "%.3f" else "%.1f"

  ggplot(
    plot_df,
    aes(x = model_predictor, y = lag_label, fill = value)
  ) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf(label_format, value)), size = 3.3) +
    facet_wrap(~processing, nrow = 1) +
    scale_fill_viridis_c(option = "C", direction = -1) +
    labs(
      title = paste0(geo, " test ", metric_name, ": AR only vs lagged Google Trends"),
      subtitle = "Weekly one-step forecasts; train 2015–2017, test 2018–2019",
      x = "Model / search predictor",
      y = "Maximum lag (weeks)",
      fill = metric_name
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      panel.grid = element_blank()
    )
}

ggsave(
  file.path(out_dir, "heatmap_test_R2.png"),
  make_metric_heatmap("R2"),
  width = 11,
  height = 5.5,
  dpi = 150
)
ggsave(
  file.path(out_dir, "heatmap_test_MAPE.png"),
  make_metric_heatmap("MAPE"),
  width = 11,
  height = 5.5,
  dpi = 150
)

message("Wrote figures under ", out_dir)
message("Done.")
