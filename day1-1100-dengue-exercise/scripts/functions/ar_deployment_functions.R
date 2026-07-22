# Shared helpers for expanding-window AR / ARX dengue deployment simulation.
#
# Source from repo root, e.g.:
#   source("day1-1100-dengue-exercise/scripts/functions/ar_deployment_functions.R")
#
# Depends on dplyr and (for preprocess_search_columns) signal_preprocess.R
# helpers denoise_frame() / detrend_frame().

# ---------------------------------------------------------------------------
# Metrics
# ---------------------------------------------------------------------------

#' Predictive R² (may be negative out of sample).
r2_score <- function(y, yhat) {
  ok <- is.finite(y) & is.finite(yhat)
  y <- y[ok]
  yhat <- yhat[ok]
  if (length(y) < 2L) return(NA_real_)
  denom <- sum((y - mean(y))^2)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  1 - sum((y - yhat)^2) / denom
}

#' Mean absolute percentage error (%). Drops non-finite or zero actuals.
mape <- function(y, yhat) {
  ok <- is.finite(y) & is.finite(yhat) & (y != 0)
  if (!any(ok)) return(NA_real_)
  mean(abs((y[ok] - yhat[ok]) / y[ok])) * 100
}

#' Pointwise absolute percentage error (%).
ape <- function(y, yhat) {
  ifelse(
    is.finite(y) & is.finite(yhat) & y != 0,
    abs(y - yhat) / abs(y) * 100,
    NA_real_
  )
}

# ---------------------------------------------------------------------------
# AR / ARX fitting utilities
# ---------------------------------------------------------------------------

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
    nm <- grep("^ar[0-9]+$", names(cf), value = TRUE)
  }
  unname(cf[nm])
}

fit_ar <- function(y_train, max_lag) {
  y_train <- as.numeric(y_train)
  fit <- arima(
    y_train,
    order = c(max_lag, 0L, 0L),
    include.mean = TRUE,
    method = "CSS"
  )
  list(
    phi = arima_phi(fit, max_lag),
    mu = arima_mean(fit),
    beta = NULL,
    p = max_lag
  )
}

fit_arx <- function(y_train, x_train, max_lag) {
  y_train <- as.numeric(y_train)
  x_train <- as.numeric(x_train)
  xreg_tr <- lag_matrix(x_train, max_lag)
  y_tr <- y_train[(max_lag + 1L):length(y_train)]
  fit <- arima(
    y_tr,
    order = c(max_lag, 0L, 0L),
    xreg = xreg_tr,
    include.mean = TRUE,
    method = "CSS"
  )
  beta <- unname(coef(fit)[colnames(xreg_tr)])
  list(
    phi = arima_phi(fit, max_lag),
    mu = arima_mean(fit),
    beta = beta,
    p = max_lag
  )
}

#' One recursive multi-step forecast from the end of y_hist.
#'
#' Cases: recursive (predicted values appended to the case history).
#' Search (ARX): observed x_all is indexed at absolute times n+1, n+2, ...
#' so Trends are treated as available in real time.
recursive_forecast <- function(y_hist, x_all = NULL, fit, horizon = 8L) {
  p <- fit$p
  phi <- fit$phi
  mu <- fit$mu
  beta <- fit$beta
  use_x <- !is.null(beta)

  y_work <- as.numeric(y_hist)
  n0 <- length(y_work)
  preds <- rep(NA_real_, horizon)

  for (h in seq_len(horizon)) {
    t <- n0 + h
    if (use_x) {
      if (t <= 2L * p) next
      regression_mean <- function(time_index) {
        mu + sum(beta * x_all[time_index - seq_len(p)])
      }
      mean_now <- regression_mean(t)
      past_times <- t - seq_len(p)
      # Mix of observed (time <= n0) and previously predicted cases
      y_past <- vapply(past_times, function(tt) {
        if (tt <= n0) y_hist[tt] else preds[tt - n0]
      }, numeric(1))
      mean_past <- vapply(past_times, regression_mean, numeric(1))
      preds[h] <- mean_now + sum(phi * (y_past - mean_past))
    } else {
      if (t <= p) next
      y_past <- vapply(t - seq_len(p), function(tt) {
        if (tt <= n0) y_hist[tt] else preds[tt - n0]
      }, numeric(1))
      preds[h] <- mu + sum(phi * (y_past - mu))
    }
  }
  preds
}

#' Denoise + detrend each search column, freezing trend params on train_n weeks.
#' Expects a start_date column (week start).
preprocess_search_columns <- function(df, predictors, train_n) {
  out <- dplyr::tibble(start_date = df$start_date)
  for (col in predictors) {
    one <- df %>%
      dplyr::select(start_date, dplyr::all_of(col)) %>%
      dplyr::rename(date = start_date)
    den <- denoise_frame(one, window = 20L, spar = 0.8, max_knots = 3L)
    det <- detrend_frame(den, train_end = train_n)
    out[[col]] <- det$data[[col]]
  }
  out
}
