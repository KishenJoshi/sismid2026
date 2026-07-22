# Performance checks for the pre-processing steps.

pct_zeros <- function(df) {
  cols <- setdiff(names(df), "date")
  if (!length(cols)) {
    return(numeric())
  }
  vapply(
    cols,
    function(col) {
      x <- df[[col]]
      x <- x[is.finite(x)]
      if (!length(x)) return(NA_real_)
      100 * mean(x == 0)
    },
    numeric(1)
  )
}

# Signal-to-noise ratio: var(smoothed) / var(raw - smoothed).
snr <- function(raw, smoothed) {
  raw <- as.numeric(raw)
  smoothed <- as.numeric(smoothed)
  ok <- is.finite(raw) & is.finite(smoothed)
  raw <- raw[ok]
  smoothed <- smoothed[ok]
  resid <- raw - smoothed
  v_s <- stats::var(smoothed)
  v_n <- stats::var(resid)
  if (!is.finite(v_s) || !is.finite(v_n) || v_n <= 0) {
    return(c(snr = NA_real_, snr_db = NA_real_))
  }
  ratio <- v_s / v_n
  c(snr = ratio, snr_db = 10 * log10(ratio))
}

r2_score <- function(y, yhat) {
  y <- as.numeric(y)
  yhat <- as.numeric(yhat)
  ok <- is.finite(y) & is.finite(yhat)
  y <- y[ok]
  yhat <- yhat[ok]
  if (length(y) < 2L) return(NA_real_)
  ss_res <- sum((y - yhat)^2)
  ss_tot <- sum((y - mean(y))^2)
  if (ss_tot <= 0) return(NA_real_)
  1 - ss_res / ss_tot
}

# Assess aggregation: % zeros before vs after.
assess_aggregation <- function(raw_df, aggregated_df) {
  before <- pct_zeros(raw_df)
  after <- pct_zeros(aggregated_df)
  list(
    pct_zeros_before = before,
    pct_zeros_after = after,
    mean_pct_zeros_before = mean(before, na.rm = TRUE),
    mean_pct_zeros_after = mean(after, na.rm = TRUE)
  )
}

# SNR before = trailing rolling-mean baseline; after = penalised spline.
assess_denoising_vs_raw <- function(raw_df, denoised_df, baseline_window = 20L) {
  cols <- intersect(
    setdiff(names(raw_df), "date"),
    setdiff(names(denoised_df), "date")
  )
  rows <- lapply(cols, function(col) {
    x <- as.numeric(raw_df[[col]])
    xs <- as.numeric(denoised_df[[col]])
    baseline <- as.numeric(
      zoo::rollapply(
        x,
        width = baseline_window,
        FUN = function(z) mean(z, na.rm = TRUE),
        align = "right",
        fill = NA_real_,
        partial = TRUE
      )
    )
    pre <- snr(x, baseline)
    post <- snr(x, xs)
    dplyr::tibble(
      series = col,
      snr_pre = unname(pre["snr"]),
      snr_pre_db = unname(pre["snr_db"]),
      snr_post = unname(post["snr"]),
      snr_post_db = unname(post["snr_db"])
    )
  })
  dplyr::bind_rows(rows)
}

# R^2 of the estimated trend on the series before detrending, and again on the
# detrended series (should fall toward 0 if removal worked).
assess_detrending <- function(before_df, trend_meta) {
  cols <- intersect(setdiff(names(before_df), "date"), names(trend_meta))
  rows <- lapply(cols, function(col) {
    meta <- trend_meta[[col]]
    x <- as.numeric(before_df[[col]])
    if (identical(meta$kind, "stochastic_diff")) {
      t <- seq_along(x)
      train <- meta$train_idx
      m_before <- stats::lm(x[train] ~ t[train])
      r2_before <- summary(m_before)$r.squared
      y <- meta$series
      ok <- which(is.finite(y))
      r2_after <- if (length(ok) >= 5L) {
        summary(stats::lm(y[ok] ~ seq_along(ok)))$r.squared
      } else {
        NA_real_
      }
    } else {
      # R^2 of the train-set trend fit (params estimated on train only).
      r2_before <- if (!is.null(meta$trend_fit)) {
        summary(meta$trend_fit)$r.squared
      } else {
        r2_score(x[meta$train_idx], meta$trend_hat[meta$train_idx])
      }
      y <- meta$series
      train <- meta$train_idx
      t <- seq_along(y)
      m_after <- .fit_det_trend(y[train], t[train], degree = meta$degree)
      # After detrending, R^2 of the same-degree trend on the train residuals.
      r2_after <- summary(m_after)$r.squared
    }
    dplyr::tibble(
      series = col,
      trend_kind = meta$kind,
      r2_trend_before = r2_before,
      r2_trend_after = r2_after
    )
  })
  dplyr::bind_rows(rows)
}

assess_preprocessing <- function(pipeline) {
  list(
    aggregation = assess_aggregation(pipeline$raw, pipeline$aggregated),
    denoising = assess_denoising_vs_raw(pipeline$aggregated, pipeline$denoised),
    detrending = assess_detrending(pipeline$denoised, pipeline$trend_meta)
  )
}

print_assessment <- function(assessment) {
  message("--- Aggregation (% zeros) ---")
  message(
    sprintf(
      "  mean %% zeros before: %.1f | after: %.1f",
      assessment$aggregation$mean_pct_zeros_before,
      assessment$aggregation$mean_pct_zeros_after
    )
  )
  before <- assessment$aggregation$pct_zeros_before
  after <- assessment$aggregation$pct_zeros_after
  message(
    "  before: ",
    paste(sprintf("%s=%.1f", names(before), before), collapse = "; ")
  )
  message(
    "  after:  ",
    paste(sprintf("%s=%.1f", names(after), after), collapse = "; ")
  )

  message("--- Denoising (SNR; pre = rolling mean, post = spline) ---")
  message(paste(utils::capture.output(print(
    as.data.frame(assessment$denoising),
    row.names = FALSE
  )), collapse = "\n"))

  message("--- Detrending (R^2 of trend before vs after) ---")
  message(paste(utils::capture.output(print(
    as.data.frame(assessment$detrending),
    row.names = FALSE
  )), collapse = "\n"))
}
