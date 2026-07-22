# Signal pre-processing for Google Trends series.

value_cols <- function(df) {
  setdiff(names(df), "date")
}

# ---------------------------------------------------------------------------
# 1. Hierarchical clustering: aggregate semantically related terms
#    Similarity = Pearson correlation of the interest series (co-movement proxy
#    for shared meaning). Within each cluster, take the row-wise mean so sparse
#    synonyms fill each other's zeros.
# ---------------------------------------------------------------------------

aggregate_related_terms <- function(df,
                                    n_clusters = NULL,
                                    h_cut = NULL,
                                    method = "complete") {
  cols <- value_cols(df)
  if (length(cols) < 2L) {
    return(list(
      data = df,
      cluster_map = stats::setNames(1L, cols),
      n_clusters = 1L
    ))
  }

  mat <- as.matrix(df[, cols, drop = FALSE])
  # Impute isolated NAs with column medians for distance only.
  for (j in seq_len(ncol(mat))) {
    miss <- is.na(mat[, j])
    if (any(miss)) {
      mat[miss, j] <- stats::median(mat[!miss, j], na.rm = TRUE)
    }
  }

  cors <- stats::cor(mat, use = "pairwise.complete.obs")
  cors[!is.finite(cors)] <- 0
  dist_mat <- stats::as.dist(1 - cors)
  hc <- stats::hclust(dist_mat, method = method)

  if (is.null(n_clusters) && is.null(h_cut)) {
    # Default: keep clusters that merge above 0.35 correlation distance,
    # but never more clusters than terms / 1 cluster minimum.
    h_cut <- 0.65
  }
  if (!is.null(n_clusters)) {
    n_clusters <- max(1L, min(as.integer(n_clusters), length(cols)))
    membership <- stats::cutree(hc, k = n_clusters)
  } else {
    membership <- stats::cutree(hc, h = h_cut)
    n_clusters <- length(unique(membership))
  }

  out <- dplyr::tibble(date = df$date)
  for (k in sort(unique(membership))) {
    members <- names(membership)[membership == k]
    label <- if (length(members) == 1L) {
      members[[1]]
    } else {
      paste0("cluster_", k, "__", paste(members, collapse = "+"))
    }
    out[[label]] <- rowMeans(as.matrix(df[, members, drop = FALSE]), na.rm = TRUE)
  }

  list(
    data = out,
    cluster_map = membership,
    n_clusters = n_clusters,
    hclust = hc
  )
}

# ---------------------------------------------------------------------------
# 2. Smooth-spline denoising on a 20-week rolling window
#    - fit smooth.spline inside each window
#    - penalise complexity via spar
#    - cap effective df so the fit has at most 3 knots (df <= 4)
# ---------------------------------------------------------------------------

.fit_window_spline <- function(y, spar = 0.8, max_knots = 3L) {
  n <- sum(is.finite(y))
  if (n < 4L) {
    return(mean(y, na.rm = TRUE))
  }
  x <- seq_along(y)
  ok <- is.finite(y)
  # Penalised spline first; then cap effective df so we keep <= max_knots
  # (df ≈ knots + 1 including the intercept).
  max_df <- min(max_knots + 1L, n - 1L)
  fit <- tryCatch(
    stats::smooth.spline(x = x[ok], y = y[ok], spar = spar),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(mean(y[ok]))
  }
  if (is.finite(fit$df) && fit$df > max_df) {
    fit <- tryCatch(
      stats::smooth.spline(x = x[ok], y = y[ok], df = max_df),
      error = function(e) fit
    )
  }
  # Evaluate at the last point of the window (causal / trailing smoother).
  as.numeric(stats::predict(fit, x = length(y))$y)
}

spline_denoise <- function(x,
                           window = 20L,
                           spar = 0.8,
                           max_knots = 3L) {
  x <- as.numeric(x)
  n <- length(x)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    start <- max(1L, i - window + 1L)
    out[i] <- .fit_window_spline(
      x[start:i],
      spar = spar,
      max_knots = max_knots
    )
  }
  out
}

denoise_frame <- function(df,
                          window = 20L,
                          spar = 0.8,
                          max_knots = 3L) {
  cols <- value_cols(df)
  out <- df
  for (col in cols) {
    out[[col]] <- spline_denoise(
      df[[col]],
      window = window,
      spar = spar,
      max_knots = max_knots
    )
  }
  out
}

# ---------------------------------------------------------------------------
# 3–5. Trend diagnostics + removal (params estimated on the training set only)
#    ADF (urca::ur.df): unit root => stochastic trend => difference
#    Otherwise choose linear vs quadratic deterministic trend by AIC on train,
#    then subtract the fitted trend from the full series.
# ---------------------------------------------------------------------------

adf_unit_root <- function(x, type = "trend", lags = NULL) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 10L) {
    return(list(is_unit_root = TRUE, statistic = NA_real_, critical = NA_real_))
  }
  if (is.null(lags)) {
    lags <- trunc((length(x) - 1)^(1 / 3))
  }
  fit <- urca::ur.df(x, type = type, lags = max(0L, as.integer(lags)))
  # tau3 (trend) or tau2 (drift) test statistic vs 5% critical value
  stat_name <- if (identical(type, "trend")) "tau3" else if (identical(type, "drift")) "tau2" else "tau1"
  stat <- as.numeric(fit@teststat[1, stat_name])
  crit <- as.numeric(fit@cval[stat_name, "5pct"])
  # More negative than critical value => reject unit root
  list(
    is_unit_root = !(is.finite(stat) && is.finite(crit) && stat < crit),
    statistic = stat,
    critical_5pct = crit,
    test = fit
  )
}

.fit_det_trend <- function(y, t, degree = 1L) {
  degree <- as.integer(degree)
  stopifnot(degree %in% c(1L, 2L))
  dat <- data.frame(y = y, t = t)
  if (degree == 1L) {
    stats::lm(y ~ t, data = dat)
  } else {
    stats::lm(y ~ t + I(t^2), data = dat)
  }
}

choose_det_trend <- function(y_train, t_train) {
  ok <- is.finite(y_train) & is.finite(t_train)
  y_train <- y_train[ok]
  t_train <- t_train[ok]
  if (length(y_train) < 5L) {
    return(list(degree = 1L, model = .fit_det_trend(y_train, t_train, 1L)))
  }
  m1 <- .fit_det_trend(y_train, t_train, 1L)
  m2 <- .fit_det_trend(y_train, t_train, 2L)
  if (stats::AIC(m2) < stats::AIC(m1)) {
    list(degree = 2L, model = m2)
  } else {
    list(degree = 1L, model = m1)
  }
}

# Returns a list with the stationarised series and trend metadata.
remove_trend <- function(x,
                         train_ratio = 0.7,
                         train_end = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  t <- seq_len(n)

  if (!is.null(train_end)) {
    train_idx <- which(t <= as.integer(train_end))
  } else {
    train_idx <- seq_len(max(5L, floor(n * train_ratio)))
  }
  test_idx <- setdiff(seq_len(n), train_idx)

  adf <- adf_unit_root(x[train_idx], type = "trend")

  if (isTRUE(adf$is_unit_root)) {
    # Stochastic trend: difference to stationarity (params N/A; operator is Δ).
    y <- c(NA_real_, diff(x))
    return(list(
      series = y,
      kind = "stochastic_diff",
      degree = NA_integer_,
      adf = adf,
      train_idx = train_idx,
      trend_fit = NULL,
      trend_hat = rep(NA_real_, n)
    ))
  }

  choice <- choose_det_trend(x[train_idx], t[train_idx])
  trend_hat <- as.numeric(stats::predict(choice$model, newdata = data.frame(t = t)))
  y <- x - trend_hat

  list(
    series = y,
    kind = if (choice$degree == 1L) "deterministic_linear" else "deterministic_quadratic",
    degree = choice$degree,
    adf = adf,
    train_idx = train_idx,
    trend_fit = choice$model,
    trend_hat = trend_hat
  )
}

detrend_frame <- function(df, train_ratio = 0.7, train_end = NULL) {
  cols <- value_cols(df)
  out <- dplyr::tibble(date = df$date)
  meta <- list()
  for (col in cols) {
    res <- remove_trend(
      df[[col]],
      train_ratio = train_ratio,
      train_end = train_end
    )
    out[[col]] <- res$series
    meta[[col]] <- res
  }
  list(data = out, meta = meta)
}

# Full pipeline: aggregate -> denoise -> detrend
preprocess_trends <- function(df,
                              n_clusters = NULL,
                              denoise_window = 20L,
                              spar = 0.8,
                              max_knots = 3L,
                              train_ratio = 0.7) {
  agg <- aggregate_related_terms(df, n_clusters = n_clusters)
  den <- denoise_frame(
    agg$data,
    window = denoise_window,
    spar = spar,
    max_knots = max_knots
  )
  det <- detrend_frame(den, train_ratio = train_ratio)
  list(
    raw = df,
    aggregated = agg$data,
    cluster_map = agg$cluster_map,
    denoised = den,
    processed = det$data,
    trend_meta = det$meta
  )
}
