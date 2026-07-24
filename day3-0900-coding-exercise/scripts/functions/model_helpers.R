# Model helpers: build lagged panel, INLA fits, CRPS, diagnostics.

source_model_deps <- function(root) {
  source(file.path(root, "scripts/functions/paths.R"), local = FALSE)
  source(file.path(root, "scripts/functions/lag_helpers.R"), local = FALSE)
}

#' Build modelling panel with climate lags and ARGO-style AR(1,12,24).
build_modelling_panel <- function(cases_path,
                                  climate_path,
                                  location_index_path,
                                  trends_path = NULL,
                                  climate_lags = CLIMATE_LAGS,
                                  ar_lags = AR_LAGS) {
  cases <- readr::read_csv(cases_path, show_col_types = FALSE)
  climate <- readr::read_csv(climate_path, show_col_types = FALSE)
  loc <- readr::read_csv(location_index_path, show_col_types = FALSE)

  panel <- cases %>%
    dplyr::inner_join(climate, by = c("rne_iso_code", "year", "month", "month_start")) %>%
    dplyr::left_join(loc, by = "rne_iso_code") %>%
    dplyr::arrange(rne_iso_code, month_start)

  panel <- add_group_lags(panel, "mean_temp_celsius", climate_lags)
  panel <- add_group_lags(panel, "total_precip_mm", climate_lags)
  panel <- add_group_lags(panel, "cases", ar_lags)

  trend_cols <- character(0)
  if (!is.null(trends_path) && file.exists(trends_path)) {
    trends <- readr::read_csv(trends_path, show_col_types = FALSE)
    wide <- trends %>%
      tidyr::pivot_wider(
        id_cols = c(rne_iso_code, month_start),
        names_from = topic_label,
        values_from = hits,
        names_prefix = "gt_"
      )
    panel <- panel %>% dplyr::left_join(wide, by = c("rne_iso_code", "month_start"))
    trend_cols <- grep("^gt_", names(panel), value = TRUE)
    for (tc in trend_cols) {
      panel <- add_group_lags(panel, tc, ar_lags)
    }
  }

  panel$log_pop <- log(pmax(panel$population, 1))
  attr(panel, "trend_cols") <- trend_cols
  panel
}

climate_covariates <- function(climate_lags = CLIMATE_LAGS) {
  c(
    paste0("mean_temp_celsius_lag", climate_lags),
    paste0("total_precip_mm_lag", climate_lags)
  )
}

case_ar_covariates <- function(ar_lags = AR_LAGS) {
  paste0("cases_lag", ar_lags)
}

search_ar_covariates <- function(trend_cols, ar_lags = AR_LAGS) {
  if (!length(trend_cols)) return(character(0))
  unlist(lapply(trend_cols, function(tc) paste0(tc, "_lag", ar_lags)), use.names = FALSE)
}

#' Fit INLA nbinomial with PC priors + BYM2 spatial.
fit_inla_count <- function(df,
                           covariates,
                           graph,
                           family = "nbinomial",
                           verbose = FALSE) {
  covs <- intersect(covariates, names(df))
  # Drop rows with NA in response, offset, or covariates used
  keep_cols <- c("cases", "log_pop", "loc_idx", covs)
  ok <- stats::complete.cases(df[, keep_cols, drop = FALSE])
  dat <- df[ok, , drop = FALSE]
  dat$loc_idx <- as.integer(dat$loc_idx)

  rhs <- paste(c(covs, "offset(log_pop)", "f(loc_idx, model = 'bym2', graph = graph, scale.model = TRUE)"), collapse = " + ")
  form <- stats::as.formula(paste("cases ~", rhs))

  pc_prec <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
  INLA::inla(
    form,
    family = family,
    data = dat,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
    control.fixed = list(prec = 0.001, prec.intercept = 0.001),
    control.family = list(hyper = list(theta = pc_prec)),
    verbose = verbose
  )
}

extract_inla_diagnostics <- function(fit, model_name, origin = NA) {
  cpo <- fit$cpo$cpo
  tibble::tibble(
    model = model_name,
    origin = as.Date(origin),
    waic = if (!is.null(fit$waic)) fit$waic$waic else NA_real_,
    dic = if (!is.null(fit$dic)) fit$dic$dic else NA_real_,
    cpo_fail = sum(is.na(cpo) | cpo == 0, na.rm = TRUE),
    cpo_n = length(cpo),
    mode_status = paste(fit$mode$status, collapse = ","),
    mlik = if (!is.null(fit$mlik)) as.numeric(fit$mlik[1, 1]) else NA_real_
  )
}

#' Approximate CRPS for a NegativeBinomial predictive draw set.
crps_sample <- function(y, samples) {
  # samples: n_draws
  y <- as.numeric(y)
  samples <- as.numeric(samples)
  mean(abs(samples - y)) - 0.5 * mean(abs(outer(samples, samples, "-")))
}

#' Sample predictive counts from INLA fit for held-out rows (approx via latent mean/sd).
predict_inla_samples <- function(fit, n_samples = 200L) {
  # Use fitted latent predictor mean/sd on observed scale via lincomb approximation:
  # INLA posterior for fitted values when compute=TRUE
  mu <- fit$summary.fitted.values$mean
  sd <- fit$summary.fitted.values$sd
  n <- length(mu)
  # Gaussian approx on mean scale then Poisson/NB sampling via mean
  # Prefer using inla.posterior.sample when config=TRUE
  if (!is.null(fit$misc$configs)) {
    ps <- INLA::inla.posterior.sample(n_samples, fit)
    # Extract predictor for each sample — predictor length = n
    mat <- sapply(ps, function(s) {
      pred <- s$latent
      # predictor entries typically named Predictor:*
      idx <- grep("^Predictor:", rownames(as.matrix(pred)))
      if (!length(idx)) idx <- seq_len(n)
      as.numeric(pred[idx[seq_len(n)], 1])
    })
    # link is log for nbinomial mean; convert to mean then sample NB size from hyperpar
    # Use mean = exp(eta); dispersion from summary
    theta <- fit$summary.hyperpar$mean[[1]]
    # INLA nbinomial: size = theta (approx) — overdispersion parameterisation varies
    size <- max(theta, 1e-3)
    means <- exp(mat)
    # return list of length n with samples
    lapply(seq_len(n), function(i) {
      m <- means[i, ]
      # NegBin via size & mu
      stats::rnbinom(n_samples, size = size, mu = pmax(m, 1e-8))
    })
  } else {
    lapply(seq_len(n), function(i) {
      m <- exp(stats::rnorm(n_samples, mean = log(pmax(mu[[i]], 1e-8)), sd = sd[[i]]))
      stats::rpois(n_samples, lambda = m)
    })
  }
}
