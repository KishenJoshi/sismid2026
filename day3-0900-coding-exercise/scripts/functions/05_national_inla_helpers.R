# Helpers for national INLA dengue models (ARGO-style + prior PPC).
#
# Model grid:
#   ar                  — case lags 1 + 12
#   climate_dlnm        — AR + nonlinear DLNM crossbasis (bake-off)
#   climate_rw2         — AR + linear temp/precip × INLA RW2 on lag coefs (1:6)
#   climate_temp_lag1   — AR + mean_temp_celsius lag 1 (raw, standardised)
#   climate_precip_lag1 — AR + total_precip_mm lag 1 (raw, standardised)
#   gt_{term}_lag1      — AR + one Google Trends series at lag 1 (raw, standardised)
#
# Engine: INLA nbinomial, no spatial (BYM2) term.
# Offset: offset(log_pop) with log_pop = log(population / 1e5).
# Fixed-effect priors: PC tail statements matched to Gaussians for control.fixed.
# Climate RW2: pc.prec on lag-field sigma (P(sigma > u) = alpha).
# NB size: PC prior via pc.mgamma (P(1/size > u) = alpha).
# Prior PPC: Monte Carlo from those priors + NegBin observation noise.

RETAINED_GT_TERMS <- c(
  "gt_fumigation",
  "gt_insecticide",
  "gt_mosquito",
  "gt_public_health"
)

NATIONAL_AR_LAGS <- c(1L, 12L)
CLIMATE_LAG_RANGE <- c(1L, 6L)
GT_LAG <- 1L

DLNM_TEMP_DF <- 2L
DLNM_PRECIP_DF <- 2L
DLNM_LAG_DF <- 2L

#---- PC prior defaults (plain-language tails; see lab book) -------------------
PC_ALPHA <- 0.05
PC_INTERCEPT_U <- 1.5
# Informative Normals on AR lags (not PC)
AR_LAG1_MEAN <- 1.0
AR_LAG1_SD <- 2.0
AR_LAG12_MEAN <- 1.0
AR_LAG12_SD <- 1.0
PC_BASIS_U <- 0.8 # climate_dlnm crossbasis columns only
PC_RW2_U <- 1.0 # climate_rw2: P(sigma > 1) = 0.05 on each lag field (option B)
# Informative Normal on GT lag-1 (not PC)
GT_LAG1_MEAN <- 1.0
GT_LAG1_SD <- 0.5
PC_NB_OVERDISP_U <- 7.0

ensure_dir <- function(...) {
  p <- file.path(...)
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

require_dlnm <- function() {
  if (!requireNamespace("dlnm", quietly = TRUE)) {
    stop("Package 'dlnm' is required for climate_dlnm. install.packages('dlnm')")
  }
}

require_inla <- function() {
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("Package 'INLA' is required. See https://www.r-inla.org/download-install")
  }
}

gt_term_short <- function(term) {
  sub("^gt_", "", term)
}

gt_lag1_z_names <- function(terms = RETAINED_GT_TERMS) {
  paste0(terms, "_lag", GT_LAG, "_z")
}

is_gt_lag1_z <- function(nm) {
  nm %in% gt_lag1_z_names()
}

is_climate_basis_z <- function(nm) {
  grepl("^cb_(temp|precip)_", nm)
}

is_climate_lag1_z <- function(nm) {
  grepl("^(mean_temp_celsius|total_precip_mm)_lag1_z$", nm)
}

#---- Panel prep --------------------------------------------------------------

prepare_national_modelling_panel <- function(panel) {
  drop_gt <- c(
    "gt_source_management", "gt_epidemic",
    "gt_dengue", "gt_Aedes_aegypti", "gt_vector"
  )
  drop_cols <- unique(c(
    drop_gt,
    unlist(lapply(drop_gt, function(t) grep(paste0("^", t, "_lag"), names(panel), value = TRUE)))
  ))
  panel <- panel %>%
    dplyr::select(-dplyr::any_of(drop_cols)) %>%
    dplyr::mutate(
      month_start = as.Date(month_start),
      log_pop = log(pmax(as.numeric(population), 1) / 1e5),
      monthly_cases = as.numeric(monthly_cases),
      mean_temp_celsius = as.numeric(mean_temp_celsius),
      total_precip_mm = as.numeric(total_precip_mm)
    )
  missing_gt <- setdiff(RETAINED_GT_TERMS, names(panel))
  if (length(missing_gt)) {
    stop("Missing retained Google Trends columns: ", paste(missing_gt, collapse = ", "))
  }
  panel
}

summarise_panel_coverage <- function(panel) {
  gt_ok <- rowSums(is.na(panel[, RETAINED_GT_TERMS, drop = FALSE])) == 0
  clim_ok <- is.finite(panel$mean_temp_celsius) & is.finite(panel$total_precip_mm)
  ar_cols <- paste0("monthly_cases_lag", NATIONAL_AR_LAGS)
  ar_ok <- rowSums(is.na(panel[, ar_cols, drop = FALSE])) == 0
  tibble::tibble(
    n_rows = nrow(panel),
    date_min = min(panel$month_start, na.rm = TRUE),
    date_max = max(panel$month_start, na.rm = TRUE),
    n_complete_gt = sum(gt_ok),
    n_complete_climate = sum(clim_ok),
    n_complete_ar = sum(ar_ok),
    n_complete_all = sum(
      gt_ok & clim_ok & ar_ok &
        is.finite(panel$monthly_cases) & is.finite(panel$log_pop)
    ),
    retained_gt = paste(RETAINED_GT_TERMS, collapse = ", ")
  )
}

ar_covariates <- function() {
  paste0("monthly_cases_lag", NATIONAL_AR_LAGS)
}

climate_lag_seq <- function() {
  seq.int(CLIMATE_LAG_RANGE[[1]], CLIMATE_LAG_RANGE[[2]])
}

temp_lag_covariates <- function() {
  paste0("mean_temp_celsius_lag", climate_lag_seq())
}

precip_lag_covariates <- function() {
  paste0("total_precip_mm_lag", climate_lag_seq())
}

gt_covariates <- function() {
  RETAINED_GT_TERMS
}

gt_lag1_covariates <- function(terms = RETAINED_GT_TERMS) {
  paste0(terms, "_lag", GT_LAG)
}

model_specs <- function() {
  specs <- list(
    ar = list(climate_method = "none", gt_term = NA_character_, gt_method = "none"),
    climate_dlnm = list(climate_method = "dlnm", gt_term = NA_character_, gt_method = "none"),
    climate_rw2 = list(climate_method = "rw2", gt_term = NA_character_, gt_method = "none"),
    climate_temp_lag1 = list(
      climate_method = "temp_lag1", gt_term = NA_character_, gt_method = "none"
    ),
    climate_precip_lag1 = list(
      climate_method = "precip_lag1", gt_term = NA_character_, gt_method = "none"
    )
  )
  for (term in RETAINED_GT_TERMS) {
    id <- paste0("gt_", gt_term_short(term), "_lag1")
    specs[[id]] <- list(
      climate_method = "none",
      gt_term = term,
      gt_method = "lag1"
    )
  }
  specs
}

#' Canonical model display order for facets / summaries.
national_model_levels <- function(models = NULL) {
  preferred <- names(model_specs())
  if (is.null(models)) {
    return(preferred)
  }
  models <- as.character(unique(models))
  c(intersect(preferred, models), setdiff(models, preferred))
}

order_model_factor <- function(df, model_col = "model") {
  lvls <- national_model_levels(df[[model_col]])
  df[[model_col]] <- factor(df[[model_col]], levels = lvls)
  df
}

#---- Fixed-knot climate / GT bases -------------------------------------------

ns_knot_spec <- function(x, df) {
  x <- x[is.finite(x)]
  if (length(x) < df + 2L) {
    stop("Not enough finite values to fix ns knots at df=", df)
  }
  basis <- splines::ns(x, df = df)
  list(
    df = df,
    knots = as.numeric(attr(basis, "knots")),
    Boundary.knots = as.numeric(attr(basis, "Boundary.knots"))
  )
}

#' Climate basis metadata. rw2 uses raw lagged exposures + INLA RW2 (no dlnm).
fit_climate_basis_spec <- function(panel, method = c("rw2", "dlnm")) {
  method <- match.arg(method)
  lag <- CLIMATE_LAG_RANGE

  if (identical(method, "rw2")) {
    return(list(
      method = "rw2",
      lag = lag,
      temp_cols = temp_lag_covariates(),
      precip_cols = precip_lag_covariates(),
      pc_rw2_u = PC_RW2_U,
      pc_alpha = PC_ALPHA
    ))
  }

  require_dlnm()
  temp <- panel$mean_temp_celsius
  precip <- panel$total_precip_mm
  temp_ns <- ns_knot_spec(temp, DLNM_TEMP_DF)
  precip_ns <- ns_knot_spec(precip, DLNM_PRECIP_DF)
  lag_grid <- seq.int(lag[1], lag[2])
  lag_ns <- ns_knot_spec(lag_grid, DLNM_LAG_DF)

  argvar_temp <- list(
    fun = "ns",
    knots = temp_ns$knots,
    Boundary.knots = temp_ns$Boundary.knots
  )
  argvar_precip <- list(
    fun = "ns",
    knots = precip_ns$knots,
    Boundary.knots = precip_ns$Boundary.knots
  )
  arglag <- list(
    fun = "ns",
    knots = lag_ns$knots,
    Boundary.knots = lag_ns$Boundary.knots
  )

  cb_temp <- dlnm::crossbasis(temp, lag = lag, argvar = argvar_temp, arglag = arglag)
  cb_precip <- dlnm::crossbasis(precip, lag = lag, argvar = argvar_precip, arglag = arglag)

  list(
    method = "dlnm",
    lag = lag,
    argvar_temp = argvar_temp,
    argvar_precip = argvar_precip,
    arglag = arglag,
    temp_ns = temp_ns,
    precip_ns = precip_ns,
    lag_ns = lag_ns,
    cb_temp_ref = cb_temp,
    cb_precip_ref = cb_precip
  )
}

crossbasis_to_df <- function(cb, prefix) {
  mat <- as.matrix(cb)
  colnames(mat) <- paste0(prefix, "_", seq_len(ncol(mat)))
  as.data.frame(mat)
}

add_climate_design <- function(
    panel,
    method = c("none", "rw2", "dlnm", "temp_lag1", "precip_lag1"),
    basis_spec = NULL
) {
  method <- match.arg(method)
  if (identical(method, "none")) {
    return(list(
      panel = panel,
      climate_cols = character(0),
      temp_cols = character(0),
      precip_cols = character(0)
    ))
  }

  if (identical(method, "temp_lag1")) {
    temp_cols <- "mean_temp_celsius_lag1"
    missing <- setdiff(temp_cols, names(panel))
    if (length(missing)) {
      stop("Missing climate lag column(s) for temp_lag1: ", paste(missing, collapse = ", "))
    }
    return(list(
      panel = panel,
      climate_cols = temp_cols,
      temp_cols = temp_cols,
      precip_cols = character(0)
    ))
  }

  if (identical(method, "precip_lag1")) {
    precip_cols <- "total_precip_mm_lag1"
    missing <- setdiff(precip_cols, names(panel))
    if (length(missing)) {
      stop(
        "Missing climate lag column(s) for precip_lag1: ",
        paste(missing, collapse = ", ")
      )
    }
    return(list(
      panel = panel,
      climate_cols = precip_cols,
      temp_cols = character(0),
      precip_cols = precip_cols
    ))
  }

  if (is.null(basis_spec) || !identical(basis_spec$method, method)) {
    stop("basis_spec for method='", method, "' must be supplied (fixed on full series).")
  }

  if (identical(method, "rw2")) {
    temp_cols <- basis_spec$temp_cols
    precip_cols <- basis_spec$precip_cols
    climate_cols <- c(temp_cols, precip_cols)
    missing <- setdiff(climate_cols, names(panel))
    if (length(missing)) {
      stop("Missing climate lag column(s) for rw2: ", paste(missing, collapse = ", "))
    }
    return(list(
      panel = panel,
      climate_cols = climate_cols,
      temp_cols = temp_cols,
      precip_cols = precip_cols
    ))
  }

  require_dlnm()
  cb_temp <- dlnm::crossbasis(
    panel$mean_temp_celsius,
    lag = basis_spec$lag,
    argvar = basis_spec$argvar_temp,
    arglag = basis_spec$arglag
  )
  cb_precip <- dlnm::crossbasis(
    panel$total_precip_mm,
    lag = basis_spec$lag,
    argvar = basis_spec$argvar_precip,
    arglag = basis_spec$arglag
  )
  temp_df <- crossbasis_to_df(cb_temp, "cb_temp")
  precip_df <- crossbasis_to_df(cb_precip, "cb_precip")
  temp_cols <- names(temp_df)
  precip_cols <- names(precip_df)
  climate_cols <- c(temp_cols, precip_cols)

  drop_old <- grep("^cb_temp_|^cb_precip_", names(panel), value = TRUE)
  panel2 <- panel %>% dplyr::select(-dplyr::any_of(drop_old))
  panel2 <- dplyr::bind_cols(panel2, temp_df, precip_df)
  list(
    panel = panel2,
    climate_cols = climate_cols,
    temp_cols = temp_cols,
    precip_cols = precip_cols
  )
}

#' GT design: raw lag-1 column already on the panel (no crossbasis).
add_gt_design <- function(panel, term, gt_method = c("none", "lag1")) {
  gt_method <- match.arg(gt_method)
  if (identical(gt_method, "none") || is.null(term) || is.na(term) || !nzchar(term)) {
    return(list(panel = panel, gt_cols = character(0)))
  }
  gt_cols <- paste0(term, "_lag", GT_LAG)
  missing <- setdiff(gt_cols, names(panel))
  if (length(missing)) {
    stop("Missing Google Trends lag column(s): ", paste(missing, collapse = ", "))
  }
  list(panel = panel, gt_cols = gt_cols)
}

covariates_for_spec <- function(spec, climate_cols = character(0), gt_cols = character(0)) {
  unique(c(ar_covariates(), climate_cols, gt_cols))
}

prepare_spec_frame <- function(panel, spec, climate_basis_specs, gt_basis_specs = list()) {
  method <- spec$climate_method
  clim_spec <- if (method %in% c("rw2", "dlnm")) climate_basis_specs[[method]] else NULL
  des <- add_climate_design(panel, method = method, basis_spec = clim_spec)

  gt_term <- spec$gt_term
  gt_method <- if (is.null(spec$gt_method) || is.na(spec$gt_method)) "none" else spec$gt_method
  gt_des <- add_gt_design(des$panel, term = gt_term, gt_method = gt_method)
  panel_out <- gt_des$panel
  gt_cols <- gt_des$gt_cols

  covs <- covariates_for_spec(spec, des$climate_cols, gt_cols)
  missing <- setdiff(covs, names(panel_out))
  if (length(missing)) {
    stop("Missing covariates for spec: ", paste(missing, collapse = ", "))
  }
  dat <- panel_out %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(covs), ~ is.finite(.x))) %>%
    dplyr::filter(is.finite(monthly_cases), is.finite(log_pop)) %>%
    dplyr::arrange(month_start)
  list(
    data = dat,
    covs_raw = covs,
    climate_cols = des$climate_cols,
    temp_cols = des$temp_cols,
    precip_cols = des$precip_cols,
    gt_cols = gt_cols,
    climate_method = method,
    gt_term = gt_term,
    gt_method = gt_method
  )
}

#---- PC priors → INLA control.fixed / control.family -------------------------

#' Match PC tail P(|beta| > u) = alpha (or one-sided) to a Normal SD.
pc_tail_to_normal_sd <- function(u, alpha = 0.05, sided = c("two", "one")) {
  sided <- match.arg(sided)
  if (any(!is.finite(u)) || any(u <= 0)) {
    stop("PC scale u must be finite and positive.")
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("PC alpha must be in (0, 1).")
  }
  z <- if (identical(sided, "two")) {
    stats::qnorm(1 - alpha / 2)
  } else {
    stats::qnorm(1 - alpha)
  }
  as.numeric(u) / z
}

#' National dengue prior catalogue (PC tails + informative AR Normals).
define_national_pc_priors <- function(alpha = PC_ALPHA) {
  intercept_sd <- pc_tail_to_normal_sd(PC_INTERCEPT_U, alpha = alpha)
  ar_lag1_mean <- AR_LAG1_MEAN
  ar_lag1_sd <- AR_LAG1_SD
  ar_lag12_mean <- AR_LAG12_MEAN
  ar_lag12_sd <- AR_LAG12_SD
  basis_sd <- pc_tail_to_normal_sd(PC_BASIS_U, alpha = alpha, sided = "two")
  gt_mean <- GT_LAG1_MEAN
  gt_sd <- GT_LAG1_SD
  list(
    alpha = alpha,
    intercept_u = PC_INTERCEPT_U,
    intercept_sd = intercept_sd,
    intercept_prec = 1 / intercept_sd^2,
    ar_lag1_mean = ar_lag1_mean,
    ar_lag1_sd = ar_lag1_sd,
    ar_lag1_prec = 1 / ar_lag1_sd^2,
    ar_lag12_mean = ar_lag12_mean,
    ar_lag12_sd = ar_lag12_sd,
    ar_lag12_prec = 1 / ar_lag12_sd^2,
    basis_u = PC_BASIS_U,
    basis_sd = basis_sd,
    basis_prec = 1 / basis_sd^2,
    rw2_u = PC_RW2_U,
    gt_mean = gt_mean,
    gt_sd = gt_sd,
    gt_prec = 1 / gt_sd^2,
    nb_u = PC_NB_OVERDISP_U,
    summary_text = paste0(
      "Priors (alpha = ", alpha, " for PC statements):\n",
      "  Intercept: P(|alpha| > ", PC_INTERCEPT_U, ") = ", alpha,
      " → Normal(0, sd = ", round(intercept_sd, 3), ") for INLA\n",
      "  AR lag-1: Normal(mean = ", ar_lag1_mean, ", sd = ", ar_lag1_sd, ")",
      " [informative; not PC]\n",
      "  Seasonal lag-12: Normal(mean = ", ar_lag12_mean, ", sd = ", ar_lag12_sd, ")",
      " [informative; not PC]\n",
      "  Climate DLNM basis slopes: P(|beta| > ", PC_BASIS_U, ") = ", alpha,
      " → Normal(0, sd = ", round(basis_sd, 3), ")\n",
      "  Climate lag-1 slopes (temp/precip): Normal(mean = ", gt_mean, ", sd = ", gt_sd, ")",
      " [same informative prior as GT lag-1]\n",
      "  Climate RW2 lag fields (temp & precip): P(sigma > ", PC_RW2_U, ") = ", alpha,
      " (INLA pc.prec; scale.model + sum-to-zero)\n",
      "  GT lag-1 slopes: Normal(mean = ", gt_mean, ", sd = ", gt_sd, ")",
      " [informative; not PC]\n",
      "  NegBin size: P(1/size > ", PC_NB_OVERDISP_U, ") = ", alpha,
      " (INLA prior pc.mgamma; larger size → closer to Poisson)"
    )
  )
}

#' Sample NegBin size under pc.mgamma: P(1/size > u) = alpha.
sample_nb_size_pc <- function(n, u = PC_NB_OVERDISP_U, alpha = PC_ALPHA) {
  rate <- -log(alpha) / u
  overdisp <- stats::rexp(n, rate = rate)
  1 / pmax(overdisp, .Machine$double.eps)
}

nbinomial_control_family <- function(u = PC_NB_OVERDISP_U, alpha = PC_ALPHA) {
  # INLA ≤24.x: pc.mgamma has 1 param (Exp rate on overdisp 1/size).
  # INLA ≥25/26: pc.mgamma has 2 params c(u, alpha) for P(1/size > u) = alpha.
  npar <- tryCatch(
    as.integer(INLA:::inla.models()$prior$pc.mgamma$nparameters),
    error = function(e) NA_integer_
  )
  param <- if (isTRUE(npar == 1L)) {
    -log(alpha) / u
  } else {
    c(u, alpha)
  }
  list(
    hyper = list(
      theta = list(
        prior = "pc.mgamma",
        param = param
      )
    )
  )
}

#' Build INLA control.fixed for standardised covariates.
national_inla_control_fixed <- function(covs_z, priors = define_national_pc_priors()) {
  ar1_z <- "monthly_cases_lag1_z"
  ar12_z <- "monthly_cases_lag12_z"
  mean_list <- stats::setNames(as.list(rep(0, length(covs_z))), covs_z)
  if (ar1_z %in% covs_z) {
    mean_list[[ar1_z]] <- priors$ar_lag1_mean
  }
  if (ar12_z %in% covs_z) {
    mean_list[[ar12_z]] <- priors$ar_lag12_mean
  }
  for (nm in covs_z) {
    if (is_gt_lag1_z(nm) || is_climate_lag1_z(nm)) {
      mean_list[[nm]] <- priors$gt_mean
    }
  }
  prec_list <- lapply(covs_z, function(nm) {
    if (identical(nm, ar1_z)) {
      priors$ar_lag1_prec
    } else if (identical(nm, ar12_z)) {
      priors$ar_lag12_prec
    } else if (is_gt_lag1_z(nm) || is_climate_lag1_z(nm)) {
      priors$gt_prec
    } else if (is_climate_basis_z(nm)) {
      priors$basis_prec
    } else {
      # Fallback: mild zero-mean (matched to GT sd scale)
      priors$gt_prec
    }
  })
  names(prec_list) <- covs_z
  list(
    mean.intercept = 0,
    prec.intercept = priors$intercept_prec,
    mean = mean_list,
    prec = prec_list
  )
}

#' control.fixed when intercept is an explicit FE (inla.stack / -1 + intercept).
national_inla_control_fixed_explicit_intercept <- function(
    covs_z,
    priors = define_national_pc_priors()
) {
  ctrl <- national_inla_control_fixed(covs_z, priors = priors)
  ctrl$mean <- c(list(intercept = 0), ctrl$mean)
  ctrl$prec <- c(list(intercept = priors$intercept_prec), ctrl$prec)
  ctrl$mean.intercept <- 0
  ctrl$prec.intercept <- priors$intercept_prec
  ctrl
}

rw2_hyper <- function(u = PC_RW2_U, alpha = PC_ALPHA) {
  list(prec = list(prior = "pc.prec", param = c(u, alpha)))
}

#' Scaled RW2 structure matrix (geom. mean marginal var ≈ 1 at tau = 1).
rw2_structure_matrix <- function(n_lags) {
  n_lags <- as.integer(n_lags)
  if (n_lags < 3L) stop("RW2 needs at least 3 lag points.")
  Q <- crossprod(diff(diag(n_lags), differences = 2L))
  eg <- eigen(Q, symmetric = TRUE)
  tol <- max(eg$values) * 1e-10
  pos <- eg$values > tol
  Qinv <- eg$vectors[, pos, drop = FALSE] %*%
    (t(eg$vectors[, pos, drop = FALSE]) / eg$values[pos])
  # Sum-to-zero projection for marginal variances used in scaling
  P <- diag(n_lags) - matrix(1 / n_lags, n_lags, n_lags)
  Qinv_c <- P %*% Qinv %*% P
  marg <- pmax(diag(Qinv_c), .Machine$double.eps)
  fac <- exp(mean(log(marg)))
  Q * fac
}

#' Draw RW2 fields under pc.prec: P(sigma > u) = alpha, sum-to-zero.
sample_rw2_pc <- function(n_draws, n_lags, u = PC_RW2_U, alpha = PC_ALPHA) {
  Q <- rw2_structure_matrix(n_lags)
  eg <- eigen(Q, symmetric = TRUE)
  tol <- max(eg$values) * 1e-10
  pos <- which(eg$values > tol)
  rate <- -log(alpha) / u
  sigma <- stats::rexp(n_draws, rate = rate)
  tau <- 1 / pmax(sigma, .Machine$double.eps)^2
  out <- matrix(0, nrow = n_draws, ncol = n_lags)
  for (i in seq_len(n_draws)) {
    z <- stats::rnorm(length(pos), sd = 1 / sqrt(tau[[i]] * eg$values[pos]))
    x <- as.numeric(eg$vectors[, pos, drop = FALSE] %*% z)
    out[i, ] <- x - mean(x)
  }
  out
}

build_negbin_formula <- function(covs_z) {
  rhs <- paste(c(covs_z, "offset(log_pop)"), collapse = " + ")
  stats::as.formula(paste("monthly_cases ~", rhs))
}

fit_national_inla <- function(dat, covs_z, verbose = FALSE) {
  require_inla()
  keep <- c("monthly_cases", "log_pop", covs_z)
  ok <- stats::complete.cases(dat[, keep, drop = FALSE])
  dat <- dat[ok, , drop = FALSE]
  form <- build_negbin_formula(covs_z)
  INLA::inla(
    form,
    family = "nbinomial",
    data = dat,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(
      dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE
    ),
    control.fixed = national_inla_control_fixed(covs_z),
    control.family = nbinomial_control_family(),
    silent = !verbose,
    verbose = verbose
  )
}

#' INLA fit: AR FEs + temp/precip RW2 lag fields via inla.stack A-matrices.
fit_national_inla_climate_rw2 <- function(
    dat,
    ar_covs_z,
    temp_cols_z,
    precip_cols_z,
    priors = define_national_pc_priors(),
    verbose = FALSE
) {
  require_inla()
  keep <- c("monthly_cases", "log_pop", ar_covs_z, temp_cols_z, precip_cols_z)
  ok <- stats::complete.cases(dat[, keep, drop = FALSE])
  dat <- dat[ok, , drop = FALSE]
  n <- nrow(dat)
  L <- length(temp_cols_z)
  if (length(precip_cols_z) != L) {
    stop("temp and precip RW2 lag designs must have the same length.")
  }

  Z_temp <- as.matrix(dat[, temp_cols_z, drop = FALSE])
  Z_precip <- as.matrix(dat[, precip_cols_z, drop = FALSE])
  storage.mode(Z_temp) <- "double"
  storage.mode(Z_precip) <- "double"

  fixed_df <- data.frame(
    intercept = rep(1, n),
    log_pop = as.numeric(dat$log_pop),
    stringsAsFactors = FALSE
  )
  for (nm in ar_covs_z) {
    fixed_df[[nm]] <- as.numeric(dat[[nm]])
  }

  stack <- INLA::inla.stack(
    data = list(monthly_cases = dat$monthly_cases),
    A = list(1, Z_temp, Z_precip),
    effects = list(
      fixed_df,
      list(temp_lag = seq_len(L)),
      list(precip_lag = seq_len(L))
    ),
    tag = "est"
  )

  form <- stats::as.formula(paste0(
    "monthly_cases ~ -1 + intercept + ",
    paste(ar_covs_z, collapse = " + "),
    " + offset(log_pop) + ",
    "f(temp_lag, model = 'rw2', scale.model = TRUE, constr = TRUE, ",
    "hyper = list(prec = list(prior = 'pc.prec', param = c(",
    priors$rw2_u, ", ", priors$alpha, ")))) + ",
    "f(precip_lag, model = 'rw2', scale.model = TRUE, constr = TRUE, ",
    "hyper = list(prec = list(prior = 'pc.prec', param = c(",
    priors$rw2_u, ", ", priors$alpha, "))))"
  ))

  INLA::inla(
    form,
    family = "nbinomial",
    data = INLA::inla.stack.data(stack),
    control.predictor = list(
      A = INLA::inla.stack.A(stack),
      compute = TRUE,
      link = 1
    ),
    control.compute = list(
      dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE
    ),
    control.fixed = national_inla_control_fixed_explicit_intercept(
      ar_covs_z, priors = priors
    ),
    control.family = nbinomial_control_family(),
    silent = !verbose,
    verbose = verbose
  )
}

#---- Prior predictive (seasonal-drivers style MC + NegBin noise) -------------

#' Draw prior predictive count matrix (n_draws × n_obs).
#' For climate_rw2, pass temp/precip standardised lag columns; they are not
#' treated as independent FE slopes (RW2 fields instead).
#' @param family "nbinomial" (default) or "poisson" (no overdispersion draw).
simulate_national_prior_counts <- function(
    dat,
    covs_z,
    n_draws = 2000L,
    seed = 20260808L,
    priors = define_national_pc_priors(),
    climate_method = "none",
    temp_cols_z = character(0),
    precip_cols_z = character(0),
    family = c("nbinomial", "poisson")
) {
  family <- match.arg(family)
  set.seed(seed)
  n <- nrow(dat)
  is_rw2 <- identical(climate_method, "rw2")
  fe_covs <- if (is_rw2) {
    setdiff(covs_z, c(temp_cols_z, precip_cols_z))
  } else {
    covs_z
  }

  X <- as.matrix(dat[, fe_covs, drop = FALSE])
  storage.mode(X) <- "double"
  log_pop <- as.numeric(dat$log_pop)

  alpha <- stats::rnorm(n_draws, mean = 0, sd = priors$intercept_sd)
  beta <- matrix(0, n_draws, length(fe_covs))
  colnames(beta) <- fe_covs
  for (nm in fe_covs) {
    if (identical(nm, "monthly_cases_lag1_z")) {
      beta[, nm] <- stats::rnorm(
        n_draws, mean = priors$ar_lag1_mean, sd = priors$ar_lag1_sd
      )
    } else if (identical(nm, "monthly_cases_lag12_z")) {
      beta[, nm] <- stats::rnorm(
        n_draws, mean = priors$ar_lag12_mean, sd = priors$ar_lag12_sd
      )
    } else if (is_gt_lag1_z(nm) || is_climate_lag1_z(nm)) {
      beta[, nm] <- stats::rnorm(
        n_draws, mean = priors$gt_mean, sd = priors$gt_sd
      )
    } else if (is_climate_basis_z(nm)) {
      beta[, nm] <- stats::rnorm(n_draws, mean = 0, sd = priors$basis_sd)
    } else {
      beta[, nm] <- stats::rnorm(
        n_draws, mean = priors$gt_mean, sd = priors$gt_sd
      )
    }
  }
  size <- if (identical(family, "nbinomial")) {
    sample_nb_size_pc(n_draws, u = priors$nb_u, alpha = priors$alpha)
  } else {
    NULL
  }

  delta_temp <- NULL
  delta_precip <- NULL
  Z_temp <- NULL
  Z_precip <- NULL
  if (is_rw2) {
    if (!length(temp_cols_z) || !length(precip_cols_z)) {
      stop("climate_method='rw2' requires temp_cols_z and precip_cols_z.")
    }
    Z_temp <- as.matrix(dat[, temp_cols_z, drop = FALSE])
    Z_precip <- as.matrix(dat[, precip_cols_z, drop = FALSE])
    storage.mode(Z_temp) <- "double"
    storage.mode(Z_precip) <- "double"
    L <- ncol(Z_temp)
    delta_temp <- sample_rw2_pc(
      n_draws, L, u = priors$rw2_u, alpha = priors$alpha
    )
    delta_precip <- sample_rw2_pc(
      n_draws, L, u = priors$rw2_u, alpha = priors$alpha
    )
  }

  yrep <- matrix(NA_real_, nrow = n_draws, ncol = n)
  for (s in seq_len(n_draws)) {
    eta <- alpha[[s]] + as.numeric(X %*% beta[s, ]) + log_pop
    if (is_rw2) {
      eta <- eta + as.numeric(Z_temp %*% delta_temp[s, ]) +
        as.numeric(Z_precip %*% delta_precip[s, ])
    }
    # Cap for numerical stability under diffuse prior tails
    mu <- exp(pmin(pmax(eta, -20), 20))
    if (identical(family, "poisson")) {
      yrep[s, ] <- stats::rpois(n, lambda = mu)
    } else {
      yrep[s, ] <- stats::rnbinom(n, size = size[[s]], mu = mu)
    }
  }
  yrep
}

summarise_prior_ribbons_from_yrep <- function(
    yrep,
    dat,
    probs = c(0.025, 0.25, 0.5, 0.75, 0.975)
) {
  qs <- apply(yrep, 2L, stats::quantile, probs = probs, na.rm = TRUE, names = FALSE)
  tibble::tibble(
    month_start = dat$month_start,
    observed = dat$monthly_cases,
    q0_025 = qs[1, ],
    q0_25 = qs[2, ],
    q0_5 = qs[3, ],
    q0_75 = qs[4, ],
    q0_975 = qs[5, ],
    covered_95 = dat$monthly_cases >= qs[1, ] & dat$monthly_cases <= qs[5, ]
  )
}

prior_coverage_95 <- function(ribbon_df) {
  ok <- is.finite(ribbon_df$observed) & !is.na(ribbon_df$covered_95)
  tibble::tibble(
    n_obs = sum(ok),
    coverage_95 = if (any(ok)) mean(ribbon_df$covered_95[ok]) else NA_real_,
    n_covered = if (any(ok)) sum(ribbon_df$covered_95[ok]) else 0L
  )
}

plot_prior_ribbons_counts <- function(ribbon_df, title) {
  ggplot2::ggplot(ribbon_df, ggplot2::aes(x = month_start)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q0_025, ymax = q0_975),
      fill = "#9ecae1", alpha = 0.35, colour = NA
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q0_25, ymax = q0_75),
      fill = "#3182bd", alpha = 0.25, colour = NA
    ) +
    ggplot2::geom_line(ggplot2::aes(y = q0_5), colour = "#08519c", linewidth = 0.6) +
    ggplot2::geom_line(
      ggplot2::aes(y = observed),
      colour = "grey25", linewidth = 0.35, alpha = 0.85
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Prior median with 50% and 95% intervals; observed counts overlaid",
      x = NULL,
      y = "Monthly dengue cases"
    ) +
    ggplot2::theme_bw(base_size = 11)
}

#---- Forecast utilities ------------------------------------------------------

rolling_forecast_origins <- function(
    dates,
    window_months = 60L,
    horizon = 3L,
    step = 1L
) {
  dates <- sort(unique(as.Date(dates)))
  if (length(dates) < window_months + horizon) {
    return(as.Date(character()))
  }
  last_train_idx <- seq.int(window_months, length(dates) - horizon, by = step)
  dates[last_train_idx]
}

scale_params_from_train <- function(train_df, cols) {
  mu <- lapply(cols, function(col) mean(train_df[[col]], na.rm = TRUE))
  sdv <- lapply(cols, function(col) {
    s <- stats::sd(train_df[[col]], na.rm = TRUE)
    if (is.na(s) || s == 0) 1 else s
  })
  names(mu) <- cols
  names(sdv) <- cols
  list(mu = mu, sd = sdv)
}

#' Extract intercept, fixed effects, and NB size from one inla.posterior.sample.
extract_inla_draw <- function(sample, covs_z) {
  lat <- as.matrix(sample$latent)
  rn <- rownames(lat)
  pick <- function(pattern) {
    i <- grep(pattern, rn)
    if (!length(i)) NA_real_ else as.numeric(lat[i[[1]], 1])
  }
  intercept <- pick("^\\(Intercept\\)")
  if (!is.finite(intercept)) intercept <- pick("^intercept(:|$)")
  if (!is.finite(intercept)) intercept <- 0
  beta <- vapply(covs_z, function(nm) {
    # INLA names look like "x:1"
    val <- pick(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", nm), "(:|$)"))
    if (!is.finite(val)) 0 else val
  }, numeric(1))
  names(beta) <- covs_z

  hyp <- sample$hyperpar
  size <- if (!is.null(hyp) && length(hyp)) {
    nm <- names(hyp)
    i <- grep("size|nbinomial", nm, ignore.case = TRUE)
    if (length(i)) as.numeric(hyp[[i[[1]]]]) else as.numeric(hyp[[1]])
  } else {
    NA_real_
  }
  if (!is.finite(size) || size <= 0) size <- 20
  list(intercept = intercept, beta = beta, size = size)
}

#' Extract AR FEs + RW2 lag fields from an INLA posterior sample.
extract_inla_draw_rw2 <- function(sample, ar_covs_z, n_lags) {
  base <- extract_inla_draw(sample, ar_covs_z)
  lat <- as.matrix(sample$latent)
  rn <- rownames(lat)
  pick_field <- function(prefix, k) {
    pat <- paste0("^", prefix, ":", k, "$")
    i <- grep(pat, rn)
    if (!length(i)) {
      # alternate naming: "temp_lag:1" already covered; try without colon variants
      i <- grep(paste0("^", prefix, ".*\\b", k, "\\b"), rn)
    }
    if (!length(i)) 0 else as.numeric(lat[i[[1]], 1])
  }
  delta_temp <- vapply(seq_len(n_lags), function(k) pick_field("temp_lag", k), numeric(1))
  delta_precip <- vapply(seq_len(n_lags), function(k) pick_field("precip_lag", k), numeric(1))
  list(
    intercept = base$intercept,
    beta = base$beta,
    size = base$size,
    delta_temp = delta_temp,
    delta_precip = delta_precip
  )
}

#' Recursive h-step forecast using INLA posterior samples + NegBin noise.
forecast_recursive_counts <- function(
    fit,
    panel,
    origin,
    covs_raw,
    scale_mu,
    scale_sd,
    horizon = 3L,
    ndraws = 400L,
    climate_method = "none",
    temp_cols_raw = character(0),
    precip_cols_raw = character(0)
) {
  require_inla()
  origin <- as.Date(origin)
  dates <- sort(unique(panel$month_start))
  origin_idx <- match(origin, dates)
  if (is.na(origin_idx) || origin_idx + horizon > length(dates)) {
    return(NULL)
  }

  is_rw2 <- identical(climate_method, "rw2")
  ar_raw <- intersect(covs_raw, ar_covariates())
  fe_raw <- if (is_rw2) {
    setdiff(covs_raw, c(temp_cols_raw, precip_cols_raw))
  } else {
    covs_raw
  }
  fe_z <- paste0(fe_raw, "_z")
  temp_z <- paste0(temp_cols_raw, "_z")
  precip_z <- paste0(precip_cols_raw, "_z")
  n_lags <- length(temp_cols_raw)

  ps <- INLA::inla.posterior.sample(ndraws, fit)
  draws <- if (is_rw2) {
    lapply(ps, extract_inla_draw_rw2, ar_covs_z = fe_z, n_lags = n_lags)
  } else {
    lapply(ps, extract_inla_draw, covs_z = fe_z)
  }

  # Each draw keeps its own recursive case path
  work_list <- lapply(seq_len(ndraws), function(i) panel)
  out <- vector("list", horizon)

  for (h in seq_len(horizon)) {
    target <- dates[[origin_idx + h]]
    yrep <- numeric(ndraws)
    for (s in seq_len(ndraws)) {
      work <- work_list[[s]]
      row_i <- match(target, work$month_start)
      for (L in NATIONAL_AR_LAGS) {
        lag_date <- dates[[origin_idx + h - L]]
        lag_row <- match(lag_date, work$month_start)
        work[[paste0("monthly_cases_lag", L)]][row_i] <- work$monthly_cases[lag_row]
      }
      x_z <- numeric(length(fe_z))
      names(x_z) <- fe_z
      for (i in seq_along(fe_raw)) {
        col <- fe_raw[[i]]
        x_z[[fe_z[[i]]]] <- (work[[col]][row_i] - scale_mu[[col]]) / scale_sd[[col]]
      }
      d <- draws[[s]]
      eta <- d$intercept + sum(d$beta * x_z[fe_z]) + work$log_pop[row_i]
      if (is_rw2) {
        z_t <- vapply(seq_along(temp_cols_raw), function(j) {
          col <- temp_cols_raw[[j]]
          (work[[col]][row_i] - scale_mu[[col]]) / scale_sd[[col]]
        }, numeric(1))
        z_p <- vapply(seq_along(precip_cols_raw), function(j) {
          col <- precip_cols_raw[[j]]
          (work[[col]][row_i] - scale_mu[[col]]) / scale_sd[[col]]
        }, numeric(1))
        eta <- eta + sum(d$delta_temp * z_t) + sum(d$delta_precip * z_p)
      }
      mu <- exp(pmin(pmax(eta, -20), 20))
      yhat <- stats::rnbinom(1L, size = d$size, mu = mu)
      work$monthly_cases[row_i] <- yhat
      work_list[[s]] <- work
      yrep[[s]] <- yhat
    }

    obs <- panel$monthly_cases[match(target, panel$month_start)]
    q025 <- stats::quantile(yrep, 0.025, names = FALSE)
    q25 <- stats::quantile(yrep, 0.25, names = FALSE)
    q50 <- stats::quantile(yrep, 0.5, names = FALSE)
    q75 <- stats::quantile(yrep, 0.75, names = FALSE)
    q975 <- stats::quantile(yrep, 0.975, names = FALSE)
    out[[h]] <- tibble::tibble(
      origin = origin,
      target = target,
      horizon = h,
      observed = obs,
      pred_mean = mean(yrep),
      pred_q025 = q025,
      pred_q25 = q25,
      pred_q50 = q50,
      pred_q75 = q75,
      pred_q975 = q975,
      covered_95 = is.finite(obs) & obs >= q025 & obs <= q975,
      crps = crps_sample(obs, yrep),
      wis = wis_sample(obs, yrep)
    )
  }
  dplyr::bind_rows(out)
}

#---- Forecast scoring / diagnostics (script 06) ------------------------------

#' Sample-based CRPS (Gneiting & Raftery).
crps_sample <- function(y, samples) {
  y <- as.numeric(y)
  samples <- as.numeric(samples)
  if (!is.finite(y) || !length(samples)) return(NA_real_)
  samples <- samples[is.finite(samples)]
  if (!length(samples)) return(NA_real_)
  mean(abs(samples - y)) - 0.5 * mean(abs(outer(samples, samples, "-")))
}

#' Interval score for a central (1 - alpha) predictive interval.
interval_score <- function(y, lower, upper, alpha) {
  (upper - lower) +
    (2 / alpha) * (lower - y) * (y < lower) +
    (2 / alpha) * (y - upper) * (y > upper)
}

#' Approximate WIS from samples (median AE + central interval scores).
wis_sample <- function(y, samples, alphas = c(0.1, 0.2, 0.5, 0.8, 0.9)) {
  y <- as.numeric(y)
  samples <- as.numeric(samples)
  if (!is.finite(y) || !length(samples)) return(NA_real_)
  samples <- samples[is.finite(samples)]
  if (!length(samples)) return(NA_real_)
  m <- stats::median(samples)
  score <- abs(y - m)
  for (a in alphas) {
    # alpha = excluded probability mass; interval level = 1 - a
    lower <- stats::quantile(samples, probs = a / 2, names = FALSE, type = 7)
    upper <- stats::quantile(samples, probs = 1 - a / 2, names = FALSE, type = 7)
    score <- score + interval_score(y, lower, upper, alpha = a)
  }
  score / (length(alphas) + 1)
}

extract_national_inla_diagnostics <- function(fit, model_name, origin = NA) {
  cpo <- if (!is.null(fit$cpo)) fit$cpo$cpo else NULL
  fail <- if (!is.null(fit$cpo) && !is.null(fit$cpo$failure)) {
    sum(fit$cpo$failure > 0, na.rm = TRUE)
  } else if (!is.null(cpo)) {
    sum(is.na(cpo) | cpo == 0, na.rm = TRUE)
  } else {
    NA_integer_
  }
  mode_status <- if (!is.null(fit$mode) && !is.null(fit$mode$status)) {
    paste(fit$mode$status, collapse = ",")
  } else {
    NA_character_
  }
  tibble::tibble(
    model = model_name,
    origin = as.Date(origin),
    mode_status = mode_status,
    cpo_fail = as.integer(fail),
    cpo_n = if (!is.null(cpo)) length(cpo) else NA_integer_,
    mlik = if (!is.null(fit$mlik)) as.numeric(fit$mlik[1, 1]) else NA_real_
  )
}

#' Origins whose h=1..horizon forecasts land in [test_start, test_end].
expanding_test_origins <- function(
    dates,
    test_start = as.Date("2020-01-01"),
    test_end = as.Date("2022-12-01"),
    horizon = 3L,
    step = 1L
) {
  dates <- sort(unique(as.Date(dates)))
  test_start <- as.Date(test_start)
  test_end <- as.Date(test_end)
  # Need origin such that origin+1 .. origin+horizon cover test months as needed
  min_origin <- test_start
  # for h=1 target = origin + 1 month ≈ test_start ⇒ origin = test_start - 1 month
  min_origin <- seq.Date(test_start, by = "-1 month", length.out = 2L)[[2L]]
  max_origin <- seq.Date(test_end, by = paste0("-", horizon, " months"), length.out = 2L)[[2L]]
  origins <- dates[dates >= min_origin & dates <= max_origin]
  if (step > 1L) {
    origins <- origins[seq(1L, length(origins), by = step)]
  }
  origins
}

summarise_crps_by_calendar_month <- function(scores_df) {
  scores_df %>%
    dplyr::mutate(month_of_year = as.integer(format(target, "%m"))) %>%
    dplyr::group_by(model, horizon, month_of_year) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_crps = mean(crps, na.rm = TRUE),
      mean_wis = mean(wis, na.rm = TRUE),
      coverage_95 = mean(covered_95, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_crpss_vs_ar <- function(scores_df, baseline = "ar") {
  base <- scores_df %>%
    dplyr::filter(model == baseline) %>%
    dplyr::select(origin, target, horizon, crps_base = crps, wis_base = wis)
  scores_df %>%
    dplyr::left_join(base, by = c("origin", "target", "horizon")) %>%
    dplyr::group_by(model, horizon) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_crps = mean(crps, na.rm = TRUE),
      mean_crps_ar = mean(crps_base, na.rm = TRUE),
      crpss_vs_ar = 1 - mean(crps, na.rm = TRUE) / mean(crps_base, na.rm = TRUE),
      mean_wis = mean(wis, na.rm = TRUE),
      mean_wis_ar = mean(wis_base, na.rm = TRUE),
      wiss_vs_ar = 1 - mean(wis, na.rm = TRUE) / mean(wis_base, na.rm = TRUE),
      coverage_95 = mean(covered_95, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_seasonal_incidence_profile <- function(panel_df) {
  panel_df %>%
    dplyr::mutate(
      month_of_year = as.integer(format(as.Date(month_start), "%m")),
      incidence = 1e5 * as.numeric(monthly_cases) / pmax(as.numeric(population), 1)
    ) %>%
    dplyr::filter(is.finite(incidence), is.finite(month_of_year)) %>%
    dplyr::group_by(month_of_year) %>%
    dplyr::summarise(
      mean_inc = mean(incidence, na.rm = TRUE),
      lo_95 = as.numeric(stats::quantile(incidence, 0.025, names = FALSE, na.rm = TRUE)),
      hi_95 = as.numeric(stats::quantile(incidence, 0.975, names = FALSE, na.rm = TRUE)),
      n_years = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(month_of_year)
}

plot_seasonal_incidence_profile <- function(profile_df) {
  ggplot2::ggplot(
    profile_df,
    ggplot2::aes(x = month_of_year, y = mean_inc)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lo_95, ymax = hi_95),
      fill = "#fcbba1", alpha = 0.45, colour = NA
    ) +
    ggplot2::geom_line(colour = "#a50f15", linewidth = 0.6) +
    ggplot2::geom_point(colour = "#a50f15", size = 1.4) +
    ggplot2::scale_x_continuous(breaks = 1:12) +
    ggplot2::labs(
      title = "Average dengue seasonal profile",
      subtitle = "Mean monthly incidence with 95% interval across years",
      x = NULL,
      y = "Incidence per 100,000"
    ) +
    ggplot2::theme_bw(base_size = 10)
}

plot_incidence_timeseries_panel <- function(panel_df, date_min = NULL, date_max = NULL) {
  plot_df <- panel_df %>%
    dplyr::mutate(
      month_start = as.Date(month_start),
      incidence = 1e5 * as.numeric(monthly_cases) / pmax(as.numeric(population), 1)
    ) %>%
    dplyr::filter(is.finite(incidence))
  if (!is.null(date_min)) {
    plot_df <- dplyr::filter(plot_df, month_start >= as.Date(date_min))
  }
  if (!is.null(date_max)) {
    plot_df <- dplyr::filter(plot_df, month_start <= as.Date(date_max))
  }
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = month_start, y = incidence)
  ) +
    ggplot2::geom_line(colour = "#a50f15", linewidth = 0.55) +
    ggplot2::labs(
      title = "Dengue incidence",
      x = NULL,
      y = "Incidence per 100,000"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7)
    )
}

plot_score_by_target <- function(
    scores_df,
    metric = c("crps", "wis", "covered_95"),
    title,
    panel_df = NULL
) {
  metric <- match.arg(metric)
  ylab <- switch(metric,
    crps = "CRPS",
    wis = "WIS",
    covered_95 = "95% coverage (0/1)"
  )
  p_score <- ggplot2::ggplot(
    order_model_factor(scores_df),
    ggplot2::aes(x = target, y = .data[[metric]])
  ) +
    ggplot2::geom_line(linewidth = 0.35, alpha = 0.85) +
    ggplot2::geom_point(size = 0.6, alpha = 0.7) +
    ggplot2::facet_grid(model ~ horizon) +
    ggplot2::labs(title = title, x = NULL, y = ylab) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      strip.text.y = ggplot2::element_text(angle = 0),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7)
    )

  if (is.null(panel_df)) {
    return(p_score)
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required to stack incidence above score plots.")
  }
  p_inc <- plot_incidence_timeseries_panel(
    panel_df,
    date_min = min(scores_df$target, na.rm = TRUE),
    date_max = max(scores_df$target, na.rm = TRUE)
  )
  patchwork::wrap_plots(p_inc, p_score, ncol = 1, heights = c(1, 3))
}

plot_crps_by_calendar_month <- function(month_df, title, panel_df = NULL) {
  p_crps <- ggplot2::ggplot(
    order_model_factor(month_df),
    ggplot2::aes(x = month_of_year, y = mean_crps)
  ) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 1.2) +
    ggplot2::scale_x_continuous(breaks = 1:12) +
    ggplot2::facet_grid(model ~ horizon) +
    ggplot2::labs(
      title = title,
      subtitle = "Mean CRPS by calendar month of target",
      x = "Month of year",
      y = "Mean CRPS"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0))

  if (is.null(panel_df)) {
    return(p_crps)
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required to stack seasonal incidence above CRPS.")
  }
  p_season <- plot_seasonal_incidence_profile(
    summarise_seasonal_incidence_profile(panel_df)
  )
  patchwork::wrap_plots(p_season, p_crps, ncol = 1, heights = c(1, 3))
}

#' Forecast ribbons/lines in incidence space (cases per 100,000).
#' `scores_df` must include `population` (join from the panel if needed).
#' @param y_scale `"incidence"` (default) or `"log_incidence"` (`log1p` of per-100k).
plot_forecast_timeseries <- function(
    scores_df,
    title,
    y_scale = c("incidence", "log_incidence")
) {
  y_scale <- match.arg(y_scale)
  if (!"population" %in% names(scores_df)) {
    stop("plot_forecast_timeseries() needs a 'population' column for incidence scaling.")
  }
  plot_df <- order_model_factor(scores_df) %>%
    dplyr::mutate(
      pop = pmax(as.numeric(population), 1),
      observed_inc = 1e5 * as.numeric(observed) / pop,
      pred_q025_inc = 1e5 * as.numeric(pred_q025) / pop,
      pred_q25_inc = 1e5 * as.numeric(pred_q25) / pop,
      pred_q50_inc = 1e5 * as.numeric(pred_q50) / pop,
      pred_q75_inc = 1e5 * as.numeric(pred_q75) / pop,
      pred_q975_inc = 1e5 * as.numeric(pred_q975) / pop
    )

  ylab <- "Monthly dengue incidence per 100,000"
  sub <- "Observed vs predictive median with 50% / 95% ribbons (incidence)"
  if (identical(y_scale, "log_incidence")) {
    plot_df <- plot_df %>%
      dplyr::mutate(
        dplyr::across(
          c(
            observed_inc, pred_q025_inc, pred_q25_inc,
            pred_q50_inc, pred_q75_inc, pred_q975_inc
          ),
          ~ log1p(.x)
        )
      )
    ylab <- "log(1 + monthly dengue incidence per 100,000)"
    sub <- "Observed vs predictive median with 50% / 95% ribbons (log1p incidence)"
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = target)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pred_q025_inc, ymax = pred_q975_inc),
      fill = "#9ecae1", alpha = 0.35, colour = NA
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pred_q25_inc, ymax = pred_q75_inc),
      fill = "#3182bd", alpha = 0.25, colour = NA
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = pred_q50_inc, colour = "Predictive median"),
      linewidth = 0.55
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = observed_inc, colour = "Observed"),
      linewidth = 0.4, alpha = 0.9
    ) +
    ggplot2::scale_colour_manual(
      name = NULL,
      values = c(
        "Observed" = "#d73027",
        "Predictive median" = "#08519c"
      )
    ) +
    ggplot2::facet_grid(model ~ horizon) +
    ggplot2::labs(
      title = title,
      subtitle = sub,
      x = NULL,
      y = ylab
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      strip.text.y = ggplot2::element_text(angle = 0),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
      legend.position = "bottom"
    )
}
