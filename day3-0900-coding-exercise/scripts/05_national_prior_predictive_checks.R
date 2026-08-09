# 05_test_inla_models_national.R
# National Mexico dengue: INLA models with climate / single-GT bake-off,
# prior predictive checks (MC from PC priors + NegBin noise), and
# 5-year / 3-step forecasts.
#
# Model grid:
#   ar                         — case lags 1 + 12 (seasonal)
#   climate_rw2                — ar + linear climate × INLA RW2 on lag coefs (1:6)
#   climate_dlnm               — ar + nonlinear climate DLNM (lags 1:6)
#   climate_temp_lag1          — ar + mean temperature lag 1 (raw)
#   climate_precip_lag1        — ar + total precipitation lag 1 (raw)
#   gt_{term}_lag1             — ar + one retained Trends series at lag 1 (raw)
#
# Likelihood: INLA nbinomial + offset(log(population / 1e5)); no BYM2 (national)
# Priors: PC tails matched to Gaussians for FEs; pc.prec on RW2; pc.mgamma on NB
#
# Usage (from repo root):
#   Rscript day3-0900-coding-exercise/scripts/05_test_inla_models_national.R
#   Rscript day3-0900-coding-exercise/scripts/05_test_inla_models_national.R --skip-forecast
#   Rscript day3-0900-coding-exercise/scripts/05_test_inla_models_national.R --max-origins 3

suppressPackageStartupMessages({
  library(tidyverse)
  library(tibble)
  library(ggplot2)
  library(splines)
  library(INLA)
})

source("day3-0900-coding-exercise/scripts/functions/lag_helpers.R")
source("day3-0900-coding-exercise/scripts/functions/05_national_inla_helpers.R")

#--------------- CLI -----

args <- commandArgs(trailingOnly = TRUE)
skip_forecast <- "--skip-forecast" %in% args
poisson_ppc <- "--poisson-ppc" %in% args
max_origins <- Inf
if ("--max-origins" %in% args) {
  i <- which(args == "--max-origins")
  max_origins <- as.integer(args[[i + 1L]])
}

ppc_ndraws <- 2000L
forecast_ndraws <- 400L
window_months <- 60L
horizon <- 3L
seed <- 20260808L
ppc_family <- if (isTRUE(poisson_ppc)) "poisson" else "nbinomial"

out_dir <- ensure_dir(
  "day3-0900-coding-exercise", "results", "05_test_inla_models_national"
)
ppc_dir <- ensure_dir(
  out_dir,
  if (identical(ppc_family, "poisson")) "prior_predictive_poisson" else "prior_predictive"
)
fc_dir <- ensure_dir(out_dir, "rolling_forecast")

#--------------- Data -----

panel_raw <- read_csv(
  "day3-0900-coding-exercise/data/mx_national_exposure_response_monthly.csv",
  show_col_types = FALSE
)
panel <- prepare_national_modelling_panel(panel_raw)

coverage <- summarise_panel_coverage(panel)
message("=== National panel coverage ===")
print(as.data.frame(coverage), row.names = FALSE)
write_csv(coverage, file.path(out_dir, "panel_coverage_summary.csv"))

base_covs <- c(
  ar_covariates(),
  "mean_temp_celsius", "total_precip_mm",
  temp_lag_covariates(),
  precip_lag_covariates(),
  gt_covariates(),
  gt_lag1_covariates()
)
panel_base <- panel %>%
  filter(if_all(all_of(intersect(base_covs, names(panel))), ~ is.finite(.x))) %>%
  filter(is.finite(monthly_cases), is.finite(log_pop)) %>%
  arrange(month_start)

message(
  "Base complete rows: ", nrow(panel_base),
  " [", min(panel_base$month_start), " .. ", max(panel_base$month_start), "]"
)

priors <- define_national_pc_priors()
message("\n", priors$summary_text)
writeLines(priors$summary_text, file.path(out_dir, "pc_prior_summary.txt"))

message("Building fixed climate basis specs on full series...")
climate_basis_specs <- list(
  rw2 = fit_climate_basis_spec(panel_base, method = "rw2"),
  dlnm = fit_climate_basis_spec(panel_base, method = "dlnm")
)
saveRDS(climate_basis_specs, file.path(out_dir, "climate_basis_specs_fixed.RDS"))

specs <- model_specs()
write_csv(
  tibble(
    model = names(specs),
    climate_method = vapply(specs, `[[`, character(1), "climate_method"),
    gt_term = vapply(specs, function(s) {
      if (is.null(s$gt_term) || is.na(s$gt_term)) NA_character_ else s$gt_term
    }, character(1)),
    gt_method = vapply(specs, `[[`, character(1), "gt_method"),
    engine = "INLA",
    n_base_rows = nrow(panel_base),
    date_min = min(panel_base$month_start),
    date_max = max(panel_base$month_start),
    retained_gt = paste(RETAINED_GT_TERMS, collapse = ", "),
    ar_lags = paste(NATIONAL_AR_LAGS, collapse = ", "),
    climate_lags = paste(CLIMATE_LAG_RANGE[1]:CLIMATE_LAG_RANGE[2], collapse = ", "),
    gt_lags = as.character(GT_LAG),
    pc_alpha = priors$alpha,
    pc_intercept_u = priors$intercept_u,
    ar_lag1_mean = priors$ar_lag1_mean,
    ar_lag1_sd = priors$ar_lag1_sd,
    ar_lag12_mean = priors$ar_lag12_mean,
    ar_lag12_sd = priors$ar_lag12_sd,
    pc_basis_u = priors$basis_u,
    pc_rw2_u = priors$rw2_u,
    gt_lag1_mean = priors$gt_mean,
    gt_lag1_sd = priors$gt_sd,
    pc_nb_u = priors$nb_u,
    dlnm_temp_df = DLNM_TEMP_DF,
    dlnm_precip_df = DLNM_PRECIP_DF,
    dlnm_lag_df = DLNM_LAG_DF,
    window_months = window_months,
    horizon = horizon
  ),
  file.path(out_dir, "model_design_summary.csv")
)

#--------------- Prior predictive checks (all models) -----

message("\n=== Prior PPC family: ", ppc_family, " ===")
ppc_coverage_rows <- list()

for (model_name in names(specs)) {
  message("\n=== Prior PPC: ", model_name, " ===")
  spec <- specs[[model_name]]
  prep <- prepare_spec_frame(panel_base, spec, climate_basis_specs)
  dat <- scale_with_train(prep$data, prep$covs_raw, train_idx = seq_len(nrow(prep$data)))
  covs_z <- paste0(prep$covs_raw, "_z")
  temp_z <- if (length(prep$temp_cols)) paste0(prep$temp_cols, "_z") else character(0)
  precip_z <- if (length(prep$precip_cols)) paste0(prep$precip_cols, "_z") else character(0)

  yrep <- simulate_national_prior_counts(
    dat = dat,
    covs_z = covs_z,
    n_draws = ppc_ndraws,
    seed = seed,
    priors = priors,
    climate_method = prep$climate_method,
    temp_cols_z = temp_z,
    precip_cols_z = precip_z,
    family = ppc_family
  )
  ribbons <- summarise_prior_ribbons_from_yrep(yrep, dat) %>%
    mutate(model = model_name, family = ppc_family)
  cov95 <- prior_coverage_95(ribbons) %>%
    mutate(model = model_name, family = ppc_family)

  message(
    "coverage_95 (counts) = ", round(cov95$coverage_95, 3),
    " (", cov95$n_covered, "/", cov95$n_obs, ")"
  )

  write_csv(ribbons, file.path(ppc_dir, paste0("ppc_ribbons_", model_name, ".csv")))
  ggsave(
    file.path(ppc_dir, paste0("ppc_ribbons_", model_name, ".png")),
    plot_prior_ribbons_counts(
      ribbons,
      title = paste0(
        "National prior predictive counts — ", model_name,
        " (", ppc_family, ")"
      )
    ),
    width = 10,
    height = 4.5,
    dpi = 120
  )
  ppc_coverage_rows[[model_name]] <- cov95
}

ppc_coverage <- bind_rows(ppc_coverage_rows) %>%
  select(model, family, n_obs, n_covered, coverage_95)
write_csv(ppc_coverage, file.path(ppc_dir, "ppc_coverage_95_counts.csv"))
message("\n=== Prior predictive 95% coverage (counts, ", ppc_family, ") ===")
print(as.data.frame(ppc_coverage), row.names = FALSE)
