# 06_fit_inla_baseline_and_trends.R
# Fit full-sample INLA climate-only and Trends-augmented models (PC priors + BYM2).

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(INLA)
  library(tibble)
})

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
source(file.path(root, "scripts/functions/model_helpers.R"))
source_model_deps(root)

out_dir <- ensure_dir(DAY3_ROOT, "outputs", "models")
graph <- readRDS(file.path(DAY3_ROOT, "outputs/spatial/mx_knn10_idw_graph.RDS"))

trends_path <- file.path(DAY3_ROOT, "data/mx_adm1_gtrends_monthly.csv")
if (!file.exists(trends_path)) trends_path <- NULL

panel <- build_modelling_panel(
  cases_path = file.path(DAY3_ROOT, "data/mx_adm1_cases_monthly.csv"),
  climate_path = file.path(DAY3_ROOT, "data/mx_adm1_climate_monthly.csv"),
  location_index_path = file.path(DAY3_ROOT, "outputs/spatial/mx_location_index.csv"),
  trends_path = trends_path
)

clim_covs <- climate_covariates()
case_covs <- case_ar_covariates()
trend_cols <- attr(panel, "trend_cols")
search_covs <- search_ar_covariates(trend_cols)

panel_fit <- panel %>%
  # Longest recent continuous OpenDengue MX Admin1 block (gaps: 2008-11, 2014-16)
  dplyr::filter(month_start >= as.Date("2017-01-01"), month_start <= as.Date("2022-12-01"))

message("Fitting INLA climate baseline...")
fit_clim <- fit_inla_count(panel_fit, covariates = c(clim_covs, case_covs), graph = graph)
saveRDS(fit_clim, file.path(out_dir, "inla_climate_baseline.RDS"))
diag_clim <- extract_inla_diagnostics(fit_clim, "inla_climate_baseline")

diag_tr <- NULL
if (length(search_covs)) {
  message("Fitting INLA Trends-augmented model...")
  fit_tr <- fit_inla_count(
    panel_fit,
    covariates = c(clim_covs, case_covs, search_covs),
    graph = graph
  )
  saveRDS(fit_tr, file.path(out_dir, "inla_climate_trends.RDS"))
  diag_tr <- extract_inla_diagnostics(fit_tr, "inla_climate_trends")
} else {
  message("No Trends columns found — skipping Trends INLA fit.")
}

diags <- dplyr::bind_rows(diag_clim, diag_tr)
readr::write_csv(diags, file.path(out_dir, "inla_fullsample_diagnostics.csv"))
print(diags)
message("INLA full-sample fits written to ", out_dir)
