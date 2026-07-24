# 07_fit_brms_screen_then_inla.R
# Non-spatial brms horseshoe screen (climate-only and Trends) -> selected subset -> spatial INLA.

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

has_brms <- requireNamespace("brms", quietly = TRUE)
if (!has_brms) {
  stop("Package 'brms' is required for script 07. Install with install.packages('brms').")
}
library(brms)

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
  dplyr::filter(month_start >= as.Date("2017-01-01"), month_start <= as.Date("2022-12-01"))

# Standardise predictors for horseshoe stability
z_candidates <- c(clim_covs, case_covs, search_covs)
z_candidates <- intersect(z_candidates, names(panel_fit))
panel_fit <- scale_with_train(panel_fit, z_candidates, train_idx = seq_len(nrow(panel_fit)))
z_covs <- paste0(z_candidates, "_z")

select_important <- function(fit, keep_case_ar = TRUE, prob_threshold = 0.5) {
  # Use posterior inclusion via |beta| credibility away from zero as a simple screen.
  # brms >= 2.21 exports posterior_summary (fix_summary was removed).
  fe <- as.data.frame(brms::fixef(fit))
  fe$Parameter <- rownames(fe)
  fe <- fe %>%
    dplyr::filter(!grepl("Intercept|shape|hu", Parameter)) %>%
    dplyr::mutate(keep = (Estimate > 0 & Q2.5 > 0) | (Estimate < 0 & Q97.5 < 0) |
      abs(Estimate) / Est.Error > 1)
  kept <- gsub("^b_", "", fe$Parameter[fe$keep])
  # Map z-names back to raw covariate names
  raw <- sub("_z$", "", kept)
  if (keep_case_ar) {
    raw <- union(raw, case_covs)
  }
  unique(raw)
}

fit_brms_screen <- function(dat, covs_z, label) {
  rhs <- paste(covs_z, collapse = " + ")
  form <- stats::as.formula(paste0(
    "cases ~ ", rhs, " + offset(log_pop) + (1 | rne_iso_code)"
  ))
  message("Fitting brms horseshoe screen: ", label, " (", length(covs_z), " predictors)...")
  brms::brm(
    formula = form,
    data = dat,
    family = brms::negbinomial(),
    prior = c(
      brms::prior(horseshoe(df = 1, par_ratio = 0.1), class = "b"),
      brms::prior(student_t(3, 0, 2.5), class = "Intercept"),
      brms::prior(student_t(3, 0, 2.5), class = "sd")
    ),
    chains = 2,
    iter = 2000,
    warmup = 1000,
    cores = 2,
    seed = 42,
    silent = 2,
    refresh = 0
  )
}

# Climate-only screen
clim_z <- paste0(c(clim_covs, case_covs), "_z")
clim_z <- intersect(clim_z, names(panel_fit))
dat_ok <- panel_fit[stats::complete.cases(panel_fit[, c("cases", "log_pop", "rne_iso_code", clim_z)]), ]

fit_brms_clim <- fit_brms_screen(dat_ok, clim_z, "climate")
saveRDS(fit_brms_clim, file.path(out_dir, "brms_screen_climate.RDS"))
sel_clim <- select_important(fit_brms_clim)
readr::write_csv(
  tibble::tibble(model = "brms_climate", selected = sel_clim),
  file.path(out_dir, "brms_selected_climate.csv")
)

message("Fitting hybrid INLA from climate screen...")
fit_hy_clim <- fit_inla_count(panel_fit, covariates = sel_clim, graph = graph)
saveRDS(fit_hy_clim, file.path(out_dir, "inla_hybrid_from_brms_climate.RDS"))
diag_hy_clim <- extract_inla_diagnostics(fit_hy_clim, "inla_hybrid_brms_climate")

diag_hy_tr <- NULL
if (length(search_covs)) {
  tr_z <- paste0(c(clim_covs, case_covs, search_covs), "_z")
  tr_z <- intersect(tr_z, names(panel_fit))
  dat_tr <- panel_fit[stats::complete.cases(panel_fit[, c("cases", "log_pop", "rne_iso_code", tr_z)]), ]
  fit_brms_tr <- fit_brms_screen(dat_tr, tr_z, "climate_trends")
  saveRDS(fit_brms_tr, file.path(out_dir, "brms_screen_climate_trends.RDS"))
  sel_tr <- select_important(fit_brms_tr)
  readr::write_csv(
    tibble::tibble(model = "brms_climate_trends", selected = sel_tr),
    file.path(out_dir, "brms_selected_climate_trends.csv")
  )
  message("Fitting hybrid INLA from Trends screen...")
  fit_hy_tr <- fit_inla_count(panel_fit, covariates = sel_tr, graph = graph)
  saveRDS(fit_hy_tr, file.path(out_dir, "inla_hybrid_from_brms_trends.RDS"))
  diag_hy_tr <- extract_inla_diagnostics(fit_hy_tr, "inla_hybrid_brms_trends")
} else {
  message("No Trends columns — skipping Trends brms screen.")
}

diags <- dplyr::bind_rows(diag_hy_clim, diag_hy_tr)
readr::write_csv(diags, file.path(out_dir, "brms_hybrid_inla_diagnostics.csv"))
print(diags)
message("brms→INLA pipeline written to ", out_dir)
