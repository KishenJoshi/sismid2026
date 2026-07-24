# 08_evaluate_expanding_window_crps.R
# Real-world expanding-window: refit at each origin with all data through origin;
# forecast leads 1-6 months; score with CRPS; save INLA diagnostics.
#
# Parallelism: origins are independent → --workers N (default: min(4, detectCores()-1)).
# Each worker runs INLA single-threaded to avoid oversubscription.
#
# Usage:
#   Rscript scripts/08_evaluate_expanding_window_crps.R
#   Rscript scripts/08_evaluate_expanding_window_crps.R --workers 6
#   Rscript scripts/08_evaluate_expanding_window_crps.R --max-origins 2 --workers 2

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

args <- commandArgs(trailingOnly = TRUE)

max_origins <- Inf
if ("--max-origins" %in% args) {
  max_origins <- as.integer(args[[which(args == "--max-origins") + 1L]])
}

n_cores <- parallel::detectCores()
if (is.na(n_cores) || n_cores < 1L) n_cores <- 1L
default_workers <- max(1L, min(4L, n_cores - 1L))
workers <- default_workers
if ("--workers" %in% args) {
  workers <- as.integer(args[[which(args == "--workers") + 1L]])
  if (is.na(workers) || workers < 1L) workers <- 1L
}
workers <- min(workers, n_cores)

out_dir <- ensure_dir(DAY3_ROOT, "outputs", "evaluation")
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

# Selected covariates from brms screen if available; else use full sets
sel_clim_path <- file.path(DAY3_ROOT, "outputs/models/brms_selected_climate.csv")
sel_tr_path <- file.path(DAY3_ROOT, "outputs/models/brms_selected_climate_trends.csv")
sel_clim <- if (file.exists(sel_clim_path)) {
  readr::read_csv(sel_clim_path, show_col_types = FALSE)$selected
} else {
  c(clim_covs, case_covs)
}
sel_tr <- if (file.exists(sel_tr_path)) {
  readr::read_csv(sel_tr_path, show_col_types = FALSE)$selected
} else if (length(search_covs)) {
  c(clim_covs, case_covs, search_covs)
} else {
  character(0)
}

model_specs <- list(
  inla_climate = c(clim_covs, case_covs),
  inla_hybrid_climate = sel_clim
)
if (length(search_covs)) {
  model_specs$inla_trends <- c(clim_covs, case_covs, search_covs)
}
if (length(sel_tr)) {
  model_specs$inla_hybrid_trends <- sel_tr
}

# Expanding window origins over the longest recent continuous OpenDengue block.
# MX Admin1 weekly OpenDengue has gaps (no 2008-2011, 2014-2016); use 2017-2022.
# AR(24) needs ~2y burn-in within this block → origins from 2019-01.
min_origin <- as.Date("2019-01-01")
max_origin <- as.Date("2022-06-01")
origins <- seq.Date(min_origin, max_origin, by = "month")
panel_months <- sort(unique(panel$month_start))
origins <- origins[origins %in% panel_months]
if (is.finite(max_origins)) origins <- origins[seq_len(min(max_origins, length(origins)))]
if (!length(origins)) stop("No valid expanding-window origins in continuous data block")

leads <- 1:6
n_samples <- 100L

message(
  "Evaluating ", length(origins), " origins from ", min(origins), " to ", max(origins),
  " | models: ", paste(names(model_specs), collapse = ", "),
  " | workers: ", workers
)

#' Evaluate all models / leads for a single origin. Returns list(scores, diags).
eval_one_origin <- function(origin,
                            panel,
                            graph,
                            model_specs,
                            leads,
                            n_samples) {
  # One BLAS/INLA thread per worker when origins are parallelised
  INLA::inla.setOption(num.threads = "1:1")

  train <- panel %>% dplyr::filter(month_start <= origin)
  score_rows <- list()
  diag_rows <- list()

  for (mname in names(model_specs)) {
    covs <- model_specs[[mname]]
    fit <- tryCatch(
      fit_inla_count(train, covariates = covs, graph = graph),
      error = function(e) {
        message("  Fit failed for ", mname, " @ ", origin, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(fit)) next
    diag_rows[[length(diag_rows) + 1L]] <- extract_inla_diagnostics(fit, mname, origin)

    for (h in leads) {
      target_month <- seq.Date(origin, by = "month", length.out = h + 1L)[[h + 1L]]
      test <- panel %>% dplyr::filter(month_start == target_month)
      if (!nrow(test)) next

      pred_df <- test
      if (h > 1L) {
        origin_cases <- train %>%
          dplyr::filter(month_start == origin) %>%
          dplyr::select(rne_iso_code, cases_at_origin = cases)
        pred_df <- pred_df %>%
          dplyr::left_join(origin_cases, by = "rne_iso_code") %>%
          dplyr::mutate(cases_lag1 = dplyr::coalesce(cases_at_origin, cases_lag1))
      }

      fe <- fit$summary.fixed
      intercept <- fe["(Intercept)", "mean"]
      eta <- rep(intercept, nrow(pred_df))
      for (cn in setdiff(covs, character(0))) {
        if (cn %in% rownames(fe) && cn %in% names(pred_df)) {
          eta <- eta + fe[cn, "mean"] * pred_df[[cn]]
        }
      }
      spat <- fit$summary.random$loc_idx
      if (!is.null(spat)) {
        nloc <- nrow(spat) / 2
        spat_mean <- spat$mean[seq_len(nloc)]
        eta <- eta + spat_mean[pred_df$loc_idx]
      }
      eta <- eta + pred_df$log_pop
      mu <- exp(eta)
      theta <- fit$summary.hyperpar$mean[[1]]
      size <- max(as.numeric(theta), 1e-3)

      for (i in seq_len(nrow(pred_df))) {
        y <- pred_df$cases[[i]]
        if (is.na(y)) next
        draws <- stats::rnbinom(n_samples, size = size, mu = max(mu[[i]], 1e-8))
        score_rows[[length(score_rows) + 1L]] <- tibble::tibble(
          model = mname,
          origin = origin,
          lead = h,
          target_month = target_month,
          rne_iso_code = pred_df$rne_iso_code[[i]],
          observed = y,
          pred_mean = mean(draws),
          crps = crps_sample(y, draws)
        )
      }
    }
  }

  list(
    scores = dplyr::bind_rows(score_rows),
    diags = dplyr::bind_rows(diag_rows)
  )
}

if (workers > 1L) {
  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)) {
    stop("Parallel eval needs packages 'future' and 'future.apply'. Install them or use --workers 1.")
  }
  old_plan <- future::plan(future::multisession, workers = workers)
  on.exit(future::plan(old_plan), add = TRUE)
  results <- future.apply::future_lapply(
    origins,
    function(origin) {
      message("Origin ", origin)
      eval_one_origin(origin, panel, graph, model_specs, leads, n_samples)
    },
    future.seed = TRUE,
    future.packages = c("dplyr", "tibble", "INLA")
  )
} else {
  INLA::inla.setOption(num.threads = paste0(max(1L, n_cores - 1L), ":1"))
  results <- lapply(origins, function(origin) {
    message("Origin ", origin)
    eval_one_origin(origin, panel, graph, model_specs, leads, n_samples)
  })
}

scores <- dplyr::bind_rows(lapply(results, `[[`, "scores"))
diags <- dplyr::bind_rows(lapply(results, `[[`, "diags"))
readr::write_csv(scores, file.path(out_dir, "expanding_window_crps_scores.csv"))
readr::write_csv(diags, file.path(out_dir, "expanding_window_inla_diagnostics.csv"))

if (nrow(scores)) {
  summary_tbl <- scores %>%
    dplyr::group_by(model, lead) %>%
    dplyr::summarise(
      mean_crps = mean(crps, na.rm = TRUE),
      median_crps = median(crps, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  readr::write_csv(summary_tbl, file.path(out_dir, "crps_summary_by_model_lead.csv"))
  print(summary_tbl)
}

message("Evaluation written to ", out_dir, " (workers=", workers, ")")
