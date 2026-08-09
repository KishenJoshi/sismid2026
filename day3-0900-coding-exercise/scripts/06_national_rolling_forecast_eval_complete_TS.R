# 06_national_rolling_forecast_eval_complete_TS.R
# Rolling-window national forecast eval over the full usable panel.
# Initial / lookback window: first 4 years (48 months) of usable rows; then
# roll forward one month at a time. Horizon: 1-step ahead.
# Climate RW2/DLNM basis specs are re-estimated on each origin's training window.
#
# Leaves 06_national_rolling_forecast_eval.R and its results untouched.
#
# Metrics: CRPS, WIS, 95% coverage; CRPSS / WISS vs AR baseline.
# Diagnostics: INLA mode.status, CPO failures, mlik (no WAIC).
# Parallelism: --workers N over forecast origins.
#
# Usage (from repo root):
#   Rscript day3-0900-coding-exercise/scripts/06_national_rolling_forecast_eval_complete_TS.R
#   Rscript day3-0900-coding-exercise/scripts/06_national_rolling_forecast_eval_complete_TS.R --workers 4
#   Rscript day3-0900-coding-exercise/scripts/06_national_rolling_forecast_eval_complete_TS.R --max-origins 2 --workers 1

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
skip_plots <- "--skip-plots" %in% args
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

forecast_ndraws <- 400L
horizon <- 1L
window_months <- 48L
seed <- 20260809L

out_dir <- ensure_dir(
  "day3-0900-coding-exercise", "results",
  "06_national_rolling_forecast_eval_complete_TS"
)
plot_dir <- ensure_dir(out_dir, "plots")

rolling_train_dates <- function(dates, origin, window_months) {
  dates <- sort(unique(as.Date(dates)))
  origin <- as.Date(origin)
  i <- match(origin, dates)
  if (is.na(i) || i < as.integer(window_months)) {
    return(as.Date(character()))
  }
  dates[seq.int(i - as.integer(window_months) + 1L, i)]
}

#--------------- Data -----

panel_raw <- read_csv(
  "day3-0900-coding-exercise/data/mx_national_exposure_response_monthly.csv",
  show_col_types = FALSE
)
panel <- prepare_national_modelling_panel(panel_raw)

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
  "Usable panel: n=", nrow(panel_base),
  " [", min(panel_base$month_start), " .. ", max(panel_base$month_start), "]",
  " | rolling window=", window_months, " months"
)

priors <- define_national_pc_priors()
specs <- model_specs()

origins <- rolling_forecast_origins(
  panel_base$month_start,
  window_months = window_months,
  horizon = horizon,
  step = 1L
)
if (is.finite(max_origins)) {
  origins <- utils::head(origins, max_origins)
}
message(
  "n_origins=", length(origins),
  " [", min(origins), " .. ", max(origins), "]",
  " | models=", paste(names(specs), collapse = ", "),
  " | workers=", workers,
  " | ndraws=", forecast_ndraws
)
write_csv(
  tibble(
    origin = origins,
    window_months = window_months,
    train_start = vapply(
      origins,
      function(o) as.character(min(rolling_train_dates(panel_base$month_start, o, window_months))),
      character(1)
    ),
    train_end = as.character(origins)
  ),
  file.path(out_dir, "forecast_origins.csv")
)

#--------------- Per-origin worker -----

eval_one_origin <- function(origin,
                            panel_base,
                            specs,
                            priors,
                            horizon,
                            window_months,
                            forecast_ndraws) {
  if (!exists("fit_national_inla", mode = "function", inherits = TRUE)) {
    root <- getwd()
    source(file.path(root, "day3-0900-coding-exercise/scripts/functions/lag_helpers.R"), local = FALSE)
    source(file.path(root, "day3-0900-coding-exercise/scripts/functions/05_national_inla_helpers.R"), local = FALSE)
  }
  INLA::inla.setOption(num.threads = "1:1")
  origin <- as.Date(origin)
  train_dates <- rolling_train_dates(panel_base$month_start, origin, window_months)
  if (length(train_dates) < window_months) {
    return(list(scores = tibble(), diags = tibble()))
  }

  score_rows <- list()
  diag_rows <- list()

  for (model_name in names(specs)) {
    spec <- specs[[model_name]]
    method <- spec$climate_method

    train_raw <- panel_base %>%
      dplyr::filter(month_start %in% train_dates)

    climate_basis_specs <- list()
    if (identical(method, "rw2") || identical(method, "dlnm")) {
      clim_fit <- tryCatch(
        fit_climate_basis_spec(train_raw, method = method),
        error = function(e) {
          message(
            "Basis failed ", model_name, " @ ", origin, ": ",
            conditionMessage(e)
          )
          NULL
        }
      )
      if (is.null(clim_fit)) next
      climate_basis_specs[[method]] <- clim_fit
    }

    prep <- tryCatch(
      prepare_spec_frame(panel_base, spec, climate_basis_specs),
      error = function(e) {
        message("Prep failed ", model_name, " @ ", origin, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(prep)) next

    dat_all <- prep$data
    covs_raw <- prep$covs_raw
    is_rw2 <- identical(prep$climate_method, "rw2")
    ar_z_names <- paste0(ar_covariates(), "_z")

    train <- dat_all %>% dplyr::filter(month_start %in% train_dates)
    if (nrow(train) < 24L) next

    sc <- scale_params_from_train(train, covs_raw)
    train_z <- train
    for (col in covs_raw) {
      train_z[[paste0(col, "_z")]] <- (train_z[[col]] - sc$mu[[col]]) / sc$sd[[col]]
    }

    fit <- tryCatch(
      {
        if (is_rw2) {
          fit_national_inla_climate_rw2(
            dat = train_z,
            ar_covs_z = ar_z_names,
            temp_cols_z = paste0(prep$temp_cols, "_z"),
            precip_cols_z = paste0(prep$precip_cols, "_z"),
            priors = priors
          )
        } else {
          fit_national_inla(
            dat = train_z,
            covs_z = paste0(covs_raw, "_z")
          )
        }
      },
      error = function(e) {
        message("Fit failed ", model_name, " @ ", origin, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(fit)) next

    diag_rows[[length(diag_rows) + 1L]] <- extract_national_inla_diagnostics(
      fit, model_name, origin
    )

    panel_z <- dat_all
    for (col in covs_raw) {
      panel_z[[paste0(col, "_z")]] <- (panel_z[[col]] - sc$mu[[col]]) / sc$sd[[col]]
    }

    fc <- tryCatch(
      forecast_recursive_counts(
        fit = fit,
        panel = panel_z,
        origin = origin,
        covs_raw = covs_raw,
        scale_mu = sc$mu,
        scale_sd = sc$sd,
        horizon = horizon,
        ndraws = forecast_ndraws,
        climate_method = prep$climate_method,
        temp_cols_raw = prep$temp_cols,
        precip_cols_raw = prep$precip_cols
      ),
      error = function(e) {
        message("Forecast failed ", model_name, " @ ", origin, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(fc) || !nrow(fc)) next

    fc <- fc %>%
      dplyr::mutate(
        model = model_name,
        month_of_year = as.integer(format(target, "%m")),
        train_start = min(train_dates),
        train_end = origin,
        window_months = as.integer(window_months)
      )
    score_rows[[length(score_rows) + 1L]] <- fc
  }

  list(
    scores = dplyr::bind_rows(score_rows),
    diags = dplyr::bind_rows(diag_rows)
  )
}

#--------------- Run -----

set.seed(seed)

use_progress <- requireNamespace("progressr", quietly = TRUE)
if (!use_progress) {
  message("Install 'progressr' for a progress bar (optional).")
}

run_origins <- function(map_fun) {
  if (!use_progress) {
    return(map_fun(origins, function(origin) {
      message("Origin ", origin)
      eval_one_origin(
        origin, panel_base, specs, priors,
        horizon, window_months, forecast_ndraws
      )
    }))
  }
  progressr::handlers("txtprogressbar")
  progressr::with_progress({
    p <- progressr::progressor(along = origins)
    map_fun(origins, function(origin) {
      on.exit(p(sprintf("origin=%s", origin)), add = TRUE)
      eval_one_origin(
        origin, panel_base, specs, priors,
        horizon, window_months, forecast_ndraws
      )
    })
  })
}

if (workers > 1L) {
  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)) {
    stop("Parallel eval needs 'future' and 'future.apply'. Install or use --workers 1.")
  }
  old_plan <- future::plan(future::multisession, workers = workers)
  on.exit(future::plan(old_plan), add = TRUE)
  flapply <- function(X, FUN) {
    future.apply::future_lapply(
      X,
      FUN,
      future.seed = TRUE,
      future.packages = c(
        "dplyr", "tibble", "INLA", "splines",
        if (use_progress) "progressr"
      )
    )
  }
  results <- run_origins(flapply)
} else {
  INLA::inla.setOption(num.threads = paste0(max(1L, n_cores - 1L), ":1"))
  results <- run_origins(lapply)
}

scores <- bind_rows(lapply(results, `[[`, "scores"))
diags <- bind_rows(lapply(results, `[[`, "diags"))

write_csv(scores, file.path(out_dir, "forecast_scores_long.csv"))
write_csv(diags, file.path(out_dir, "fit_diagnostics_long.csv"))

if (!nrow(scores)) {
  message("No forecast scores produced.")
  quit(save = "no", status = 0)
}

summary_mh <- scores %>%
  group_by(model, horizon) %>%
  summarise(
    n = dplyr::n(),
    mean_crps = mean(crps, na.rm = TRUE),
    mean_wis = mean(wis, na.rm = TRUE),
    coverage_95 = mean(covered_95, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(summary_mh, file.path(out_dir, "metrics_summary_by_model_horizon.csv"))

crps_month <- summarise_crps_by_calendar_month(scores)
write_csv(crps_month, file.path(out_dir, "crps_by_calendar_month_horizon.csv"))

crpss <- summarise_crpss_vs_ar(scores, baseline = "ar")
write_csv(crpss, file.path(out_dir, "crpss_vs_ar_by_model_horizon.csv"))

message("\n=== Metrics by model × horizon ===")
print(as.data.frame(summary_mh), row.names = FALSE)
message("\n=== CRPSS / WISS vs AR ===")
print(as.data.frame(crpss), row.names = FALSE)

# Sensitivity: drop calendar-year 2020 targets
scores_excl_2020 <- scores %>%
  filter(as.integer(format(as.Date(target), "%Y")) != 2020L)
message(
  "\nExcl. 2020: n_scores=", nrow(scores_excl_2020),
  " / ", nrow(scores),
  " [", min(scores_excl_2020$target), " .. ", max(scores_excl_2020$target), "]"
)

summary_mh_excl_2020 <- scores_excl_2020 %>%
  group_by(model, horizon) %>%
  summarise(
    n = dplyr::n(),
    mean_crps = mean(crps, na.rm = TRUE),
    mean_wis = mean(wis, na.rm = TRUE),
    coverage_95 = mean(covered_95, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(
  summary_mh_excl_2020,
  file.path(out_dir, "metrics_summary_by_model_horizon_excl_2020.csv")
)

crps_month_excl_2020 <- summarise_crps_by_calendar_month(scores_excl_2020)
write_csv(
  crps_month_excl_2020,
  file.path(out_dir, "crps_by_calendar_month_horizon_excl_2020.csv")
)

crpss_excl_2020 <- summarise_crpss_vs_ar(scores_excl_2020, baseline = "ar")
write_csv(
  crpss_excl_2020,
  file.path(out_dir, "crpss_vs_ar_by_model_horizon_excl_2020.csv")
)

message("\n=== Metrics by model × horizon (excl. 2020) ===")
print(as.data.frame(summary_mh_excl_2020), row.names = FALSE)
message("\n=== CRPSS / WISS vs AR (excl. 2020) ===")
print(as.data.frame(crpss_excl_2020), row.names = FALSE)

mode_ok <- diags %>%
  mutate(mode_ok = mode_status == "0" | mode_status == "0,0" | grepl("^0", mode_status)) %>%
  group_by(model) %>%
  summarise(
    n_fits = dplyr::n(),
    n_mode_ok = sum(mode_ok, na.rm = TRUE),
    mean_cpo_fail = mean(cpo_fail, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(mode_ok, file.path(out_dir, "inla_mode_status_summary.csv"))
message("\n=== INLA mode.status summary ===")
print(as.data.frame(mode_ok), row.names = FALSE)

#--------------- Plots -----

title_span <- paste0(
  format(min(scores$target), "%Y-%m"), " – ", format(max(scores$target), "%Y-%m")
)

if (!isTRUE(skip_plots)) {
  ggsave(
    file.path(plot_dir, "crps_by_target_model_horizon.png"),
    plot_score_by_target(
      scores, "crps",
      paste0("National CRPS by target (rolling 4y; ", title_span, ")"),
      panel_df = panel_base
    ),
    width = 14, height = 12, dpi = 120
  )
  ggsave(
    file.path(plot_dir, "wis_by_target_model_horizon.png"),
    plot_score_by_target(
      scores, "wis",
      paste0("National WIS by target (rolling 4y; ", title_span, ")"),
      panel_df = panel_base
    ),
    width = 14, height = 12, dpi = 120
  )
  ggsave(
    file.path(plot_dir, "coverage95_by_target_model_horizon.png"),
    plot_score_by_target(
      scores, "covered_95",
      paste0("National 95% coverage by target (rolling 4y; ", title_span, ")")
    ),
    width = 14, height = 10, dpi = 120
  )
  ggsave(
    file.path(plot_dir, "crps_by_calendar_month_horizon.png"),
    plot_crps_by_calendar_month(
      crps_month,
      "Seasonal CRPS skill (calendar month × horizon; rolling 4y)",
      panel_df = panel_base
    ),
    width = 12, height = 12, dpi = 120
  )
  scores_fc <- scores %>%
    left_join(
      panel_base %>% dplyr::select(month_start, population),
      by = c("target" = "month_start")
    )
  ggsave(
    file.path(plot_dir, "forecast_timeseries_by_model_horizon.png"),
    plot_forecast_timeseries(
      scores_fc,
      paste0("National rolling 4y forecasts — ", title_span)
    ),
    width = 14, height = 10, dpi = 120
  )
  ggsave(
    file.path(plot_dir, "forecast_timeseries_by_model_horizon_log_incidence.png"),
    plot_forecast_timeseries(
      scores_fc,
      paste0("National rolling 4y forecasts — ", title_span, " (log incidence)"),
      y_scale = "log_incidence"
    ),
    width = 14, height = 10, dpi = 120
  )
  message("Plots written to ", plot_dir)
}

message("\nOutputs written to ", out_dir)
