# Simulate real-world expanding-window deployment of weekly AR / ARX dengue forecasts.
#
# Protocol:
#   1. Start with the first `initial_train_weeks` (default 104 ≈ 2 years).
#   2. At each origin, refit models and issue a recursive 8-week forecast.
#   3. Advance one week: the newly observed week is appended to training.
#   4. Repeat until fewer than 8 weeks remain after the origin.
#
# Error propagation (recursive):
#   Horizon 1 uses only observed case history.
#   Horizon h>1 replaces unknown future case lags with earlier predictions ŷ,
#   so forecast error feeds into later steps and typically grows with lead.
#   For ARX, Google Trends is treated as available in real time: observed
#   search values are used at each horizon (only cases are recursed).
#
# Usage (from repo root):
#   Rscript day1-1100-dengue-exercise/scripts/simulate_ar_deployment.R MX
#   Rscript day1-1100-dengue-exercise/scripts/simulate_ar_deployment.R BR
#
# Outputs (under outputs/simulate_ar_deployment/{GEO}/):
#   forecasts_long.csv
#     One row per origin × lead × model config.
#     Columns: geo, origin_week, prediction_week, prediction_lead, actual,
#     predicted, APE, model, predictor, processing, max_lag, train_n
#     (R2 omitted here — needs a set of points, not a single forecast.)
#   metrics_by_lead.csv
#     R2 and MAPE for each prediction_lead (1..8), pooling all origins
#     for that lead within each model config.
#   metrics_by_origin.csv
#     R2 and MAPE for each origin_week over its 8-lead forecast window
#     (handy for tracking deployment skill over time).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(urca)
})

source("day1-1100-dengue-exercise/scripts/functions/load_most_recent_file.R")
source("day1-1100-dengue-exercise/scripts/functions/geo_weekly_helpers.R")
source("day1-1100-dengue-exercise/scripts/functions/ar_deployment_functions.R")
source("day1-1530-ai-agents-data-scraping/scripts/functions/signal_preprocess.R")

geo <- parse_geo_arg("MX")
max_lags <- 1:3
horizon <- 8L
initial_train_weeks <- 104L

out_dir <- file.path(
  "day1-1100-dengue-exercise/outputs/simulate_ar_deployment",
  geo
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------

gtrends_data <- load_most_recent_file(
  "day1-1100-dengue-exercise/downloads",
  "googletrends_dengue.csv",
  geo = geo
) %>%
  mutate(start_date = as.Date(start_date))

case_data <- load_most_recent_file(
  "day1-1100-dengue-exercise/downloads",
  paste0("dengue_cases_", geo, "_weekly.csv"),
  geo = NULL
) %>%
  mutate(start_date = as.Date(start_date))

combined_dat <- left_join(gtrends_data, case_data, by = "start_date") %>%
  filter(!is.na(dengue_total))

n <- nrow(combined_dat)
if (n < initial_train_weeks + horizon) {
  stop(
    "Series too short for deployment sim: n=", n,
    " needs >= ", initial_train_weeks + horizon
  )
}

# Freeze preprocessing params on the initial 2-year window (deployment-realistic:
# only information available at the first origin is used for trend/denoise fits).
processed_df <- preprocess_search_columns(
  combined_dat,
  predictors,
  train_n = initial_train_weeks
)

message(
  "Geo: ", geo,
  " | weeks: ", n,
  " | initial train: ", initial_train_weeks,
  " | horizon: ", horizon
)
message(
  "Date range: ", min(combined_dat$start_date), " to ", max(combined_dat$start_date),
  " | origins: ", n - initial_train_weeks - horizon + 1L
)

# ---------------------------------------------------------------------------
# Expanding-window simulation
# ---------------------------------------------------------------------------

configs <- tidyr::expand_grid(
  processing = c("raw", "processed"),
  model = c("ar", "arx"),
  predictor = predictors,
  max_lag = max_lags
) %>%
  # AR-only does not depend on predictor; keep one AR row per processing/lag
  filter(!(model == "ar" & predictor != predictors[[1]])) %>%
  mutate(
    predictor = if_else(model == "ar", NA_character_, predictor)
  )

origins <- seq.int(initial_train_weeks, n - horizon)

all_forecasts <- list()
k <- 0L

for (oi in seq_along(origins)) {
  origin_idx <- origins[[oi]]
  origin_week <- combined_dat$start_date[[origin_idx]]
  train_n <- origin_idx

  if (oi == 1L || oi %% 25L == 0L) {
    message(
      "Origin ", oi, "/", length(origins),
      " | ", origin_week,
      " | train_n=", train_n
    )
  }

  for (ci in seq_len(nrow(configs))) {
    cfg <- configs[ci, ]
    p <- as.integer(cfg$max_lag)

    search_df <- if (identical(cfg$processing, "raw")) combined_dat else processed_df
    y_train <- search_df$dengue_total[seq_len(train_n)]
    y_future <- search_df$dengue_total[(train_n + 1L):(train_n + horizon)]
    dates_future <- search_df$start_date[(train_n + 1L):(train_n + horizon)]

    fit <- tryCatch(
      {
        if (identical(cfg$model, "ar")) {
          fit_ar(y_train, p)
        } else {
          x_all <- search_df[[cfg$predictor]]
          fit_arx(y_train, x_all[seq_len(train_n)], p)
        }
      },
      error = function(e) {
        warning(
          "Fit failed at ", origin_week, " / ",
          cfg$model, " / ", cfg$processing, " / lag=", p,
          ": ", conditionMessage(e)
        )
        NULL
      }
    )
    if (is.null(fit)) next

    x_all <- if (identical(cfg$model, "arx")) {
      search_df[[cfg$predictor]]
    } else {
      NULL
    }

    preds <- tryCatch(
      recursive_forecast(y_train, x_all = x_all, fit = fit, horizon = horizon),
      error = function(e) {
        warning("Forecast failed at ", origin_week, ": ", conditionMessage(e))
        rep(NA_real_, horizon)
      }
    )

    k <- k + 1L
    all_forecasts[[k]] <- tibble(
      geo = geo,
      origin_week = origin_week,
      prediction_week = dates_future,
      prediction_lead = seq_len(horizon),
      actual = y_future,
      predicted = as.numeric(preds),
      APE = ape(y_future, preds),
      model = cfg$model,
      predictor = cfg$predictor,
      processing = cfg$processing,
      max_lag = p,
      train_n = train_n
    )
  }
}

forecasts_long <- bind_rows(all_forecasts)
stopifnot(nrow(forecasts_long) > 0L)

# ---------------------------------------------------------------------------
# Secondary metrics tables
# ---------------------------------------------------------------------------

# R2 / MAPE for each lead, pooling every origin that forecast that lead
metrics_by_lead <- forecasts_long %>%
  group_by(geo, model, predictor, processing, max_lag, prediction_lead) %>%
  summarise(
    n_origins = sum(is.finite(actual) & is.finite(predicted)),
    R2 = r2_score(actual, predicted),
    MAPE = mape(actual, predicted),
    .groups = "drop"
  ) %>%
  arrange(model, predictor, processing, max_lag, prediction_lead)

# R2 / MAPE for each origin over its 8-week forecast path
metrics_by_origin <- forecasts_long %>%
  group_by(
    geo, origin_week, model, predictor, processing, max_lag, train_n
  ) %>%
  summarise(
    n_leads = sum(is.finite(actual) & is.finite(predicted)),
    R2 = r2_score(actual, predicted),
    MAPE = mape(actual, predicted),
    .groups = "drop"
  ) %>%
  arrange(origin_week, model, predictor, processing, max_lag)

# ---------------------------------------------------------------------------
# Write
# ---------------------------------------------------------------------------

write_csv(forecasts_long, file.path(out_dir, "forecasts_long.csv"))
write_csv(metrics_by_lead, file.path(out_dir, "metrics_by_lead.csv"))
write_csv(metrics_by_origin, file.path(out_dir, "metrics_by_origin.csv"))

message("Wrote ", nrow(forecasts_long), " forecast rows -> ", file.path(out_dir, "forecasts_long.csv"))
message("Wrote metrics_by_lead (", nrow(metrics_by_lead), " rows)")
message("Wrote metrics_by_origin (", nrow(metrics_by_origin), " rows)")
message("Done.")
