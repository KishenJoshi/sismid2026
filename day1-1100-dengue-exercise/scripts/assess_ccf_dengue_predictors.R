# Cross-correlation (CCF) between weekly dengue cases and Google Trends predictors
# on the training window only (2015-2017). Lags are in weeks.
#
# Usage:
#   Rscript day1-1100-dengue-exercise/scripts/assess_ccf_dengue_predictors.R MX
#   Rscript day1-1100-dengue-exercise/scripts/assess_ccf_dengue_predictors.R BR

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(stringr)
  library(lubridate)
  library(patchwork)
  library(urca)
})

source("day1-1100-dengue-exercise/scripts/functions/load_most_recent_file.R")
source("day1-1100-dengue-exercise/scripts/functions/geo_weekly_helpers.R")
source("day1-1530-ai-agents-data-scraping/scripts/functions/signal_preprocess.R")

geo <- parse_geo_arg("MX")
dat <- load_weekly_dengue_geo(geo)
max_lag <- 12L

raw_df <- dat$dengue_df %>%
  transmute(
    start_date,
    y = dengue_total,
    dengue, mosquito, sintomas_de_dengue
  )

n_train <- sum(raw_df$start_date <= train_end)
train_df <- raw_df %>% filter(start_date <= train_end)

message("Geo: ", geo)
message(
  "Training window: ", min(train_df$start_date), " to ", max(train_df$start_date),
  " (", nrow(train_df), " weeks)"
)

preprocess_search_columns <- function(df, predictors, train_n) {
  proc_df <- dplyr::tibble(start_date = df$start_date)
  for (col in predictors) {
    one <- df %>%
      select(start_date, all_of(col)) %>%
      rename(date = start_date)
    den <- denoise_frame(one, window = 20L, spar = 0.8, max_knots = 3L)
    det <- detrend_frame(den, train_end = train_n)
    proc_df[[paste0(col)]] <- det$data[[col]]
  }
  out <- full_join(df %>% dplyr::select(start_date, dengue_total),
                   proc_df, by = "start_date")
  out
}

processed_full <- preprocess_search_columns(raw_df, predictors, n_train)
processed_train <- processed_full %>%
  filter(start_date <= train_end) %>%
  left_join(train_df %>% select(start_date, y), by = "start_date")

ccf_cases_lead_search <- function(cases, search, max_lag = 12L) {
  cases <- as.numeric(cases)
  search <- as.numeric(search)
  ok <- is.finite(cases) & is.finite(search)
  cases <- cases[ok]
  search <- search[ok]

  cc <- stats::ccf(
    cases, search,
    lag.max = max_lag,
    type = "correlation",
    plot = FALSE,
    na.action = na.omit
  )
  lags <- as.numeric(cc$lag)
  acf <- as.numeric(cc$acf)
  keep <- lags >= 0 & lags <= max_lag
  tibble(lag = lags[keep], ccf = acf[keep])
}

ccf_crit <- function(n) 1.96 / sqrt(n)

plot_ccf_panel <- function(ccf_df, n, title) {
  crit <- ccf_crit(n)
  ggplot(ccf_df, aes(x = lag, y = ccf)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_hline(yintercept = c(-crit, crit), linetype = "dashed", colour = "grey40") +
    geom_col(width = 0.7, fill = "#2c7fb8") +
    scale_x_continuous(breaks = seq(0, max_lag, by = 2)) +
    coord_cartesian(ylim = c(-1, 1)) +
    labs(
      title = title,
      subtitle = sprintf(
        "cases lead / search lags (weeks); max |ccf|=%.3f at lag %d; n=%d",
        max(abs(ccf_df$ccf), na.rm = TRUE),
        ccf_df$lag[which.max(abs(ccf_df$ccf))],
        n
      ),
      x = "Lag k (weeks): corr(cases[t+k], search[t])",
      y = "CCF"
    ) +
    theme_minimal()
}

out_dir <- file.path(
  "day1-1100-dengue-exercise/outputs/assess_ccf_dengue_predictors",
  geo
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

all_ccf <- list()
for (predictor in predictors) {
  message("CCF for predictor: ", predictor)

  raw_ccf <- ccf_cases_lead_search(train_df$y, train_df[[predictor]], max_lag) %>%
    mutate(geo = geo, predictor = predictor, processing = "raw")
  proc_ccf <- ccf_cases_lead_search(
    processed_train$y, processed_train[[predictor]], max_lag
  ) %>%
    mutate(geo = geo, predictor = predictor, processing = "processed")

  all_ccf[[length(all_ccf) + 1L]] <- bind_rows(raw_ccf, proc_ccf)

  n_raw <- sum(is.finite(train_df$y) & is.finite(train_df[[predictor]]))
  n_proc <- sum(is.finite(processed_train$y) & is.finite(processed_train[[predictor]]))

  fig <- (
    plot_ccf_panel(raw_ccf, n_raw, paste0(predictor, " — raw")) |
      plot_ccf_panel(proc_ccf, n_proc, paste0(predictor, " — processed"))
  ) +
    plot_annotation(
      title = paste0(geo, ": CCF dengue cases vs ", predictor, " (train 2015–2017)"),
      subtitle = paste0("Weekly lags 0–", max_lag, "; dashed ≈ 95% null bands")
    )

  ggsave(
    file.path(out_dir, paste0("ccf_", predictor, "_raw_vs_processed.png")),
    fig, width = 11, height = 4.5, dpi = 150
  )
}

ccf_long <- bind_rows(all_ccf)
write_csv(ccf_long, file.path(out_dir, "ccf_long.csv"))

summary_tbl <- ccf_long %>%
  group_by(geo, predictor, processing) %>%
  summarise(
    max_abs_ccf = max(abs(ccf), na.rm = TRUE),
    lag_at_max_abs = lag[which.max(abs(ccf))],
    ccf_at_lag0 = ccf[lag == 0][1],
    .groups = "drop"
  )
write_csv(summary_tbl, file.path(out_dir, "ccf_summary.csv"))
print(as.data.frame(summary_tbl), row.names = FALSE)
message("Wrote outputs under ", out_dir)
