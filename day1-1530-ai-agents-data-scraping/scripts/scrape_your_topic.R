# Scrape Google Trends for your own topic (R), then pre-process the signal.
#
# Mirrors day1-1530 Lane A/B: fetch (term or topic) -> tidy -> pre-process ->
# assess -> save. Default example topic: flu in the US (cached fallback).
#
# Usage (from repo root):
#   Rscript day1-1530-ai-agents-data-scraping/scripts/scrape_your_topic.R
#
# Packages: gtrendsR, dplyr, tidyr, readr, httr, jsonlite, urca, zoo, ggplot2

suppressPackageStartupMessages({
  library(gtrendsR)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(httr)
  library(jsonlite)
  library(urca)
  library(zoo)
  library(ggplot2)
})

root <- "day1-1530-ai-agents-data-scraping"
source("day1-1530-ai-agents-data-scraping/scripts/functions/gt_helpers.R")
source("day1-1530-ai-agents-data-scraping/scripts/functions/signal_preprocess.R")
source("day1-1530-ai-agents-data-scraping/scripts/functions/assess_performance.R")

# ===== EDIT THESE for your own topic =====
MY_TERMS <- c("influenza", "flu", "fever") # phrases; additive e.g. "flu + gripe" ok
MY_GEO <- "US" # e.g. "US", "MX", "US-GA", "BR"
# "term" = raw / additive strings; "topic" = resolve each phrase to a mid
QUERY_TYPE <- "term"
RECENT_TF <- "today 5-y"
# Set TRUE to skip the live Google Trends pull (use cached CSV only).
# Or: USE_CACHE_ONLY=TRUE Rscript .../scrape_your_topic.R
USE_CACHE_ONLY <- tolower(Sys.getenv("USE_CACHE_ONLY", "false")) %in%
  c("1", "true", "t", "yes")
# ========================================

CACHE_PATHS <- c(
  file.path(root, "data/google_trends_flu_us_cached.csv"),
  "data/google_trends_flu_us_cached.csv",
  "google_trends_flu_us_cached.csv"
)

out_dir <- file.path(root, "outputs", "scrape_your_topic")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# 1. Live pull (term or topic); fall back to cache if blocked
# ---------------------------------------------------------------------------

live_ok <- FALSE
if (isTRUE(USE_CACHE_ONLY)) {
  message("USE_CACHE_ONLY=TRUE; loading cached example.")
  series <- load_cache(CACHE_PATHS)
} else {
  message(
    "Pulling Google Trends [", QUERY_TYPE, "]: ",
    paste(MY_TERMS, collapse = ", "), " | geo=", MY_GEO
  )
  series <- gt_fetch(
    kw_list = MY_TERMS,
    timeframe = RECENT_TF,
    geo = MY_GEO,
    query_type = QUERY_TYPE
  )
  if (is.null(series)) {
    message("Live pull failed; falling back to cached example.")
    series <- load_cache(CACHE_PATHS)
  } else {
    live_ok <- TRUE
  }
}

cols <- setdiff(names(series), "date")
message(
  "rows: ", nrow(series),
  " | last data point: ", max(series$date),
  " | columns: ", paste(cols, collapse = ", ")
)

# Optional: additive combined query (topic-like OR) when using raw terms
if (live_ok && identical(QUERY_TYPE, "term") && length(MY_TERMS) > 1L) {
  combo_query <- paste(MY_TERMS, collapse = " + ")
  message("Also pulling additive query: ", combo_query)
  combo <- gt_fetch(
    kw_list = combo_query,
    timeframe = RECENT_TF,
    geo = MY_GEO,
    query_type = "term"
  )
  if (!is.null(combo)) {
    combo_col <- setdiff(names(combo), "date")[1]
    series <- series %>%
      left_join(
        combo %>% rename(additive_combined = !!sym(combo_col)),
        by = "date"
      )
  }
}

# ---------------------------------------------------------------------------
# 2. Sanity check on the raw scrape
# ---------------------------------------------------------------------------

message("--- Sanity check (raw) ---")
message("geo: ", MY_GEO)
message("date range: ", min(series$date), " to ", max(series$date))
message("rows: ", nrow(series))
value_names <- setdiff(names(series), "date")
for (col in value_names) {
  message(
    "  ", col,
    ": missing=", sum(is.na(series[[col]])),
    ", sd=", signif(sd(series[[col]], na.rm = TRUE), 4),
    ", varies=", isTRUE(sd(series[[col]], na.rm = TRUE) > 0)
  )
}

raw_path <- file.path(out_dir, "my_topic_search.csv")
write_csv(series, raw_path)
message("Wrote raw table: ", raw_path)

# ---------------------------------------------------------------------------
# 3. Pre-process: aggregate related terms -> spline denoise -> detrend
# ---------------------------------------------------------------------------

# Aggregate only the original search terms (not the optional additive column)
term_cols <- intersect(norm_col(MY_TERMS), names(series))
if (!length(term_cols)) {
  # cache / rename edge case: use all non-additive value columns
  term_cols <- setdiff(names(series), c("date", "additive_combined"))
}

raw_terms <- series %>% select(date, all_of(term_cols))

pipeline <- preprocess_trends(
  raw_terms,
  n_clusters = NULL, # correlation-distance cut; or set e.g. 2
  denoise_window = 20L,
  spar = 0.8,
  max_knots = 3L,
  train_ratio = 0.7
)

message(
  "Term clusters: ",
  paste(names(pipeline$cluster_map), "=", pipeline$cluster_map, collapse = "; ")
)

# ---------------------------------------------------------------------------
# 4. Assess pre-processing performance
# ---------------------------------------------------------------------------

assessment <- assess_preprocessing(pipeline)
print_assessment(assessment)

metrics_path <- file.path(out_dir, "preprocessing_metrics.csv")
agg_metrics <- tibble(
  step = "aggregation",
  series = NA_character_,
  metric = c("mean_pct_zeros_before", "mean_pct_zeros_after"),
  value = c(
    assessment$aggregation$mean_pct_zeros_before,
    assessment$aggregation$mean_pct_zeros_after
  ),
  detail = NA_character_
)
den_metrics <- assessment$denoising %>%
  pivot_longer(cols = -series, names_to = "metric", values_to = "snr") %>%
  mutate(step = "denoising", detail = NA_character_) %>%
  select(step, series, metric, snr, detail)
det_metrics <- assessment$detrending %>%
  pivot_longer(
    cols = c(r2_trend_before, r2_trend_after),
    names_to = "metric",
    values_to = "r2"
  ) %>%
  transmute(
    step = "detrending",
    series,
    metric,
    r2,
    detail = trend_kind
  )
bind_rows(agg_metrics, den_metrics, det_metrics) %>%
  write_csv(metrics_path)
message("Wrote metrics: ", metrics_path)

# ---------------------------------------------------------------------------
# 5. Save processed data
# ---------------------------------------------------------------------------

processed_path <- file.path(out_dir, "my_topic_search_processed.csv")
write_csv(pipeline$processed, processed_path)
message("Wrote processed table: ", processed_path)

# Also keep intermediate stages for inspection
write_csv(
  pipeline$aggregated,
  file.path(out_dir, "my_topic_search_aggregated.csv")
)
write_csv(
  pipeline$denoised,
  file.path(out_dir, "my_topic_search_denoised.csv")
)

# ---------------------------------------------------------------------------
# 6. Plot each pre-processing stage on the same axes
# ---------------------------------------------------------------------------
# Stages: initial (raw) -> aggregated -> denoised -> detrended -> final.
# "detrended" / "final" are the detrend-step output (formerly "processed").

proc_cols <- setdiff(names(pipeline$processed), "date")
stage_order <- c("initial", "aggregated", "denoised", "detrended", "final")

stage_long <- function(df, stage) {
  df %>%
    select(date, all_of(intersect(proc_cols, names(df)))) %>%
    pivot_longer(-date, names_to = "series", values_to = "value") %>%
    mutate(stage = stage, line_id = paste(stage, series, sep = "::"))
}

# Map each processed column back to its raw member term(s).
members_for <- function(proc_col, cluster_map) {
  if (proc_col %in% names(cluster_map)) {
    return(proc_col)
  }
  if (grepl("^cluster_[0-9]+__", proc_col)) {
    members_str <- sub("^cluster_[0-9]+__", "", proc_col)
    return(strsplit(members_str, "+", fixed = TRUE)[[1]])
  }
  for (id in unique(cluster_map)) {
    members <- names(cluster_map)[cluster_map == id]
    label <- if (length(members) == 1L) {
      members[[1]]
    } else {
      paste0("cluster_", id, "__", paste(members, collapse = "+"))
    }
    if (identical(label, proc_col)) {
      return(members)
    }
  }
  character()
}

# Initial: each raw component term, drawn separately on its processed-series panel.
initial_rows <- list()
for (col in proc_cols) {
  members <- intersect(members_for(col, pipeline$cluster_map), names(pipeline$raw))
  for (m in members) {
    initial_rows[[length(initial_rows) + 1L]] <- tibble(
      date = pipeline$raw$date,
      series = col,
      value = as.numeric(pipeline$raw[[m]]),
      stage = "initial",
      line_id = paste("initial", col, m, sep = "::")
    )
  }
}

compare_df <- bind_rows(
  bind_rows(initial_rows),
  stage_long(pipeline$aggregated, "aggregated"),
  stage_long(pipeline$denoised, "denoised"),
  stage_long(pipeline$processed, "detrended"),
  stage_long(pipeline$processed, "final")
) %>%
  filter(!is.na(value)) %>%
  mutate(stage = factor(stage, levels = stage_order))

p_all <- ggplot(
  compare_df,
  aes(x = date, y = value, colour = stage, group = line_id)
) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  facet_wrap(~series, scales = "free_y", ncol = 1) +
  labs(
    x = "Date",
    y = "Value",
    colour = "Stage",
    title = "Google Trends: pre-processing stages"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

fig_all <- file.path(out_dir, "processed_vs_unprocessed.png")
ggsave(
  fig_all,
  plot = p_all,
  width = 9,
  height = max(3.5, 2.8 * length(proc_cols)),
  dpi = 150
)
message("Saved figure: ", fig_all)

for (s in proc_cols) {
  p_s <- ggplot(
    filter(compare_df, series == s),
    aes(x = date, y = value, colour = stage, group = line_id)
  ) +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    labs(
      x = "Date",
      y = "Value",
      colour = "Stage",
      title = paste0("Pre-processing stages: ", s)
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  safe_name <- gsub("[^A-Za-z0-9._-]+", "_", s)
  fig_s <- file.path(out_dir, paste0("processed_vs_unprocessed_", safe_name, ".png"))
  ggsave(fig_s, plot = p_s, width = 9, height = 4.5, dpi = 150)
  message("Saved figure: ", fig_s)
}

message("Done.")
