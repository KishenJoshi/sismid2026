# Google Trends: dengue (MX) + term-vs-topic flu comparison (gtrendsR path).
#
# Mirrors notebooks/00_google_trends_api_soln.ipynb. Uses gtrendsR instead of
# the official Trends API; live pull with cached fallback.
#
# Usage (from repo root):
#   Rscript day2-0900-data-beyond-google-trends/scripts/00_google_trends_api.R
#
# Packages: gtrendsR, dplyr, tidyr, readr, httr, jsonlite, ggplot2

suppressPackageStartupMessages({
  library(gtrendsR)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(httr)
  library(jsonlite)
  library(ggplot2)
})

root <- "day2-0900-data-beyond-google-trends"
source("day1-1530-ai-agents-data-scraping/scripts/functions/gt_helpers.R")

out_dir <- file.path(root, "outputs", "00_google_trends_api")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

USE_CACHE_ONLY <- tolower(Sys.getenv("USE_CACHE_ONLY", "false")) %in%
  c("1", "true", "t", "yes")

dengue_cache <- c(
  file.path(root, "data/gt_api_dengue_mx_cached.csv"),
  "data/gt_api_dengue_mx_cached.csv",
  "gt_api_dengue_mx_cached.csv"
)
tvt_cache <- c(
  file.path(root, "data/gt_api_flu_term_vs_topic_cached.csv"),
  "data/gt_api_flu_term_vs_topic_cached.csv",
  "gt_api_flu_term_vs_topic_cached.csv"
)

# ---------------------------------------------------------------------------
# 1. Dengue terms in Mexico
# ---------------------------------------------------------------------------

TERMS <- c("dengue", "sintomas de dengue", "mosquito")
GEO <- "MX"
timeframe_dengue <- "2021-07-01 2026-07-01"

if (isTRUE(USE_CACHE_ONLY)) {
  message("USE_CACHE_ONLY=TRUE; loading dengue cache.")
  dengue <- load_cache(dengue_cache)
  src <- "cached snapshot"
} else {
  message("Pulling Google Trends (gtrendsR): ", paste(TERMS, collapse = ", "),
          " | geo=", GEO)
  dengue <- gt_fetch(
    kw_list = TERMS,
    timeframe = timeframe_dengue,
    geo = GEO,
    query_type = "term"
  )
  if (is.null(dengue)) {
    message("Live pull failed; falling back to cache.")
    dengue <- load_cache(dengue_cache)
    src <- "cached snapshot"
  } else {
    src <- "live gtrendsR"
  }
}

cols <- setdiff(names(dengue), "date")
gap <- median(diff(as.numeric(dengue$date)), na.rm = TRUE)
freq_lab <- if (is.finite(gap) && gap <= 10) "weekly" else "monthly"
message(
  src, ": ", nrow(dengue), " points (", freq_lab, "), ",
  min(dengue$date), " -> ", max(dengue$date)
)

dengue_long <- dengue %>%
  pivot_longer(all_of(cols), names_to = "series", values_to = "hits")

p_dengue <- ggplot(dengue_long, aes(date, hits, colour = series)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Google Trends (gtrendsR): dengue in Mexico",
    x = NULL, y = "interest (0-100)", colour = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(
  file.path(out_dir, "dengue_mx_trends.png"),
  p_dengue, width = 11, height = 4, dpi = 120, bg = "white"
)

# ---------------------------------------------------------------------------
# 2. Reproducibility check (gtrendsR is sampling-based; may not match)
# ---------------------------------------------------------------------------

if (!isTRUE(USE_CACHE_ONLY)) {
  a <- gt_fetch("dengue", timeframe = "2024-01-01 2026-07-01", geo = "MX")
  b <- gt_fetch("dengue", timeframe = "2024-01-01 2026-07-01", geo = "MX")
  if (!is.null(a) && !is.null(b)) {
    identical_pulls <- isTRUE(all.equal(a[[2]], b[[2]], tolerance = 1e-9))
    message("two gtrendsR pulls identical? ", identical_pulls)
    message("  (official Trends API is byte-stable; gtrendsR often is not)")
  } else {
    message("Could not complete live reproducibility check (rate-limited?).")
  }
}

# ---------------------------------------------------------------------------
# 3. Term "flu" vs topic /m/0cycc across countries
# ---------------------------------------------------------------------------

geo_labels <- c(
  US = "United States", FR = "France", IT = "Italy", MX = "Mexico"
)
timeframe_flu <- "2022-01-01 2026-07-01"

if (isTRUE(USE_CACHE_ONLY)) {
  tvt <- load_cache(tvt_cache)
} else {
  frames <- list()
  for (g in names(geo_labels)) {
    message("Term vs topic for ", geo_labels[[g]], " (", g, ")")
    # One call: raw term + topic mid
    cframe <- tryCatch(
      {
        res <- gtrendsR::gtrends(
          keyword = c("flu", "/m/0cycc"),
          geo = g,
          time = normalize_timeframe(timeframe_flu),
          onlyInterest = TRUE
        )
        iot <- res$interest_over_time
        if (is.null(iot) || nrow(iot) < 1L) stop("empty interest_over_time")
        iot %>%
          transmute(
            date = as.Date(date),
            keyword = as.character(keyword),
            hits = parse_hits(hits)
          ) %>%
          group_by(date, keyword) %>%
          summarise(hits = mean(hits, na.rm = TRUE), .groups = "drop") %>%
          mutate(
            series = dplyr::case_when(
              grepl("flu", keyword, ignore.case = TRUE) &
                !grepl("^/m/", keyword) ~ "term_flu",
              grepl("^/m/", keyword) | grepl("Influenza", keyword, ignore.case = TRUE)
              ~ "topic_influenza",
              TRUE ~ norm_col(keyword)
            )
          ) %>%
          select(date, series, hits) %>%
          pivot_wider(names_from = series, values_from = hits) %>%
          mutate(geo = geo_labels[[g]])
      },
      error = function(e) {
        message("  failed: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(cframe)) frames[[g]] <- cframe
    Sys.sleep(3)
  }
  if (length(frames) > 0L) {
    tvt <- bind_rows(frames)
  } else {
    message("All live term-vs-topic pulls failed -> cache")
    tvt <- load_cache(tvt_cache)
  }
}

summary_tbl <- tvt %>%
  group_by(geo) %>%
  summarise(
    term_max = round(max(term_flu, na.rm = TRUE), 1),
    term_mean = round(mean(term_flu, na.rm = TRUE), 1),
    topic_max = round(max(topic_influenza, na.rm = TRUE), 1),
    topic_mean = round(mean(topic_influenza, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(summary_tbl)
write_csv(summary_tbl, file.path(out_dir, "flu_term_vs_topic_summary.csv"))

p_tvt <- ggplot(tvt, aes(x = date)) +
  geom_line(aes(y = term_flu, colour = 'term "flu"'), linewidth = 0.6) +
  geom_line(aes(y = topic_influenza, colour = "topic /m/0cycc"), linewidth = 0.6) +
  facet_wrap(~geo, ncol = 2) +
  coord_cartesian(ylim = c(0, 105)) +
  labs(
    title = "English term is nearly silent abroad; the topic sees the season everywhere",
    x = NULL, y = "interest (0-100)", colour = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(
  file.path(out_dir, "flu_term_vs_topic.png"),
  p_tvt, width = 12, height = 6, dpi = 120, bg = "white"
)

# ---------------------------------------------------------------------------
# 4. Save dengue series
# ---------------------------------------------------------------------------

out_csv <- file.path(out_dir, "gt_api_dengue_mx.csv")
write_csv(dengue, out_csv)
message("saved ", out_csv, " | ", nrow(dengue), " rows, ",
        paste(names(dengue), collapse = ", "))
write_csv(tvt, file.path(out_dir, "gt_api_flu_term_vs_topic.csv"))
message("outputs in ", out_dir)
