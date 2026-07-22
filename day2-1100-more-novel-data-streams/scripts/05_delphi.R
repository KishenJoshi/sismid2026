# CMU Delphi Epidata: Facebook CTIS %CLI for Georgia.
#
# Mirrors notebooks/05_delphi_soln.ipynb.
#
# Usage (from repo root):
#   Rscript day2-1100-more-novel-data-streams/scripts/05_delphi.R
#
# Packages: dplyr, tidyr, readr, httr, jsonlite, ggplot2

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(httr)
  library(jsonlite)
  library(ggplot2)
})

root <- "day2-1100-more-novel-data-streams"
out_dir <- file.path(root, "outputs", "05_delphi")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

USER_AGENT <- "SISMID2026-course/1.0 (your-email@example.com)"
EPIDATA <- "https://api.delphi.cmu.edu/epidata/covidcast/"

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ===== EDIT for your own signal / place =====
SOURCE <- "fb-survey"
SIGNAL <- "smoothed_wcli" # % COVID-like illness, weighted
GEO_TYPE <- "state"
GEO <- "ga"
WINDOWS <- c(
  "20200901-20201231", "20210101-20210630",
  "20210701-20211231", "20220101-20220625"
)
# ============================================

cache_path <- function(fname) {
  candidates <- c(
    file.path(root, "data", fname),
    file.path("data", fname),
    fname
  )
  for (p in candidates) if (file.exists(p)) return(p)
  NULL
}

epidata <- function(...) {
  params <- list(...)
  url <- modify_url(EPIDATA, query = params)
  resp <- GET(url, user_agent(USER_AGENT), timeout(60))
  stop_for_status(resp)
  content(resp, as = "parsed", type = "application/json")$epidata %||% list()
}

# ---------------------------------------------------------------------------
# Pull FB survey CLI for GA
# ---------------------------------------------------------------------------

fb <- tryCatch(
  {
    rows <- list()
    for (w in WINDOWS) {
      got <- epidata(
        data_source = SOURCE, signal = SIGNAL, time_type = "day",
        geo_type = GEO_TYPE, geo_value = GEO, time_values = w
      )
      for (r in got) {
        rows[[length(rows) + 1L]] <- tibble(
          date = as.character(r$time_value),
          value = r$value,
          stderr = r$stderr %||% NA_real_,
          sample_size = r$sample_size %||% NA_real_
        )
      }
      Sys.sleep(0.8)
    }
    if (length(rows) < 1L) stop("no rows returned")
    bind_rows(rows)
  },
  error = function(e) {
    p <- cache_path("delphi_fb_survey_ga.csv")
    message("Live Epidata pull failed: ", conditionMessage(e), " -> cache ", p)
    read_csv(p, show_col_types = FALSE)
  }
)

fb <- fb %>%
  mutate(
    date = {
      raw <- as.character(date)
      parsed <- as.Date(raw, format = "%Y%m%d")
      need_iso <- is.na(parsed)
      if (any(need_iso)) parsed[need_iso] <- as.Date(raw[need_iso])
      parsed
    }
  ) %>%
  arrange(date)

message(nrow(fb), " days, ", min(fb$date, na.rm = TRUE), " -> ",
        max(fb$date, na.rm = TRUE))
message("mean sample size: ",
        format(round(mean(fb$sample_size, na.rm = TRUE), 0), big.mark = ","),
        " respondents/day")

# ---------------------------------------------------------------------------
# Plot with 95% CI when stderr present
# ---------------------------------------------------------------------------

p <- ggplot(fb, aes(date, value)) +
  geom_line(linewidth = 0.8, colour = "#2A6F97") +
  labs(
    title = paste0("Facebook symptom survey (CTIS) via CMU Delphi, ", toupper(GEO)),
    x = "date", y = "% COVID-like illness"
  ) +
  theme_minimal()

if (any(!is.na(fb$stderr))) {
  p <- p +
    geom_ribbon(
      aes(ymin = value - 1.96 * stderr, ymax = value + 1.96 * stderr),
      fill = "#2A6F97", alpha = 0.25, colour = NA
    ) +
    geom_line(linewidth = 0.8, colour = "#2A6F97")
}

ggsave(
  file.path(out_dir, "delphi_fb_cli_ga.png"),
  p, width = 11, height = 4, dpi = 120, bg = "white"
)

# ---------------------------------------------------------------------------
# Diagnostics + save
# ---------------------------------------------------------------------------

message("date range   : ", min(fb$date, na.rm = TRUE), " to ",
        max(fb$date, na.rm = TRUE))
message("%CLI range   : ", round(min(fb$value, na.rm = TRUE), 2), " to ",
        round(max(fb$value, na.rm = TRUE), 2))
message("missing      :")
print(colSums(is.na(fb[c("value", "stderr", "sample_size")])))
peak_i <- which.max(fb$value)
message("peak week    : ", fb$date[[peak_i]], " -> ",
        round(fb$value[[peak_i]], 2), " % CLI")

out_csv <- file.path(out_dir, "delphi_fb_survey.csv")
write_csv(fb, out_csv)
message("saved ", out_csv)

# ---------------------------------------------------------------------------
# Quick probe of available sources
# ---------------------------------------------------------------------------

for (pair in list(
  c("fb-survey", "smoothed_wcli"),
  c("doctor-visits", "smoothed_adj_cli")
)) {
  tryCatch(
    {
      got <- epidata(
        data_source = pair[[1]], signal = pair[[2]], time_type = "day",
        geo_type = "state", geo_value = "ga", time_values = "20210101-20210107"
      )
      message(sprintf("%-16s %-20s -> %d rows in that week",
                      pair[[1]], pair[[2]], length(got)))
    },
    error = function(e) {
      message(sprintf("%-16s probe failed: %s", pair[[1]], conditionMessage(e)))
    }
  )
  Sys.sleep(0.8)
}
