# 04_download_gtrends_topics_adm1.R
# Scrape Google Trends one topic/term × one MX-XX geo at a time (independent 0-100).
#
# Usage (from day3 root):
#   Rscript scripts/04_download_gtrends_topics_adm1.R
#   Rscript scripts/04_download_gtrends_topics_adm1.R --max-queries 5   # smoke test
#
# Resume-safe: skips existing CSV files in today's downloads folder.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(httr)
  library(jsonlite)
  library(gtrendsR)
})

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
source(file.path(root, "scripts/functions/paths.R"))
source(file.path(root, "scripts/functions/week_to_month.R"))
source(file.path(root, "scripts/functions/gt_topic_adm1_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
max_queries <- Inf
if ("--max-queries" %in% args) {
  i <- which(args == "--max-queries")
  max_queries <- as.integer(args[[i + 1L]])
}

# Topics (language-agnostic mids) + Spanish literal control terms.
topic_queries <- c(
  "Aedes",
  "Aedes aegypti",
  "mosquito",
  "vector",
  "vector control",
  "space spraying",
  "dengue",
  "dengue control",
  "standing water",
  "source management",
  "insecticide",
  "fogging",
  "larvicide",
  "fumigation",
  "breeding site",
  "insect repellent",
  "epidemic",
  "public health",
  "water storage"
)
term_queries <- c(
  "control del dengue",
  "control de vectores"
)

# Long window → monthly Trends resolution from Google; covers OpenDengue era.
# Note: some sparse adm1 × topic pairs return no data; those geos are skipped.
timeframe <- "2004-01-01 2022-12-31"
# Fallback shorter window if long query returns empty (still monthly for >5y).
timeframe_fallback <- "2015-01-01 2022-12-31"
# Conservative pacing after prior 429 rate-limits from parallel/faster runs.
sleep_sec <- 20

# Resume into the dated folder that already has CSVs (avoid a new empty Sys.Date() dir).
dl_root <- file.path(DAY3_ROOT, "downloads")
existing_dirs <- list.dirs(dl_root, full.names = TRUE, recursive = FALSE)
csv_counts <- vapply(
  existing_dirs,
  function(d) length(list.files(d, pattern = "^gtrends_.*\\.csv$")),
  integer(1)
)
if (length(existing_dirs) && any(csv_counts > 0L)) {
  out_dir <- existing_dirs[[which.max(csv_counts)]]
} else {
  out_dir <- ensure_dir(DAY3_ROOT, "downloads", as.character(Sys.Date()))
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
meta_path <- file.path(out_dir, "topic_resolution_log.csv")
meta_rows <- list()

jobs <- dplyr::bind_rows(
  tibble::tibble(
    query = topic_queries,
    query_type = "topic",
    canon = vapply(topic_queries, norm_col, character(1))
  ),
  tibble::tibble(
    query = term_queries,
    query_type = "term",
    canon = vapply(term_queries, norm_col, character(1))
  )
)

geos <- MX_STATES_32
n_done <- 0L
n_skip <- 0L
n_fail <- 0L
n_skip_nodata <- 0L

message(
  "Sequential download | sleep=", sleep_sec, "s",
  " | resume-safe skip of existing CSVs + .nodata markers | out=", out_dir
)

for (qi in seq_len(nrow(jobs))) {
  q <- jobs$query[[qi]]
  qt <- jobs$query_type[[qi]]
  canon <- jobs$canon[[qi]]

  for (geo in geos) {
    if (n_done >= max_queries) {
      message("Reached --max-queries=", max_queries, "; stopping.")
      break
    }
    fname <- sprintf("gtrends_%s_%s.csv", canon, gsub("-", "_", geo))
    fpath <- file.path(out_dir, fname)
    nodata_path <- paste0(fpath, ".nodata")
    if (file.exists(fpath)) {
      n_skip <- n_skip + 1L
      next
    }
    if (file.exists(nodata_path)) {
      n_skip_nodata <- n_skip_nodata + 1L
      next
    }

    message("[", n_done + 1L, "] ", canon, " @ ", geo)
    series <- tryCatch(
      fetch_one_topic_one_geo(
        query = q,
        geo = geo,
        timeframe = timeframe,
        query_type = qt,
        canon_name = canon
      ),
      error = function(e) {
        message("  ERROR: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(series) || nrow(series) == 0L) {
      message("  Retrying with fallback timeframe ", timeframe_fallback)
      series <- tryCatch(
        fetch_one_topic_one_geo(
          query = q,
          geo = geo,
          timeframe = timeframe_fallback,
          query_type = qt,
          canon_name = canon
        ),
        error = function(e) NULL
      )
    }

    if (is.null(series) || nrow(series) == 0L) {
      # Persist failure so resume does not re-hammer Google for empty geo×term pairs.
      writeLines(
        c(
          paste0("query=", q),
          paste0("geo=", geo),
          paste0("time=", Sys.time())
        ),
        nodata_path
      )
      n_fail <- n_fail + 1L
      message("  Marked nodata: ", basename(nodata_path))
      Sys.sleep(sleep_sec)
      next
    }

    readr::write_csv(series, fpath)
    meta_rows[[length(meta_rows) + 1L]] <- series %>%
      dplyr::distinct(topic_label, query_input, query_type, mid, topic_title, topic_type, geo)
    n_done <- n_done + 1L
    Sys.sleep(sleep_sec)
  }
  if (n_done >= max_queries) break
}

if (length(meta_rows)) {
  readr::write_csv(dplyr::bind_rows(meta_rows), meta_path)
}

message(
  "Done. new=", n_done, " skipped=", n_skip,
  " skipped_nodata=", n_skip_nodata, " failed=", n_fail,
  " | folder=", out_dir
)
