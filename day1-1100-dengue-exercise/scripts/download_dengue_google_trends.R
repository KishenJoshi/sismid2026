# Scrape Google Trends for dengue-related search interest by country.
#
# Weekly interest for 2015-01-01 .. 2019-12-31 (matches OpenDengue modelling window).
# Each target term is queried individually (avoids relative scaling within a
# multi-keyword request), then joined into one wide table.
#
# Usage (from repo root):
#   Rscript day1-1100-dengue-exercise/scripts/download_dengue_google_trends.R MX
#   Rscript day1-1100-dengue-exercise/scripts/download_dengue_google_trends.R BR
#
# Output:
#   day1-1100-dengue-exercise/downloads/<Sys.Date()>/googletrends_dengue_{GEO}.csv
#
# Packages: gtrendsR, dplyr, tidyr, readr

suppressPackageStartupMessages({
  library(gtrendsR)
  library(dplyr)
  library(tidyr)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
geo <- if (length(args) >= 1L) toupper(args[[1]]) else "MX"

# Country-specific terms from Table A of Yang et al. (2017), queried
# individually. Existing modelling columns (dengue, mosquito,
# sintomas_de_dengue) are retained unchanged; the additional columns are
# downloaded for future analysis but are not yet used by the models.
kw_by_geo <- list(
  BR = c(
    dengue = "dengue",
    sintomas_dengue = "sintomas dengue",
    mosquito_generic = "mosquito",
    sintomas_da_dengue = "sintomas da dengue",
    a_dengue = "a dengue",
    mosquito_dengue = "mosquito dengue",
    mosquito = "mosquito da dengue",
    dengue_hemorragica = "dengue hemorrágica",
    sintomas_de_dengue = "sintomas de dengue",
    sobre_a_dengue = "sobre a dengue"
  ),
  MX = c(
    dengue = "dengue",
    dengue_dengue_dengue = "dengue dengue dengue",
    el_dengue = "el dengue",
    dengue_sintomas = "dengue sintomas",
    sintomas_del_dengue = "sintomas del dengue",
    dengue_hemorragico = "dengue hemorragico",
    sintomas_de_dengue = "sintomas de dengue",
    que_es_dengue = "que es dengue",
    dengue_clasico = "dengue clasico",
    dengue_mosquito = "dengue mosquito",
    # Retain the existing model input; this was not a Mexico term in Table A.
    mosquito = "mosquito"
  )
)
term_map <- kw_by_geo[[geo]]
if (is.null(term_map)) {
  term_map <- kw_by_geo[["MX"]]
}

# ~5y custom range → weekly resolution in gtrendsR.
timeframe <- "2015-01-01 2019-12-31"
sleep_between_queries_sec <- 10

parse_hits <- function(x) {
  x <- as.character(x)
  x[x == "<1"] <- "0"
  as.numeric(x)
}

fetch_trends <- function(keyword, geo, time, tries = 4L, wait_sec = 45) {
  last_err <- NULL
  for (i in seq_len(tries)) {
    tryCatch(
      {
        return(gtrends(
          keyword = keyword,
          geo = geo,
          time = time,
          onlyInterest = TRUE
        ))
      },
      error = function(e) {
        last_err <<- e
        message("  Attempt ", i, "/", tries, " failed: ", conditionMessage(e))
        if (i < tries) {
          message("  Waiting ", wait_sec, "s before retry...")
          Sys.sleep(wait_sec)
        }
      }
    )
  }
  stop(last_err)
}

#' Pull one keyword and return a one-column wide tibble (start_date + canon name).
fetch_one_term <- function(canon_name, keyword, geo, time) {
  message("  Querying: \"", keyword, "\" -> ", canon_name)
  iot <- fetch_trends(keyword, geo, time)$interest_over_time
  if (is.null(iot) || nrow(iot) == 0L) {
    stop("Google Trends returned no interest_over_time for: ", keyword)
  }
  iot %>%
    transmute(
      start_date = as.Date(date),
      hits = parse_hits(hits)
    ) %>%
    filter(!is.na(start_date), is.finite(hits)) %>%
    group_by(start_date) %>%
    summarise(hits = mean(hits), .groups = "drop") %>%
    arrange(start_date) %>%
    rename(!!canon_name := hits)
}

message(
  "Pulling Google Trends one term at a time: ",
  paste(names(term_map), collapse = ", ")
)
message("Geo: ", geo, " | Timeframe: ", timeframe, " (weekly)")
message("Sleep between queries: ", sleep_between_queries_sec, "s")

term_tables <- list()
canon_names <- names(term_map)
for (i in seq_along(term_map)) {
  canon <- canon_names[[i]]
  kw <- unname(term_map[[i]])
  term_tables[[canon]] <- fetch_one_term(canon, kw, geo, timeframe)
  if (i < length(term_map)) {
    message("  Sleeping ", sleep_between_queries_sec, "s before next query...")
    Sys.sleep(sleep_between_queries_sec)
  }
}

googletrends_dengue <- Reduce(
  function(a, b) full_join(a, b, by = "start_date"),
  term_tables
)

desired <- c("start_date", names(term_map))
googletrends_dengue <- googletrends_dengue %>%
  select(all_of(desired)) %>%
  arrange(start_date)

if (nrow(googletrends_dengue) < 2L) {
  stop("Parsed Google Trends data look empty; not writing CSV.")
}

dir_to_save <- paste0("day1-1100-dengue-exercise/downloads/", Sys.Date())
dir.create(dir_to_save, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(dir_to_save, paste0("googletrends_dengue_", geo, ".csv"))
write_csv(googletrends_dengue, out_file)

message("Wrote ", nrow(googletrends_dengue), " rows to ", out_file)
message(
  "Date range: ", min(googletrends_dengue$start_date), " to ",
  max(googletrends_dengue$start_date)
)
