# Prepare weekly OpenDengue dengue cases for modelling (2015-2019).
#
# Usage (from repo root):
#   Rscript day1-1100-dengue-exercise/scripts/prepare_opendengue_weekly_cases.R MX
#   Rscript day1-1100-dengue-exercise/scripts/prepare_opendengue_weekly_cases.R BR
#
# Output:
#   day1-1100-dengue-exercise/downloads/<Sys.Date()>/dengue_cases_{GEO}_weekly.csv
#   columns: start_date, end_date, dengue_total, ...

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
})

args <- commandArgs(trailingOnly = TRUE)
geo <- if (length(args) >= 1L) toupper(args[[1]]) else "MX"

opendengue_path <- switch(
  geo,
  MX = "day1-1100-dengue-exercise/data/mexico_dengue_opendengue.csv",
  BR = "day1-1100-dengue-exercise/data/brazil_dengue_opendengue.csv",
  stop("Unsupported geo: ", geo, " (use MX or BR)")
)

iso <- switch(geo, MX = "MEX", BR = "BRA")

raw <- read_csv(opendengue_path, show_col_types = FALSE)

cases <- raw %>%
  filter(
    toupper(ISO_A0) %in% c("MEX", "BRA"),
    T_res == "Week"
  ) %>%
  dplyr::select(adm_0_name, calendar_start_date, calendar_end_date, Year, T_res, dengue_total) %>%
  rename(
    start_date = calendar_start_date,
    end_date = calendar_end_date
  ) %>%
  filter(
    start_date >= as.Date("2015-01-01"),
    end_date <= as.Date("2019-12-31")
  ) %>%
  arrange(start_date) %>%
  distinct(start_date, .keep_all = TRUE)

if (nrow(cases) < 50L) {
  stop("Too few weekly rows after filter for geo=", geo, " (n=", nrow(cases), ")")
}

# Basic weekly spacing check
gaps <- diff(as.numeric(cases$start_date))
if (any(gaps < 6 | gaps > 8)) {
  warning(
    "Unusual week spacing detected for geo=", geo,
    "; min gap=", min(gaps), " max gap=", max(gaps)
  )
}

dir_to_save <- paste0("day1-1100-dengue-exercise/downloads/", Sys.Date())
dir.create(dir_to_save, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(dir_to_save, paste0("dengue_cases_", geo, "_weekly.csv"))
write_csv(cases, out_file)

message("Wrote ", nrow(cases), " weekly rows to ", out_file)
message("Date range: ", min(cases$start_date), " to ", max(cases$end_date))
message(
  "Train weeks (2015-2017): ",
  sum(cases$start_date >= as.Date("2015-01-01") & cases$end_date <= as.Date("2017-12-31"))
)
message(
  "Test weeks (2018-2019): ",
  sum(cases$start_date >= as.Date("2018-01-01") & cases$end_date <= as.Date("2019-12-31"))
)

