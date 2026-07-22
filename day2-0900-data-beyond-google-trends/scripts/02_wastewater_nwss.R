# CDC NWSS Influenza A wastewater for Georgia.
#
# Mirrors notebooks/02_wastewater_nwss_soln.ipynb.
#
# Usage (from repo root):
#   Rscript day2-0900-data-beyond-google-trends/scripts/02_wastewater_nwss.R
#
# Packages: dplyr, tidyr, readr, httr, jsonlite, lubridate, ggplot2

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(httr)
  library(jsonlite)
  library(lubridate)
  library(ggplot2)
})

root <- "day2-0900-data-beyond-google-trends"
out_dir <- file.path(root, "outputs", "02_wastewater_nwss")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

NWSS_ID <- "ymmh-divb"
COLS <- c(
  "sample_collect_date", "site", "counties_served", "population_served",
  "pcr_target_avg_conc", "pcr_target_flowpop_lin", "pcr_target_detect"
)
CACHE_PATHS <- c(
  file.path(root, "data/cdc_nwss_influenza_a_ga_cached.csv"),
  "data/cdc_nwss_influenza_a_ga_cached.csv",
  "cdc_nwss_influenza_a_ga_cached.csv"
)

nwss_fetch <- function(state = "ga") {
  params <- list(
    state_territory = state,
    `$select` = paste(COLS, collapse = ","),
    `$order` = "sample_collect_date",
    `$limit` = "50000"
  )
  url <- modify_url(
    paste0("https://data.cdc.gov/resource/", NWSS_ID, ".json"),
    query = params
  )
  tryCatch(
    {
      resp <- GET(url, timeout(45))
      stop_for_status(resp)
      txt <- content(resp, as = "text", encoding = "UTF-8")
      as_tibble(jsonlite::fromJSON(txt, simplifyDataFrame = TRUE))
    },
    error = function(e) {
      message("CDC NWSS live pull failed: ", conditionMessage(e))
      NULL
    }
  )
}

load_cache <- function() {
  for (p in CACHE_PATHS) {
    if (file.exists(p)) {
      message("Using cached snapshot: ", p)
      return(read_csv(p, show_col_types = FALSE))
    }
  }
  stop("NWSS cache not found; check the data/ folder.")
}

get_ga_flu_wastewater <- function() {
  df <- nwss_fetch("ga")
  if (is.null(df) || nrow(df) < 1L) {
    df <- load_cache()
  }
  df %>%
    mutate(
      date = as.Date(sample_collect_date),
      conc = suppressWarnings(as.numeric(pcr_target_avg_conc))
    )
}

# ---------------------------------------------------------------------------
# Fetch
# ---------------------------------------------------------------------------

ww <- get_ga_flu_wastewater()
message(
  "rows: ", nrow(ww), " | range: ", min(ww$date, na.rm = TRUE),
  " to ", max(ww$date, na.rm = TRUE)
)
message("distinct sites: ", n_distinct(ww$site))
print(head(ww %>% select(date, site, counties_served, population_served, conc)))

# ---------------------------------------------------------------------------
# Weekly state mean
# ---------------------------------------------------------------------------

wk <- ww %>%
  filter(!is.na(conc), !is.na(date)) %>%
  mutate(week = floor_date(date, "week", week_start = 7)) %>%
  group_by(week) %>%
  summarise(conc = mean(conc, na.rm = TRUE), .groups = "drop") %>%
  rename(date = week)

p_wk <- ggplot(wk, aes(date, conc)) +
  geom_line(linewidth = 0.7, colour = "#2A6F97") +
  labs(
    title = "Georgia influenza A in wastewater (weekly mean concentration)",
    x = "date", y = "copies/L"
  ) +
  theme_minimal()
ggsave(
  file.path(out_dir, "ga_flu_wastewater_weekly.png"),
  p_wk, width = 10, height = 4, dpi = 120, bg = "white"
)

# ---------------------------------------------------------------------------
# Largest site vs state aggregate
# ---------------------------------------------------------------------------

ww <- ww %>%
  mutate(pop = suppressWarnings(as.numeric(population_served)))
big <- ww %>%
  arrange(desc(pop)) %>%
  slice(1) %>%
  pull(site)

site <- ww %>%
  filter(site == big, !is.na(conc), !is.na(date)) %>%
  mutate(week = floor_date(date, "week", week_start = 7)) %>%
  group_by(week) %>%
  summarise(conc = mean(conc, na.rm = TRUE), .groups = "drop") %>%
  rename(date = week)

cmp <- bind_rows(
  wk %>% mutate(series = "state weekly mean"),
  site %>% mutate(series = paste0("site ", big, " only"))
)

p_cmp <- ggplot(cmp, aes(date, conc, colour = series)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Single site vs state aggregate",
    x = "date", y = "copies/L", colour = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(
  file.path(out_dir, "site_vs_state_aggregate.png"),
  p_cmp, width = 10, height = 4, dpi = 120, bg = "white"
)

# ---------------------------------------------------------------------------
# Coverage + save
# ---------------------------------------------------------------------------

cov <- ww %>%
  filter(!is.na(conc), !is.na(date)) %>%
  mutate(week = floor_date(date, "week", week_start = 7)) %>%
  group_by(week) %>%
  summarise(n_sites = n_distinct(site), .groups = "drop") %>%
  arrange(week)

message("reporting sites per week (last 6):")
print(tail(cov, 6))
message("latest sample date: ", max(ww$date, na.rm = TRUE),
        " (expect a lag vs today)")

out_csv <- file.path(out_dir, "ga_flu_wastewater_weekly.csv")
write_csv(wk, out_csv)
write_csv(cov, file.path(out_dir, "reporting_sites_per_week.csv"))
message("saved ", out_csv, " ", paste(dim(wk), collapse = " x "))
