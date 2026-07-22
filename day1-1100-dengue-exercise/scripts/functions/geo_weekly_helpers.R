# Shared geo / window helpers for day1-1100 dengue scripts.
# Sourced after load_most_recent_file.R

parse_geo_arg <- function(default = "MX") {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 1L) toupper(args[[1]]) else default
}

train_start <- as.Date("2015-01-01")
train_end <- as.Date("2017-12-31")
test_start <- as.Date("2018-01-01")
test_end <- as.Date("2019-12-31")

predictors <- c("dengue", "mosquito", "sintomas_de_dengue")

#' Load most recent weekly Google Trends file for a geo (no join).


#' Load most recent weekly OpenDengue cases file for a geo (no join).
load_weekly_dengue_cases <- function(geo) {
  load_most_recent_file(
    "day1-1100-dengue-exercise/downloads",
    paste0("dengue_cases_", geo, "_weekly.csv"),
    geo = NULL
  ) %>%
    mutate(start_date = as.Date(start_date)) %>%
    select(start_date, dengue_total) %>%
    arrange(start_date)
}

#' Inner-join Trends and cases on start_date, then apply modelling window filters.
#' Kept separate so you can inspect / replace the join manually.
join_weekly_gtrends_cases <- function(
  gtrends,
  cases,
  start = train_start,
  end = test_end
) {
  gtrends %>%
    inner_join(cases, by = "start_date") %>%
    filter(
      start_date >= start,
      start_date <= end,
      is.finite(dengue_total)
    ) %>%
    arrange(start_date)
}

#' Convenience wrapper: load both sources, join, and split train/test.
load_weekly_dengue_geo <- function(geo) {
  gtrends <- load_weekly_gtrends(geo)
  cases <- load_weekly_dengue_cases(geo)
  dengue_df <- join_weekly_gtrends_cases(gtrends, cases)

  list(
    geo = geo,
    gtrends = gtrends,
    cases = cases,
    dengue_df = dengue_df,
    train_df = dengue_df %>% filter(start_date <= train_end),
    test_df = dengue_df %>% filter(start_date >= test_start)
  )
}
