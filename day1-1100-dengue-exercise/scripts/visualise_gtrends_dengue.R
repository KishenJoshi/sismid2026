# Visualise weekly Google Trends + OpenDengue dengue cases for a country.
#
# Usage (from repo root):
#   Rscript day1-1100-dengue-exercise/scripts/visualise_gtrends_dengue.R MX
#   Rscript day1-1100-dengue-exercise/scripts/visualise_gtrends_dengue.R BR

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(stringr)
  library(lubridate)
})

source("day1-1100-dengue-exercise/scripts/functions/load_most_recent_file.R")
source("day1-1100-dengue-exercise/scripts/functions/geo_weekly_helpers.R")

geo <- parse_geo_arg("MX")

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

dengue_df <- left_join(gtrends_data, case_data, by = "start_date") %>%
  filter(!is.na(dengue_total))

`%||%` <- function(a, b) if (!is.null(a)) a else b

message("Geo: ", geo)
message(
  "Joined weekly range: ", min(dengue_df$start_date), " to ",
  max(dengue_df$start_date), " (", nrow(dengue_df), " weeks)"
)

plot_dengue_timeseries <- function(dengue_df,
                                   colnames_to_plot = "dengue_total",
                                   facet = FALSE,
                                   title = NULL) {
  missing_cols <- setdiff(colnames_to_plot, names(dengue_df))
  if (length(missing_cols) > 0L) {
    stop("Column(s) not found: ", paste(missing_cols, collapse = ", "))
  }

  plot_df <- dengue_df %>%
    select(start_date, all_of(colnames_to_plot)) %>%
    pivot_longer(
      cols = all_of(colnames_to_plot),
      names_to = "series",
      values_to = "value"
    ) %>%
    filter(!is.na(value))

  p <- ggplot(plot_df, aes(x = start_date, y = value, colour = series)) +
    geom_line(linewidth = 0.7) +
    labs(
      x = "Week start date",
      y = "Value",
      colour = "Series",
      title = title %||% paste(geo, "weekly dengue time series")
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  if (isTRUE(facet)) {
    p <- p +
      facet_wrap(~series, scales = "free_y", ncol = 1) +
      guides(colour = "none")
  }
  p
}

cols_plot <- c("dengue_total", predictors)
p <- plot_dengue_timeseries(
  dengue_df,
  colnames_to_plot = cols_plot,
  facet = TRUE,
  title = paste0(geo, " weekly dengue (OpenDengue + Google Trends), 2015–2019")
)

out_dir <- file.path(
  "day1-1100-dengue-exercise/outputs/visualise_gtrends_dengue",
  geo
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "all_time_series.png")
ggsave(out_file, plot = p, width = 9, height = 8, dpi = 150)
message("Saved figure to ", out_file)
