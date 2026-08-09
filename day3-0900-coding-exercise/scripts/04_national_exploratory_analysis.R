# 04_national_exploratory_analysis.R
# National exploratory diagnostics for dengue, climate, and Google Trends.
#
# Current: gtrends hit distributions + screening time series, then retained-term
# cases/Trends and cases/climate faceted panels (contemporaneous only), plus a
# 2020–2022 national zoom. Later: ACF/CCF (ADM1-style).
#
# Usage (from repo root):
#   Rscript day3-0900-coding-exercise/scripts/04_national_exploratory_analysis.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})

source("day3-0900-coding-exercise/scripts/functions/00_exploratory_analysis_functions.R")

out_dir <- ensure_dir(
  "day3-0900-coding-exercise", "results", "04_national_exploratory_analysis"
)

#--------------- Load national exposure–response panel

panel <- read_csv(
  "day3-0900-coding-exercise/data/mx_national_exposure_response_monthly.csv",
  show_col_types = FALSE
) %>%
  mutate(month_start = as.Date(month_start))

gt_cols <- select_gt_cols(panel)
message("Contemporaneous Google Trends columns: ", paste(gt_cols, collapse = ", "))

gt_long <- pivot_gtrends_long(panel, gt_cols)

#--------------- Hit distributions for manual term assessment (all terms)

gt_summary <- summarise_gtrends_hits(gt_long)
print(gt_summary, n = Inf)
write_csv(gt_summary, file.path(out_dir, "gtrends_hits_summary.csv"))

p_hist <- plot_gtrends_hits_hist(gt_long)
ggsave(
  file.path(out_dir, "gtrends_hits_histograms.png"),
  p_hist, width = 12, height = 8, dpi = 120
)

p_ts <- plot_gtrends_hits_timeseries(gt_long, ncol = 2)
ggsave(
  file.path(out_dir, "gtrends_hits_timeseries.png"),
  p_ts, width = 12, height = max(6, 1.1 * ceiling(length(gt_cols) / 2)), dpi = 120
)

#--------------- Drop low-richness / discarded terms after screening plots

drop_gt_terms <- c("gt_source_management", "gt_epidemic")
drop_gt_lags <- unlist(lapply(drop_gt_terms, function(t) paste0(t, "_lag", 1:3)))
panel <- panel %>% select(-any_of(c(drop_gt_terms, drop_gt_lags)))

gt_cols_retained <- select_gt_cols(panel)
message(
  "Retained Google Trends columns after drop: ",
  paste(gt_cols_retained, collapse = ", ")
)

# Calendar-year case peaks (first month of max; skip years with max == 0)
case_peaks <- seasonal_case_peaks(panel, "monthly_cases")
write_csv(case_peaks, file.path(out_dir, "seasonal_case_peaks.csv"))
message("Seasonal case peaks (n=", nrow(case_peaks), "):")
print(case_peaks, n = Inf)

#--------------- Cases + retained Google Trends (cases on top)

cases_gt_cols <- c("monthly_cases", gt_cols_retained)
cases_gt_labels <- c(
  "monthly_cases",
  sub("^gt_", "", gt_cols_retained)
)
cases_gt_long <- pivot_series_long(panel, cases_gt_cols, cases_gt_labels)

p_cases_gt <- plot_faceted_timeseries(
  cases_gt_long,
  title = "National dengue cases and retained Google Trends (contemporaneous)",
  peak_dates = case_peaks$month_start,
  ncol = 2
)
ggsave(
  file.path(out_dir, "timeseries_cases_gtrends.png"),
  p_cases_gt,
  width = 12,
  height = max(6, 1.1 * ceiling(length(cases_gt_cols) / 2)),
  dpi = 120
)

#--------------- Cases + climate

cases_clim_cols <- c("monthly_cases", "mean_temp_celsius", "total_precip_mm")
cases_clim_long <- pivot_series_long(panel, cases_clim_cols)

p_cases_clim <- plot_faceted_timeseries(
  cases_clim_long,
  title = "National dengue cases and climate (contemporaneous)",
  peak_dates = case_peaks$month_start
)
ggsave(
  file.path(out_dir, "timeseries_cases_climate.png"),
  p_cases_clim,
  width = 10,
  height = 6,
  dpi = 120
)

#--------------- Further term drop after cases/Trends time-series panels
# Keep screening + cases/GT plots above intact; trim panel for later analysis.

drop_gt_terms_2 <- c("gt_dengue", "gt_Aedes_aegypti", "gt_vector")
drop_gt_lags_2 <- unlist(lapply(drop_gt_terms_2, function(t) paste0(t, "_lag", 1:3)))
panel <- panel %>% select(-any_of(c(drop_gt_terms_2, drop_gt_lags_2)))

gt_cols_retained <- select_gt_cols(panel)
message(
  "Retained Google Trends columns after second drop: ",
  paste(gt_cols_retained, collapse = ", ")
)

#--------------- National time series zoom: 2020–2022 (forecast eval window)

panel_2020_2022 <- panel %>%
  filter(
    month_start >= as.Date("2020-01-01"),
    month_start <= as.Date("2022-12-01")
  )
peaks_2020_2022 <- case_peaks %>%
  filter(
    month_start >= as.Date("2020-01-01"),
    month_start <= as.Date("2022-12-01")
  )

cases_gt_cols_zoom <- c("monthly_cases", gt_cols_retained)
cases_gt_labels_zoom <- c(
  "monthly_cases",
  sub("^gt_", "", gt_cols_retained)
)
cases_gt_long_zoom <- pivot_series_long(
  panel_2020_2022, cases_gt_cols_zoom, cases_gt_labels_zoom
)

p_cases_gt_zoom <- plot_faceted_timeseries(
  cases_gt_long_zoom,
  title = "National dengue cases and retained Google Trends (2020–2022)",
  peak_dates = peaks_2020_2022$month_start,
  ncol = 2
)
ggsave(
  file.path(out_dir, "timeseries_cases_gtrends_2020_2022.png"),
  p_cases_gt_zoom,
  width = 12,
  height = max(6, 1.1 * ceiling(length(cases_gt_cols_zoom) / 2)),
  dpi = 120
)

cases_clim_long_zoom <- pivot_series_long(
  panel_2020_2022,
  c("monthly_cases", "mean_temp_celsius", "total_precip_mm")
)
p_cases_clim_zoom <- plot_faceted_timeseries(
  cases_clim_long_zoom,
  title = "National dengue cases and climate (2020–2022)",
  peak_dates = peaks_2020_2022$month_start
)
ggsave(
  file.path(out_dir, "timeseries_cases_climate_2020_2022.png"),
  p_cases_clim_zoom,
  width = 10,
  height = 6,
  dpi = 120
)

message("Exploratory time-series outputs written to ", out_dir)

# Addressing serial correlation (for later ACF/CCF / modelling):
# - Include autoregressive lags of cases (e.g. AR 1, 12, 24 as in the exposure–response panel).
# - Model seasonal structure explicitly (month FE / seasonal smooth) rather than only raw ACF.
# - Prewhiten predictors before CCF, or use residuals from an AR/seasonal model.
# - Difference or detrend if non-stationarity dominates the ACF.
# - Prefer proper time-series / spatiotemporal models (INLA, brms) over naive OLS.
