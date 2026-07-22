# Visualise expanding-window AR / ARX deployment metrics (MX or BR).
#
# Reads outputs from simulate_ar_deployment.R and writes:
#   lead_decay_R2.png / lead_decay_MAPE.png
#     3×3 panels: rows = search term, cols = AR | ARX raw | ARX processed
#     Lines = max_lag (1,2,3); x = prediction_lead
#   origin_performance_R2.png / origin_performance_MAPE.png
#     3×8 panels: rows = search term, columns = prediction lead (1–8)
#     Lines = AR | ARX raw | ARX processed; max_lag fixed at 3
#
# Usage:
#   Rscript day1-1100-dengue-exercise/scripts/plot_ar_deployment.R MX

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(stringr)
  library(patchwork)
})

source("day1-1100-dengue-exercise/scripts/functions/geo_weekly_helpers.R")
source("day1-1100-dengue-exercise/scripts/functions/ar_deployment_functions.R")

geo <- parse_geo_arg("MX")
out_dir <- file.path(
  "day1-1100-dengue-exercise/outputs/simulate_ar_deployment",
  geo
)

metrics_by_lead <- read_csv(
  file.path(out_dir, "metrics_by_lead.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    max_lag = factor(max_lag, levels = 1:3),
    predictor_lab = case_when(
      predictor == "dengue" ~ "dengue",
      predictor == "mosquito" ~ "mosquito",
      predictor == "sintomas_de_dengue" ~ "sintomas de dengue",
      TRUE ~ as.character(predictor)
    )
  )

metrics_by_origin <- read_csv(
  file.path(out_dir, "metrics_by_origin.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    origin_week = as.Date(origin_week),
    max_lag = factor(max_lag, levels = 1:3),
    predictor_lab = case_when(
      predictor == "dengue" ~ "dengue",
      predictor == "mosquito" ~ "mosquito",
      predictor == "sintomas_de_dengue" ~ "sintomas de dengue",
      TRUE ~ as.character(predictor)
    )
  )

forecasts_long <- read_csv(
  file.path(out_dir, "forecasts_long.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    origin_week = as.Date(origin_week),
    predictor_lab = case_when(
      predictor == "dengue" ~ "dengue",
      predictor == "mosquito" ~ "mosquito",
      predictor == "sintomas_de_dengue" ~ "sintomas de dengue",
      TRUE ~ as.character(predictor)
    )
  )

term_levels <- c("dengue", "mosquito", "sintomas de dengue")
lag_cols <- c("1" = "#1b9e77", "2" = "#d95f02", "3" = "#7570b3")

# ---------------------------------------------------------------------------
# Lead-decay panels (3 rows × 3 cols)
# ---------------------------------------------------------------------------

#' Build long data for one metric: AR (shared) + ARX raw + ARX processed per term.
lead_panel_data <- function(df, metric) {
  ar <- df %>%
    filter(model == "ar", processing == "raw") %>%
    transmute(
      prediction_lead,
      max_lag,
      value = .data[[metric]],
      panel = "AR",
      predictor_lab = NA_character_
    )

  # Replicate AR under each search-term row for the 3×3 grid
  ar_grid <- tidyr::expand_grid(
    predictor_lab = term_levels,
    ar %>% select(-predictor_lab)
  ) %>%
    mutate(panel = "AR")

  arx <- df %>%
    filter(model == "arx") %>%
    transmute(
      predictor_lab,
      prediction_lead,
      max_lag,
      value = .data[[metric]],
      panel = if_else(processing == "raw", "ARX (raw)", "ARX (processed)")
    )

  bind_rows(ar_grid, arx) %>%
    mutate(
      predictor_lab = factor(predictor_lab, levels = term_levels),
      panel = factor(panel, levels = c("AR", "ARX (raw)", "ARX (processed)"))
    )
}

plot_lead_decay <- function(df, metric, ylab, ylim = NULL) {
  d <- lead_panel_data(df, metric)
  p <- ggplot(d, aes(prediction_lead, value, colour = max_lag, group = max_lag)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.6) +
    facet_grid(predictor_lab ~ panel) +
    scale_colour_manual(values = lag_cols, name = "max lag") +
    scale_x_continuous(breaks = 1:8) +
    labs(
      x = "Prediction lead (weeks)",
      y = ylab,
      title = paste0(geo, ": forecast skill vs lead (", ylab, ")")
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "grey92", colour = NA),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p
}

p_r2 <- plot_lead_decay(metrics_by_lead, "R2", expression(R^2), ylim = c(NA, 1))
p_mape <- plot_lead_decay(metrics_by_lead, "MAPE", "MAPE (%)")

ggsave(
  file.path(out_dir, "lead_decay_R2.png"),
  p_r2,
  width = 10,
  height = 8,
  dpi = 150
)
ggsave(
  file.path(out_dir, "lead_decay_MAPE.png"),
  p_mape,
  width = 10,
  height = 8,
  dpi = 150
)

# ---------------------------------------------------------------------------
# Performance through time by prediction lead (3 rows × 8 columns)
# ---------------------------------------------------------------------------

series_cols <- c(
  "AR" = "#333333",
  "ARX (raw)" = "#e41a1c",
  "ARX (processed)" = "#2ca02c"
)

origin_plot_data <- function(df, lag_keep = 3L) {
  ar <- df %>%
    filter(model == "ar", processing == "raw", max_lag == lag_keep) %>%
    transmute(
      origin_week, prediction_lead, actual, predicted, APE,
      series = "AR"
    )

  # AR does not use a search term, so repeat it as the baseline in each row.
  ar_grid <- tidyr::expand_grid(
    predictor_lab = term_levels,
    ar
  )

  arx <- df %>%
    filter(model == "arx", max_lag == lag_keep) %>%
    transmute(
      origin_week, prediction_lead, predictor_lab, actual, predicted, APE,
      series = paste0("ARX (", processing, ")")
    )

  bind_rows(ar_grid, arx) %>%
    mutate(
      predictor_lab = factor(predictor_lab, levels = term_levels),
      prediction_lead = factor(
        prediction_lead,
        levels = 1:8,
        labels = paste("Lead", 1:8)
      ),
      series = factor(
        series,
        levels = c("AR", "ARX (raw)", "ARX (processed)")
      )
    )
}

# Eight adjacent forecast origins are used independently for each lead.
# Confidence limits are percentile intervals from paired bootstrap resampling.
rolling_r2 <- function(df, window = 8L, boot_reps = 300L) {
  df <- arrange(df, origin_week)
  n <- nrow(df)
  out <- tibble(
    origin_week = df$origin_week,
    R2 = NA_real_,
    R2_low = NA_real_,
    R2_high = NA_real_
  )
  if (n < window) return(out)

  for (i in seq.int(window, n)) {
    idx <- seq.int(i - window + 1L, i)
    y <- df$actual[idx]
    yhat <- df$predicted[idx]
    out$R2[i] <- r2_score(y, yhat)

    boot <- replicate(boot_reps, {
      b <- sample.int(window, window, replace = TRUE)
      r2_score(y[b], yhat[b])
    })
    boot <- boot[is.finite(boot)]
    if (length(boot) >= 20L) {
      ci <- quantile(boot, c(0.025, 0.975), names = FALSE)
      out$R2_low[i] <- ci[[1]]
      out$R2_high[i] <- ci[[2]]
    }
  }
  out
}

origin_data <- origin_plot_data(forecasts_long)

set.seed(20260721)
rolling_r2_data <- origin_data %>%
  group_by(predictor_lab, prediction_lead, series) %>%
  group_modify(~ rolling_r2(.x, window = 8L, boot_reps = 300L)) %>%
  ungroup()

write_csv(
  rolling_r2_data,
  file.path(out_dir, "rolling_R2_by_origin_and_lead.csv")
)

common_origin_theme <- theme_bw(base_size = 9) +
  theme(
    strip.background = element_rect(fill = "grey92", colour = NA),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_origin_r2 <- ggplot(
  rolling_r2_data,
  aes(origin_week, R2, colour = series, fill = series, group = series)
) +
  geom_ribbon(
    aes(ymin = R2_low, ymax = R2_high),
    alpha = 0.10,
    colour = NA
  ) +
  geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.3) +
  geom_line(
    data = rolling_r2_data %>% filter(series != "AR"),
    linewidth = 0.55
  ) +
  # Draw AR last so the shared baseline remains visible above ARX layers.
  geom_line(
    data = rolling_r2_data %>% filter(series == "AR"),
    linewidth = 0.65,
    alpha = 0.5
  ) +
  facet_wrap(
    vars(predictor_lab, prediction_lead),
    ncol = 8,
    scales = "free_y"
  ) +
  scale_colour_manual(values = series_cols, name = NULL) +
  scale_fill_manual(values = series_cols, name = NULL) +
  labs(
    x = "Origin week",
    y = expression(R^2),
    title = paste0(geo, ": rolling forecast performance by lead"),
    subtitle = "8-origin rolling predictive R² with bootstrap 95% CIs (max lag = 3); free y-scale per panel"
  ) +
  common_origin_theme

p_origin_mape <- ggplot(
  origin_data,
  aes(origin_week, APE, colour = series, group = series)
) +
  geom_line(
    data = origin_data %>% filter(series != "AR"),
    linewidth = 0.5,
    alpha = 0.85
  ) +
  # Draw AR last so overlapping ARX paths do not hide it.
  geom_line(
    data = origin_data %>% filter(series == "AR"),
    linewidth = 0.65,
    alpha = 0.5
  ) +
  facet_wrap(
    vars(predictor_lab, prediction_lead),
    ncol = 8,
    scales = "free_y"
  ) +
  scale_colour_manual(values = series_cols, name = NULL) +
  labs(
    x = "Origin week",
    y = "Absolute percentage error (%)",
    title = paste0(geo, ": pointwise forecast error by lead"),
    subtitle = "Each line is pointwise APE at that lead (max lag = 3); lower is better"
  ) +
  common_origin_theme

ggsave(
  file.path(out_dir, "origin_performance_R2.png"),
  p_origin_r2,
  width = 22,
  height = 9,
  dpi = 150
)
ggsave(
  file.path(out_dir, "origin_performance_MAPE.png"),
  p_origin_mape,
  width = 22,
  height = 9,
  dpi = 150
)

message("Wrote lead_decay_R2.png / lead_decay_MAPE.png")
message("Wrote 24-panel origin_performance_R2.png / origin_performance_MAPE.png")
message("Wrote rolling_R2_by_origin_and_lead.csv")
message("Done.")
