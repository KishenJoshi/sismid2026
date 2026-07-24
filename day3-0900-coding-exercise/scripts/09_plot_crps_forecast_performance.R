# 09_plot_crps_forecast_performance.R
# Day1-style forecast / performance figures, scored with CRPS.
#
# Inputs (from script 08):
#   outputs/evaluation/expanding_window_crps_scores.csv
#
# Outputs:
#   outputs/evaluation/plots/
#     lead_decay_CRPS.png
#     origin_performance_CRPS.png
#     forecast_vs_observed_by_lead_national.png
#     forecast_vs_observed_by_lead_<STATE>.png
#
# Usage (from day3 root):
#   Rscript scripts/09_plot_crps_forecast_performance.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(tibble)
})

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
source(file.path(root, "scripts/functions/paths.R"))

scores_path <- file.path(DAY3_ROOT, "outputs/evaluation/expanding_window_crps_scores.csv")
if (!file.exists(scores_path)) {
  stop(
    "Missing ", scores_path,
    " — run scripts/08_evaluate_expanding_window_crps.R first."
  )
}

out_dir <- ensure_dir(DAY3_ROOT, "outputs", "evaluation", "plots")
scores <- readr::read_csv(scores_path, show_col_types = FALSE) %>%
  dplyr::mutate(
    origin = as.Date(origin),
    target_month = as.Date(target_month),
    lead = as.integer(lead)
  )

example_states <- intersect(
  c("MX-JAL", "MX-VER", "MX-YUC", "MX-NLE"),
  unique(scores$rne_iso_code)
)

# Friendly labels + colours (day1-like contrast: baseline vs Trends)
model_levels <- c(
  "inla_climate",
  "inla_trends",
  "inla_hybrid_climate",
  "inla_hybrid_trends"
)
model_labels <- c(
  inla_climate = "INLA climate",
  inla_trends = "INLA climate+Trends",
  inla_hybrid_climate = "Hybrid (brms→INLA) climate",
  inla_hybrid_trends = "Hybrid (brms→INLA) climate+Trends"
)
model_cols <- c(
  inla_climate = "#377eb8",
  inla_trends = "#e41a1c",
  inla_hybrid_climate = "#4daf4a",
  inla_hybrid_trends = "#984ea3"
)

scores <- scores %>%
  dplyr::filter(model %in% model_levels) %>%
  dplyr::mutate(
    model = factor(model, levels = intersect(model_levels, unique(model))),
    model_lab = factor(
      model_labels[as.character(model)],
      levels = unname(model_labels[intersect(model_levels, levels(model))])
    ),
    lead_lab = factor(paste("Lead", lead), levels = paste("Lead", sort(unique(lead))))
  )

lead_breaks <- sort(unique(scores$lead))
present_models <- levels(droplevels(scores$model))
cols_use <- model_cols[present_models]
labs_use <- model_labels[present_models]

common_theme <- theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey92", colour = NA),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

common_origin_theme <- theme_bw(base_size = 9) +
  theme(
    strip.background = element_rect(fill = "grey92", colour = NA),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#----- Lead decay: mean CRPS vs lead (state-pooled + national-pooled)
lead_state <- scores %>%
  dplyr::group_by(model, model_lab, lead) %>%
  dplyr::summarise(
    mean_crps = mean(crps, na.rm = TRUE),
    median_crps = median(crps, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(grain = "State-level (pooled)")

# National: sum observed / pred across states within origin×lead, then CRPS proxy
# via mean absolute error on national totals is not CRPS; instead average state CRPS
# weighted equally, and also score on national sums using |pred-obs| as a simple
# national skill curve for the vs-observed plots. For lead decay keep pooled mean CRPS.
lead_decay <- lead_state

p_lead <- ggplot(
  lead_decay,
  aes(lead, mean_crps, colour = model_lab, group = model_lab)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_colour_manual(values = setNames(cols_use, labs_use), name = NULL) +
  scale_x_continuous(breaks = lead_breaks) +
  labs(
    x = "Forecast lead (months after origin)",
    y = "Mean CRPS",
    title = "MX Admin1: forecast skill vs lead (CRPS)",
    subtitle = "Lower is better. Expanding-window INLA; state-month scores pooled."
  ) +
  common_theme

ggsave(
  file.path(out_dir, "lead_decay_CRPS.png"),
  p_lead,
  width = 8,
  height = 5,
  dpi = 150
)

# Split comparisons: full covariate sets vs brms hybrids
plot_lead_pair <- function(models, title_suffix, outfile) {
  d <- lead_decay %>% dplyr::filter(as.character(model) %in% models)
  if (!nrow(d)) return(invisible(NULL))
  labs_p <- model_labels[models]
  cols_p <- model_cols[models]
  p <- ggplot(
    d,
    aes(lead, mean_crps, colour = model_lab, group = model_lab)
  ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    scale_colour_manual(values = setNames(cols_p, labs_p), name = NULL) +
    scale_x_continuous(breaks = lead_breaks) +
    labs(
      x = "Forecast lead (months after origin)",
      y = "Mean CRPS",
      title = paste0("MX Admin1: CRPS vs lead — ", title_suffix),
      subtitle = "Lower is better. Expanding-window evaluation."
    ) +
    common_theme
  ggsave(file.path(out_dir, outfile), p, width = 8, height = 5, dpi = 150)
  invisible(p)
}

plot_lead_pair(
  c("inla_climate", "inla_trends"),
  "climate vs Trends (full INLA)",
  "lead_decay_CRPS_climate_vs_trends.png"
)
plot_lead_pair(
  c("inla_hybrid_climate", "inla_hybrid_trends"),
  "climate vs Trends (brms→INLA hybrid)",
  "lead_decay_CRPS_hybrid.png"
)

#----- Origin performance: mean CRPS by origin, faceted by lead
origin_perf <- scores %>%
  dplyr::group_by(model, model_lab, origin, lead, lead_lab) %>%
  dplyr::summarise(mean_crps = mean(crps, na.rm = TRUE), .groups = "drop")

p_origin <- ggplot(
  origin_perf,
  aes(origin, mean_crps, colour = model_lab, group = model_lab)
) +
  geom_line(linewidth = 0.55, alpha = 0.85) +
  facet_wrap(vars(lead_lab), ncol = length(lead_breaks), scales = "free_y") +
  scale_colour_manual(values = setNames(cols_use, labs_use), name = NULL) +
  labs(
    x = "Origin month (last month in training window)",
    y = "Mean CRPS across states",
    title = "MX Admin1: expanding-window CRPS by origin and lead",
    subtitle = "Lower is better"
  ) +
  common_origin_theme

ggsave(
  file.path(out_dir, "origin_performance_CRPS.png"),
  p_origin,
  width = 12,
  height = 4.5,
  dpi = 150
)

# Rolling mean CRPS (analogous to day1 rolling R²)
rolling_crps <- function(df, window = 6L) {
  df <- df %>% dplyr::arrange(origin)
  n <- nrow(df)
  if (n < 2L) {
    return(tibble::tibble(
      origin = df$origin,
      mean_crps = df$mean_crps,
      crps_roll = df$mean_crps
    ))
  }
  roll <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    j0 <- max(1L, i - window + 1L)
    roll[[i]] <- mean(df$mean_crps[j0:i], na.rm = TRUE)
  }
  tibble::tibble(
    origin = df$origin,
    mean_crps = df$mean_crps,
    crps_roll = roll
  )
}

rolling_data <- origin_perf %>%
  dplyr::group_by(model, model_lab, lead, lead_lab) %>%
  dplyr::group_modify(~ rolling_crps(.x, window = 6L)) %>%
  dplyr::ungroup()

p_roll <- ggplot(
  rolling_data,
  aes(origin, crps_roll, colour = model_lab, group = model_lab)
) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  facet_wrap(vars(lead_lab), ncol = length(lead_breaks), scales = "free_y") +
  scale_colour_manual(values = setNames(cols_use, labs_use), name = NULL) +
  labs(
    x = "Origin month",
    y = "6-origin rolling mean CRPS",
    title = "MX Admin1: rolling CRPS by lead",
    subtitle = "Expanding-window; lower is better"
  ) +
  common_origin_theme

ggsave(
  file.path(out_dir, "origin_performance_rolling_CRPS.png"),
  p_roll,
  width = 12,
  height = 4.5,
  dpi = 150
)

#----- Forecast vs observed by lead
# Prefer climate / Trends pair for overlays; fall back to whatever is present.
overlay_models <- intersect(
  c("inla_climate", "inla_trends", "inla_hybrid_climate", "inla_hybrid_trends"),
  present_models
)

plot_vs_observed <- function(df, models, title, outfile, width = 12, height = 4.5) {
  models <- intersect(models, as.character(unique(df$model)))
  if (!length(models)) return(invisible(NULL))

  df <- df %>% dplyr::filter(as.character(model) %in% models)
  labs_m <- model_labels[models]
  cols_m <- model_cols[models]

  obs <- df %>%
    dplyr::distinct(target_month, lead, lead_lab, observed) %>%
    dplyr::transmute(
      target_month,
      lead,
      lead_lab,
      series = "Observed",
      value = observed
    )

  pred <- df %>%
    dplyr::transmute(
      target_month,
      lead,
      lead_lab,
      series = as.character(model_lab),
      value = pred_mean
    )

  vs <- dplyr::bind_rows(obs, pred) %>%
    dplyr::mutate(
      series = factor(series, levels = c("Observed", unname(labs_m)))
    )

  line_cols <- c("Observed" = "#000000", setNames(cols_m, labs_m))
  line_w <- c("Observed" = 0.7, setNames(rep(0.4, length(models)), labs_m))

  p <- ggplot(
    vs,
    aes(target_month, value, colour = series, linewidth = series, group = series)
  ) +
    geom_line(alpha = 0.65) +
    facet_wrap(vars(lead_lab), ncol = length(lead_breaks), scales = "free_y") +
    scale_colour_manual(values = line_cols, name = NULL) +
    scale_linewidth_manual(values = line_w, name = NULL) +
    labs(
      x = "Target month (month being forecast)",
      y = "Dengue cases",
      title = title,
      subtitle = "Black = observed; coloured = predictive mean from expanding-window fits"
    ) +
    common_origin_theme

  ggsave(file.path(out_dir, outfile), p, width = width, height = height, dpi = 150)
  invisible(p)
}

# National: sum across states within model×origin×lead×target
national <- scores %>%
  dplyr::filter(model %in% overlay_models) %>%
  dplyr::group_by(model, model_lab, origin, lead, lead_lab, target_month) %>%
  dplyr::summarise(
    observed = sum(observed, na.rm = TRUE),
    pred_mean = sum(pred_mean, na.rm = TRUE),
    crps = mean(crps, na.rm = TRUE),
    .groups = "drop"
  )

plot_vs_observed(
  national,
  overlay_models,
  "MX national (sum of Admin1): forecast vs observed by lead",
  "forecast_vs_observed_by_lead_national.png"
)

for (st in example_states) {
  st_df <- scores %>%
    dplyr::filter(rne_iso_code == st, model %in% overlay_models)
  plot_vs_observed(
    st_df,
    overlay_models,
    paste0(st, ": forecast vs observed by lead"),
    paste0("forecast_vs_observed_by_lead_", gsub("^MX-", "", st), ".png")
  )
}

plot_vs_observed(
  national,
  intersect(c("inla_climate", "inla_trends"), present_models),
  "MX national: climate vs Trends (full INLA)",
  "forecast_vs_observed_by_lead_national_climate_vs_trends.png"
)
plot_vs_observed(
  national,
  intersect(c("inla_hybrid_climate", "inla_hybrid_trends"), present_models),
  "MX national: climate vs Trends (brms→INLA hybrid)",
  "forecast_vs_observed_by_lead_national_hybrid.png"
)

# Save aggregated tables used for plots
readr::write_csv(lead_decay, file.path(out_dir, "lead_decay_CRPS.csv"))
readr::write_csv(origin_perf, file.path(out_dir, "origin_performance_CRPS.csv"))
readr::write_csv(rolling_data, file.path(out_dir, "origin_performance_rolling_CRPS.csv"))
readr::write_csv(national, file.path(out_dir, "forecasts_national_long.csv"))

message("CRPS forecast/performance plots written to ", out_dir)
)
