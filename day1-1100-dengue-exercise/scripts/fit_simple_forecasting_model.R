# Fit three univariate GLM (log-link) weekly forecasts of dengue cases from
# Google Trends (train 2015-2017, test 2018-2019).
#
# Usage:
#   Rscript day1-1100-dengue-exercise/scripts/fit_simple_forecasting_model.R MX
#   Rscript day1-1100-dengue-exercise/scripts/fit_simple_forecasting_model.R BR

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(stringr)
  library(lubridate)
  library(patchwork)
})

source("day1-1100-dengue-exercise/scripts/functions/load_most_recent_file.R")
source("day1-1100-dengue-exercise/scripts/functions/geo_weekly_helpers.R")

geo <- parse_geo_arg("MX")
dat <- load_weekly_dengue_geo(geo)
train_df <- dat$train_df
test_df <- dat$test_df

message("Geo: ", geo)
message(
  "Train: ", min(train_df$start_date), " to ", max(train_df$start_date),
  " (", nrow(train_df), " weeks)"
)
message(
  "Test:  ", min(test_df$start_date), " to ", max(test_df$start_date),
  " (", nrow(test_df), " weeks)"
)

r2_score <- function(y, yhat) {
  1 - sum((y - yhat)^2, na.rm = TRUE) /
    sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
}

rmse <- function(y, yhat) {
  sqrt(mean((y - yhat)^2, na.rm = TRUE))
}

fit_one <- function(predictor, train_df, test_df) {
  fml <- as.formula(paste("dengue_total", "~", predictor))
  fit <- glm(fml, data = train_df, family = poisson(link = "log"))
  intercept <- unname(coef(fit)[["(Intercept)"]])
  slope <- unname(coef(fit)[[predictor]])

  train_y <- train_df$dengue_total
  train_hat <- as.numeric(fitted(fit))
  test_y <- test_df$dengue_total
  test_hat <- as.numeric(predict(fit, newdata = test_df, type = "response"))

  train_r2 <- r2_score(train_y, train_hat)
  train_cor <- cor(train_y, train_hat, use = "complete.obs")
  test_r2 <- r2_score(test_y, test_hat)
  test_cor <- cor(test_y, test_hat, use = "complete.obs")

  metrics <- bind_rows(
    tibble(
      geo = geo, predictor = predictor, set = "train",
      intercept = intercept, correlation = train_cor, R2 = train_r2,
      RMSE = rmse(train_y, train_hat)
    ),
    tibble(
      geo = geo, predictor = predictor, set = "test",
      intercept = NA_real_, correlation = test_cor, R2 = test_r2,
      RMSE = rmse(test_y, test_hat)
    )
  )

  x_grid <- seq(
    min(train_df[[predictor]], na.rm = TRUE),
    max(train_df[[predictor]], na.rm = TRUE),
    length.out = 100
  )
  curve_df <- tibble(
    !!predictor := x_grid,
    dengue_total = as.numeric(
      predict(fit, newdata = setNames(data.frame(x_grid), predictor), type = "response")
    )
  )

  p_train <- ggplot(train_df, aes(x = .data[[predictor]], y = dengue_total)) +
    geom_point(alpha = 0.75, colour = "#2c7fb8") +
    geom_line(
      data = curve_df,
      aes(x = .data[[predictor]], y = dengue_total),
      colour = "#e34a33",
      linewidth = 0.8
    ) +
    labs(
      title = paste0(predictor, " — train"),
      subtitle = sprintf(
        "log-link a=%.3f; R²=%.3f; cor=%.3f",
        intercept, train_r2, train_cor
      ),
      x = predictor,
      y = "Weekly dengue cases"
    ) +
    theme_minimal()

  p_test <- ggplot(
    tibble(predicted = test_hat, actual = test_y),
    aes(x = predicted, y = actual)
  ) +
    geom_point(alpha = 0.75, colour = "#2c7fb8") +
    geom_abline(intercept = 0, slope = 1, colour = "#e34a33", linewidth = 0.8) +
    labs(
      title = paste0(predictor, " — test"),
      subtitle = sprintf(
        "pred vs actual; a=%.3f, b=%.3f; cor=%.3f; R²=%.3f; RMSE=%.1f",
        intercept, slope, test_cor, test_r2, rmse(test_y, test_hat)
      ),
      x = "Predicted",
      y = "Actual"
    ) +
    theme_minimal()

  list(metrics = metrics, p_train = p_train, p_test = p_test)
}

results <- lapply(predictors, fit_one, train_df = train_df, test_df = test_df)
metrics_long <- bind_rows(lapply(results, `[[`, "metrics"))
print(as.data.frame(metrics_long), row.names = FALSE)

out_dir <- file.path(
  "day1-1100-dengue-exercise/outputs/fit_simple_forecasting_model",
  geo
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(metrics_long, file.path(out_dir, "metrics_long.csv"))

six_panel <- wrap_plots(
  results[[1]]$p_train, results[[1]]$p_test,
  results[[2]]$p_train, results[[2]]$p_test,
  results[[3]]$p_train, results[[3]]$p_test,
  ncol = 2
) +
  plot_annotation(
    title = paste0(geo, ": GLM (poisson, log link) weekly dengue forecasts"),
    subtitle = "Train 2015–2017 (left); test 2018–2019 (right)"
  )

ggsave(
  file.path(out_dir, "six_panel_train_test.png"),
  six_panel,
  width = 11,
  height = 12,
  dpi = 150
)
message("Wrote outputs under ", out_dir)
