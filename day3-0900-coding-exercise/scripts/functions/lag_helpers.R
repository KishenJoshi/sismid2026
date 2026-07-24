# Lag / design helpers for ARGO-style monthly models.

AR_LAGS <- c(1L, 12L, 24L)
CLIMATE_LAGS <- 1:6

#' Add lag columns within each state for a numeric variable.
add_group_lags <- function(df, var, lags, id_col = "rne_iso_code", time_col = "month_start") {
  df <- df %>% dplyr::arrange(.data[[id_col]], .data[[time_col]])
  for (L in lags) {
    new_name <- paste0(var, "_lag", L)
    df <- df %>%
      dplyr::group_by(.data[[id_col]]) %>%
      dplyr::mutate(!!new_name := dplyr::lag(.data[[var]], n = L)) %>%
      dplyr::ungroup()
  }
  df
}

#' Standardise selected columns on the training rows only; apply to full df.
scale_with_train <- function(df, cols, train_idx) {
  for (col in cols) {
    mu <- mean(df[[col]][train_idx], na.rm = TRUE)
    sdv <- stats::sd(df[[col]][train_idx], na.rm = TRUE)
    if (is.na(sdv) || sdv == 0) sdv <- 1
    df[[paste0(col, "_z")]] <- (df[[col]] - mu) / sdv
  }
  df
}
