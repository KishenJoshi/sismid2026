# Helpers for national / ADM1 exploratory diagnostics (ACF, CCF, Trends EDA).

ensure_dir <- function(...) {
  p <- file.path(...)
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

# Contemporaneous gt_* columns only (exclude *_lag*).
select_gt_cols <- function(df) {
  gt_all <- grep("^gt_", names(df), value = TRUE)
  gt_all[!grepl("_lag\\d+$", gt_all)]
}

acf_df <- function(x, lag.max = 24) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 10L) return(NULL)
  a <- stats::acf(x, lag.max = lag.max, plot = FALSE)
  data.frame(lag = as.numeric(a$lag), acf = as.numeric(a$acf))
}

ccf_df <- function(y, x, lag.max = 12) {
  y <- as.numeric(y)
  x <- as.numeric(x)
  ok <- is.finite(y) & is.finite(x)
  y <- y[ok]
  x <- x[ok]
  if (length(y) < 10L) return(NULL)
  c <- stats::ccf(y, x, lag.max = lag.max, plot = FALSE)
  data.frame(lag = as.numeric(c$lag), ccf = as.numeric(c$acf))
}

dual_axis_plot <- function(df, y1, y2, title) {
  r1 <- range(df[[y1]], na.rm = TRUE)
  r2 <- range(df[[y2]], na.rm = TRUE)
  if (!all(is.finite(r1)) || !all(is.finite(r2)) || diff(r2) == 0) return(NULL)
  scale <- diff(r1) / diff(r2)
  mid1 <- mean(r1)
  mid2 <- mean(r2)
  df$.y2_scaled <- (df[[y2]] - mid2) * scale + mid1
  ggplot2::ggplot(df, ggplot2::aes(x = .data$month_start)) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[y1]], colour = "cases"), linewidth = 0.4) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$.y2_scaled, colour = y2),
      linewidth = 0.4, alpha = 0.85
    ) +
    ggplot2::scale_y_continuous(
      name = "cases",
      sec.axis = ggplot2::sec_axis(~ (. - mid1) / scale + mid2, name = y2)
    ) +
    ggplot2::scale_colour_manual(values = c(cases = "#1b9e77", stats::setNames("#d95f02", y2))) +
    ggplot2::labs(title = title, x = NULL, colour = NULL) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
}

pivot_gtrends_long <- function(df, gt_cols = NULL) {
  if (is.null(gt_cols)) gt_cols <- select_gt_cols(df)
  if (!length(gt_cols)) stop("No contemporaneous gt_* columns found")
  df %>%
    dplyr::select(month_start, dplyr::all_of(gt_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(gt_cols),
      names_to = "term",
      values_to = "hits"
    ) %>%
    dplyr::mutate(
      term = sub("^gt_", "", term),
      hits = as.numeric(hits)
    )
}

summarise_gtrends_hits <- function(gt_long) {
  gt_long %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(
      n_months = dplyr::n(),
      n_missing = sum(is.na(hits)),
      n_zero = sum(!is.na(hits) & hits == 0),
      n_nonzero = sum(!is.na(hits) & hits > 0),
      pct_zero = round(100 * n_zero / n_months, 1),
      pct_nonzero = round(100 * n_nonzero / n_months, 1),
      unique_vals = dplyr::n_distinct(hits[!is.na(hits)]),
      mean_hits = round(mean(hits, na.rm = TRUE), 2),
      median_hits = round(stats::median(hits, na.rm = TRUE), 2),
      max_hits = max(hits, na.rm = TRUE),
      sd_hits = round(stats::sd(hits, na.rm = TRUE), 2),
      max_zero_streak_mo = {
        is_z <- !is.na(hits) & hits == 0
        r <- rle(is_z)
        if (any(r$values)) as.integer(max(r$lengths[r$values])) else 0L
      },
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(pct_nonzero), dplyr::desc(unique_vals), term)
}

plot_gtrends_hits_hist <- function(gt_long) {
  ggplot2::ggplot(gt_long, ggplot2::aes(x = hits)) +
    ggplot2::geom_histogram(bins = 30, fill = "#756bb1", colour = "white", linewidth = 0.2) +
    ggplot2::facet_wrap(~term, scales = "free_y") +
    ggplot2::labs(
      title = "Distribution of Google Trends hits by term (national, contemporaneous)",
      x = "Hits", y = "Months"
    ) +
    ggplot2::theme_bw(base_size = 11)
}

plot_gtrends_hits_timeseries <- function(gt_long, ncol = 2) {
  ggplot2::ggplot(gt_long, ggplot2::aes(x = month_start, y = hits)) +
    ggplot2::geom_line(linewidth = 0.35, colour = "#756bb1") +
    ggplot2::facet_wrap(~term, scales = "free_y", ncol = ncol) +
    ggplot2::labs(
      title = "Google Trends hit time series by term (national, contemporaneous)",
      x = NULL, y = "Hits"
    ) +
    ggplot2::theme_bw(base_size = 10)
}

# Long panel for faceted contemporaneous series (cases first via series factor levels).
pivot_series_long <- function(df, value_cols, labels = NULL) {
  if (is.null(labels)) labels <- value_cols
  if (length(labels) != length(value_cols)) {
    stop("labels must be the same length as value_cols")
  }
  label_map <- stats::setNames(labels, value_cols)
  df %>%
    dplyr::select(month_start, dplyr::all_of(value_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = "series",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      series = factor(unname(label_map[series]), levels = labels),
      value = as.numeric(value)
    )
}

# First calendar-year peak month of monthly_cases (skip years with max == 0).
seasonal_case_peaks <- function(df, cases_col = "monthly_cases") {
  df %>%
    dplyr::mutate(
      year = as.integer(format(month_start, "%Y")),
      cases = as.numeric(.data[[cases_col]])
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::filter(is.finite(cases), max(cases, na.rm = TRUE) > 0) %>%
    dplyr::slice(which.max(cases)) %>%
    dplyr::ungroup() %>%
    dplyr::select(year, month_start, cases) %>%
    dplyr::arrange(month_start)
}

plot_faceted_timeseries <- function(series_long, title, ylab = NULL,
                                    peak_dates = NULL, ncol = 1) {
  p <- ggplot2::ggplot(series_long, ggplot2::aes(x = month_start, y = value)) +
    ggplot2::geom_line(linewidth = 0.35, colour = "#3182bd")
  if (!is.null(peak_dates) && length(peak_dates)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = as.Date(peak_dates),
        colour = "#e6550d",
        linetype = "dashed",
        linewidth = 0.3,
        alpha = 0.75
      )
  }
  p +
    ggplot2::facet_wrap(~series, scales = "free_y", ncol = ncol) +
    ggplot2::labs(title = title, x = NULL, y = ylab) +
    ggplot2::theme_bw(base_size = 10)
}
