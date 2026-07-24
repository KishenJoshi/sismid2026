# 05_plot_acf_ccf.R
# Raw dual-axis series + ACF/CCF diagnostics for cases, climate, and Trends.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
source(file.path(root, "scripts/functions/paths.R"))
source(file.path(root, "scripts/functions/week_to_month.R"))
source(file.path(root, "scripts/functions/gt_topic_adm1_helpers.R"))

out_dir <- ensure_dir(DAY3_ROOT, "outputs", "diagnostics")

cases <- readr::read_csv(file.path(DAY3_ROOT, "data/mx_adm1_cases_monthly.csv"), show_col_types = FALSE)
climate <- readr::read_csv(file.path(DAY3_ROOT, "data/mx_adm1_climate_monthly.csv"), show_col_types = FALSE)

panel <- cases %>%
  dplyr::inner_join(climate, by = c("rne_iso_code", "year", "month", "month_start"))

# Optional Trends panel if downloads exist.
trends_monthly <- NULL
dl_root <- file.path(DAY3_ROOT, "downloads")
if (dir.exists(dl_root) && length(list.dirs(dl_root, recursive = FALSE))) {
  tryCatch({
    trends_raw <- load_gtrends_adm1_panel(dl_root)
    # If already monthly (long timeframe), average by calendar month; else day-split.
    span_days <- as.numeric(diff(range(trends_raw$date, na.rm = TRUE)))
    if (span_days > 5 * 365) {
      trends_monthly <- trends_raw %>%
        dplyr::mutate(
          year = as.integer(format(date, "%Y")),
          month = as.integer(format(date, "%m")),
          month_start = as.Date(sprintf("%04d-%02d-01", year, month))
        ) %>%
        dplyr::group_by(geo, topic_label, year, month, month_start) %>%
        dplyr::summarise(hits = mean(hits, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(rne_iso_code = geo)
    } else {
      trends_monthly <- trends_weekly_to_monthly(trends_raw) %>%
        dplyr::rename(rne_iso_code = geo)
    }
    readr::write_csv(trends_monthly, file.path(DAY3_ROOT, "data/mx_adm1_gtrends_monthly.csv"))
  }, error = function(e) {
    message("Trends not available yet: ", conditionMessage(e))
  })
}

#----- Helpers
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
  ggplot(df, aes(x = month_start)) +
    geom_line(aes(y = .data[[y1]], colour = "cases"), linewidth = 0.4) +
    geom_line(aes(y = .y2_scaled, colour = y2), linewidth = 0.4, alpha = 0.85) +
    scale_y_continuous(
      name = "cases",
      sec.axis = sec_axis(~ (. - mid1) / scale + mid2, name = y2)
    ) +
    scale_colour_manual(values = c(cases = "#1b9e77", setNames("#d95f02", y2))) +
    labs(title = title, x = NULL, colour = NULL) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")
}

#----- Pooled ACF of cases
acf_cases <- panel %>%
  dplyr::group_by(rne_iso_code) %>%
  dplyr::group_modify(~ {
    d <- acf_df(.x$cases)
    if (is.null(d)) return(tibble::tibble())
    d
  }) %>%
  dplyr::ungroup()

if (nrow(acf_cases)) {
  p_acf <- ggplot(acf_cases, aes(lag, acf)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_col(width = 0.7, fill = "#3182bd") +
    facet_wrap(~rne_iso_code) +
    labs(title = "ACF of monthly dengue cases by state", x = "Lag (months)", y = "ACF") +
    theme_bw(base_size = 9)
  ggsave(file.path(out_dir, "acf_cases_by_state.png"), p_acf, width = 14, height = 10, dpi = 120)
}

# National (sum) ACF
nat <- panel %>%
  dplyr::group_by(month_start) %>%
  dplyr::summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop")
acf_nat <- acf_df(nat$cases)
if (!is.null(acf_nat)) {
  p_acf_nat <- ggplot(acf_nat, aes(lag, acf)) +
    geom_hline(yintercept = 0) +
    geom_col(fill = "#3182bd", width = 0.7) +
    labs(title = "ACF of national (summed) monthly dengue cases", x = "Lag (months)") +
    theme_bw()
  ggsave(file.path(out_dir, "acf_cases_national.png"), p_acf_nat, width = 7, height = 4, dpi = 120)
}

#----- CCF vs climate (pooled across states via within-state then average)
ccf_climate <- list()
for (pred in c("mean_temp_celsius", "total_precip_mm")) {
  tmp <- panel %>%
    dplyr::group_by(rne_iso_code) %>%
    dplyr::group_modify(~ {
      d <- ccf_df(.x$cases, .x[[pred]])
      if (is.null(d)) return(tibble::tibble())
      d
    }) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(lag) %>%
    dplyr::summarise(ccf = mean(ccf, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(predictor = pred)
  ccf_climate[[pred]] <- tmp
}
ccf_clim_df <- dplyr::bind_rows(ccf_climate)
if (nrow(ccf_clim_df)) {
  p_ccf_clim <- ggplot(ccf_clim_df, aes(lag, ccf)) +
    geom_hline(yintercept = 0) +
    geom_col(fill = "#e6550d", width = 0.7) +
    facet_wrap(~predictor, ncol = 1) +
    labs(
      title = "Mean CCF: cases vs climate (average across states)",
      subtitle = "Positive lag = climate leads cases",
      x = "Lag (months)", y = "CCF"
    ) +
    theme_bw()
  ggsave(file.path(out_dir, "ccf_cases_climate.png"), p_ccf_clim, width = 7, height = 6, dpi = 120)
}

#----- Raw dual-axis plots (example states)
example_states <- intersect(c("MX-JAL", "MX-VER", "MX-YUC", "MX-NLE"), unique(panel$rne_iso_code))
for (st in example_states) {
  d <- panel %>% dplyr::filter(rne_iso_code == st)
  for (pred in c("mean_temp_celsius", "total_precip_mm")) {
    p <- dual_axis_plot(d, "cases", pred, paste0(st, ": cases vs ", pred))
    if (!is.null(p)) {
      ggsave(
        file.path(out_dir, paste0("raw_dual_", st, "_", pred, ".png")),
        p, width = 9, height = 4, dpi = 120
      )
    }
  }
}

#----- Trends CCF / raw if available
if (!is.null(trends_monthly) && nrow(trends_monthly)) {
  topics <- unique(trends_monthly$topic_label)
  ccf_tr <- list()
  for (tp in topics) {
    tr <- trends_monthly %>% dplyr::filter(topic_label == tp)
    joined <- panel %>%
      dplyr::inner_join(
        tr %>% dplyr::select(rne_iso_code, month_start, hits),
        by = c("rne_iso_code", "month_start")
      )
    tmp <- joined %>%
      dplyr::group_by(rne_iso_code) %>%
      dplyr::group_modify(~ {
        d <- ccf_df(.x$cases, .x$hits)
        if (is.null(d)) return(tibble::tibble())
        d
      }) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(lag) %>%
      dplyr::summarise(ccf = mean(ccf, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(topic = tp)
    ccf_tr[[tp]] <- tmp

    # Raw dual-axis for one example state
    st <- example_states[[1]]
    d <- joined %>% dplyr::filter(rne_iso_code == st)
    if (nrow(d)) {
      p <- dual_axis_plot(d, "cases", "hits", paste0(st, ": cases vs ", tp))
      if (!is.null(p)) {
        ggsave(
          file.path(out_dir, paste0("raw_dual_", st, "_", tp, ".png")),
          p, width = 9, height = 4, dpi = 120
        )
      }
    }
  }
  ccf_tr_df <- dplyr::bind_rows(ccf_tr)
  if (nrow(ccf_tr_df)) {
    p_ccf_tr <- ggplot(ccf_tr_df, aes(lag, ccf)) +
      geom_hline(yintercept = 0) +
      geom_col(fill = "#756bb1", width = 0.7) +
      facet_wrap(~topic) +
      labs(title = "Mean CCF: cases vs Google Trends topics", x = "Lag (months)", y = "CCF") +
      theme_bw(base_size = 9)
    ggsave(file.path(out_dir, "ccf_cases_gtrends.png"), p_ccf_tr, width = 14, height = 10, dpi = 120)
  }
}

message("Diagnostics written to ", out_dir)
