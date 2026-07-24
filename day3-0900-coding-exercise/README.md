# Day 3 coding exercise: Mexico adm1 dengue forecasts with climate + vector-control Google Trends

Assess whether vector-control-related Google Trends topics improve monthly dengue forecasts for Mexico's 32 Admin1 states, relative to climate-only Bayesian baselines.

## Design (locked)

- **Outcome:** monthly dengue cases (weekly OpenDengue day-split across month boundaries)
- **Climate:** monthly mean temperature + total precipitation from ERA5 daily extracts in `dengue_seasonal_drivers`
- **Population offset:** `log(population)` from seasonal-drivers GHS-based `ADM1_incidence.csv`
- **Spatial:** Mexico-only k=10 inverse-distance KNN INLA graph (at-risk pop-weighted centroids)
- **Trends:** one topic/term × one `MX-XX` geo at a time (independent 0–100 scaling)
- **AR / seasonality:** ARGO-style lags **1, 12, 24** for cases (and for search terms in Trends models)
- **Models:** INLA (PC priors + BYM2) climate vs Trends; brms horseshoe screen → spatial INLA (climate and Trends families each have a climate-only baseline)
- **Evaluation:** expanding-window refits with all data through each origin; leads 1–6 months; **CRPS** + INLA WAIC/CPO/mode diagnostics

## Setup

From this directory:

```bash
Rscript scripts/01_prepare_mx_cases_monthly.R
Rscript scripts/02_prepare_mx_climate_monthly.R
Rscript scripts/03_prepare_mx_spatial_graph.R
Rscript scripts/04_download_gtrends_topics_adm1.R          # sequential, 20s sleep; resume-safe
# smoke test: Rscript scripts/04_download_gtrends_topics_adm1.R --max-queries 2
Rscript scripts/05_plot_acf_ccf.R
Rscript scripts/06_fit_inla_baseline_and_trends.R
Rscript scripts/07_fit_brms_screen_then_inla.R
Rscript scripts/08_evaluate_expanding_window_crps.R
# smoke test: Rscript scripts/08_evaluate_expanding_window_crps.R --max-origins 2
```

Requires: `dplyr`, `tidyr`, `readr`, `data.table`, `ggplot2`, `sf`, `terra`, `spdep`, `INLA`, `rnaturalearth`, `gtrendsR`, `httr`, `jsonlite`, `brms` (for script 07), `purrr`, `tibble`.

**Data note:** OpenDengue Mexico Admin1 weekly cases are **not continuous** (gaps include 2008–2011 and 2014–2016). Modelling and expanding-window evaluation use the longest recent continuous block **2017–2022** (origins from 2019 for AR(24) burn-in). Gap months are listed in `data/mx_adm1_cases_month_gaps.csv`.

External data root (auto-detected): `.../Project/dengue_seasonal_drivers`.

## Topics / terms scraped

**Topics:** Aedes, Aedes aegypti, mosquito, vector, vector control, space spraying, dengue, dengue control, standing water, source management, insecticide, fogging, larvicide, fumigation, breeding site, insect repellent, epidemic, public health, water storage

**Spanish terms:** `control del dengue`, `control de vectores`
