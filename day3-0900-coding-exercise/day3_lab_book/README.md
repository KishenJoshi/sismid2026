# Day 3 lab book — national dengue models (Mexico)

**Exercise:** `day3-0900-coding-exercise`\
**Focus:** National monthly dengue incidence, climate, and Google Trends\
**Prior PPC:** `scripts/05_national_prior_predictive_checks.R`\
**Forecast eval:** `scripts/06_national_rolling_forecast_eval.R`\
**Helpers:** `scripts/functions/05_national_inla_helpers.R`\
**Panel:** `data/mx_national_exposure_response_monthly.csv`\
**Engine:** **INLA** (negative binomial; no spatial BYM2 at national level)\
**Inspired by:** Yang, Santillana & Kou (2015), *PNAS* — ARGO (AutoRegression with GOogle search data)

------------------------------------------------------------------------

## 1. Modelling aims

Fit and compare negative-binomial count models for national Mexico dengue that:

1.  Use **autoregression + 12-month seasonality** as the structured baseline\
2.  Compare **climate** structures: DLNM and RW2 (lags 1–6), plus **single lag-1** temp / precip screens\
3.  Screen **one Google Trends series at a time** using the **raw lag-1** hits series\
4.  Use **penalised complexity (PC) priors** (and informative Normals where noted), implemented in INLA\
5.  Support **prior predictive checks** (Monte Carlo from priors + NegBin noise) and **expanding-window 1-step-ahead** forecast evaluation

**Deferred (not in this repo grid yet):** joint climate + selected Trends model; backward WAIC term contribution; dynamic final-model pipeline.

Scale-up path: same formulae at ADM1 later (geo filter / spatial effects / BYM2).

------------------------------------------------------------------------

## 2. Data and design conventions

| Item | Choice |
|------------------------------------|------------------------------------|
| Outcome | Monthly dengue **counts** $Y_t$ |
| Offset | $\log(\mathrm{population}_t / 10^5)$ → $\mathrm{e}^{\eta_t}$ is expected **incidence per 100,000** |
| Likelihood | INLA negative binomial (`family = "nbinomial"`) |
| Engine | INLA; **no BYM2** (national series) |
| Time | Monthly; Trends window binds overlap (\~2004–2022 with panel) |
| Case AR / seasonality | Lags **1** and **12** on all models |
| Climate exposures | Mean temperature (°C), total precipitation (mm) |
| Climate lags | **1–6** months (`rw2`, `dlnm`); **lag 1 only** for `climate_temp_lag1` / `climate_precip_lag1` |
| Google Trends lags | **lag 1 only** (raw standardised series; no lag 0; no GT smooth basis) |
| Retained Trends | `fumigation`, `insecticide`, `mosquito`, `public_health` |
| Predictors | Standardised using **training-window** mean/SD (`*_z`) |
| Forecast protocol (`06`) | Expanding window from **2015-01**; score **2020–2022**; **1-step** ahead |

### Shared observation mode

$$
Y_t \sim \mathrm{NegBin}(\mu_t, \phi),
\qquad
\log \mu_t = \eta_t + \log(\mathrm{pop}_t / 10^5).
$$

Implemented in INLA as `family = "nbinomial"` with $\mu_t = (\mathrm{pop}_t / 10^5)\,\mathrm{e}^{\eta_t}$ and size $\phi$ (larger $\phi$ → closer to Poisson). Code stores `log_pop = log(population / 1e5)` and uses `offset(log_pop)`.

------------------------------------------------------------------------

## 3. Priors (PC + informative Normals)

INLA fixed effects only accept **Gaussian** priors in `control.fixed`. Most slopes use **PC tail** statements matched to zero-mean Normals; AR lags and GT / single-climate lag-1 slopes use **informative** Normals (not PC).

**Two-sided PC** (default):

$$
\mathrm{P}(|\beta| > u) = \alpha
\quad\Rightarrow\quad
\mathrm{sd} = u / z_{1-\alpha/2}.
$$

with $\alpha = 0.05$.

### 3.1 Current defaults

| Parameter | Prior statement | INLA implementation |
|------------------------|------------------------|------------------------|
| Intercept $\alpha$ | $\mathrm{P}(\lvert\alpha\rvert > 1.5) = 0.05$ | `prec.intercept` from matched Normal SD |
| AR lag-1 | $\beta_1 \sim \mathcal{N}(1,\ 2^2)$ (informative) | `mean = 1`, `prec = 1/4` on `monthly_cases_lag1_z` |
| Seasonal lag-12 | $\beta_{12} \sim \mathcal{N}(1,\ 1^2)$ (informative) | `mean = 1`, `prec = 1` on `monthly_cases_lag12_z` |
| Climate DLNM basis slopes | $\mathrm{P}(\lvert\beta\rvert > 0.8) = 0.05$ | named `prec` on `cb_temp_*` / `cb_precip_*` |
| Climate RW2 lag fields | $\mathrm{P}(\sigma > 1) = 0.05$ (temp & precip) | `pc.prec` on each `f(..., model = "rw2")` |
| Climate lag-1 slopes (temp / precip) | $\beta \sim \mathcal{N}(1,\ 0.5^2)$ (informative; same as GT) | named `mean` / `prec` on `mean_temp_celsius_lag1_z` / `total_precip_mm_lag1_z` |
| GT lag-1 slopes | $\beta \sim \mathcal{N}(1,\ 0.5^2)$ (informative) | named `mean` / `prec` on `gt_*_lag1_z` |
| NegBin size $\phi$ | $\mathrm{P}(1/\phi > 7) = 0.05$ | `pc.mgamma` (see note) |

**NegBin `pc.mgamma`:** statement is $\mathrm{P}(1/\phi > 7)=0.05$. INLA **24.12** (R 4.4 Windows binary) expects a **single** rate parameter $\lambda = -\log(\alpha)/u$; newer INLA builds accept `param = c(u, \alpha)`. Helpers detect `nparameters` and set the prior accordingly.

Script writes `pc_prior_summary.txt` on each PPC run.

### 3.2 Prior predictive checks (seasonal-drivers style)

1.  Draw intercept / FE slopes / RW2 lag fields (for `climate_rw2`) / NegBin size from the **same priors** used in fitting\
2.  Form $\log\mu_t = \alpha + x_t^\top\beta + Z_t^{\mathrm{temp}}\delta^{\mathrm{temp}} + Z_t^{\mathrm{ppt}}\delta^{\mathrm{ppt}} + \log(\mathrm{pop}_t/10^5)$ (RW2 terms only when present)\
3.  Sample $Y_t^{\mathrm{rep}} \sim \mathrm{NegBin}(\mu_t, \phi)$\
4.  Ribbons (50% / 95%) + **coverage_95** vs observed counts

Primary script: `05_national_prior_predictive_checks.R` → `results/05_test_inla_models_national/prior_predictive/`.

------------------------------------------------------------------------

## 4. Building blocks

### 4.1 Autoregression + seasonality

$$
\eta^{\mathrm{AR}}_t
=
\alpha
+ \beta_1\, y_{t-1}^{\mathrm{z}}
+ \beta_{12}\, y_{t-12}^{\mathrm{z}}.
$$

### 4.2 Google Trends — single series, lag 1

One retained series at **lag 1 only** (no smooth lag basis — avoids under-determined P-splines on short lag grids):

$$
\eta^{\mathrm{GT},k}_t
=
\gamma_k\, G_{k,t-1}^{\mathrm{z}}.
$$

Model IDs: `gt_fumigation_lag1`, `gt_insecticide_lag1`, `gt_mosquito_lag1`, `gt_public_health_lag1`.

### 4.3 Climate — single lag-1 screens

$$
\eta^{\mathrm{temp}}_t = \beta_T\, T_{t-1}^{\mathrm{z}},
\qquad
\eta^{\mathrm{ppt}}_t = \beta_P\, P_{t-1}^{\mathrm{z}}.
$$

Same informative prior as GT lag-1: $\mathcal{N}(1, 0.5^2)$.

### 4.4 Climate — true RW2 on lags (`rw2`)

Linear in **standardised** lagged temp/precip ($T_{t-\ell}^{\mathrm{z}}$, $P_{t-\ell}^{\mathrm{z}}$, $\ell=1,\ldots,6$). Lag coefficients are INLA latent fields:

$$
\eta^{\mathrm{clim}}_t
=
\sum_{\ell=1}^{6} \delta^{\mathrm{temp}}_{\ell}\, T_{t-\ell}^{\mathrm{z}}
+
\sum_{\ell=1}^{6} \delta^{\mathrm{ppt}}_{\ell}\, P_{t-\ell}^{\mathrm{z}},
\quad
\delta^{\cdot} \sim \mathrm{RW2}(\tau),\ \mathrm{P}(\sigma>1)=0.05
$$

with `scale.model = TRUE` and sum-to-zero on each field (level in the intercept). Fitted via `inla.stack` $A$-matrices (not dlnm P-spline FE columns).

### 4.5 Climate — nonlinear DLNM (`dlnm`)

Nonlinear exposure × lag bake-off (temp / precip / lag `ns` **df = 2** over lags 1–6); knots frozen on the full series.

------------------------------------------------------------------------

## 5. Models tested (full grid)

Display / fit order:

| Model ID                | Contents                 |
|-------------------------|--------------------------|
| `ar`                    | AR + seasonal only       |
| `climate_dlnm`          | AR + climate dlnm        |
| `climate_rw2`           | AR + climate rw2         |
| `climate_temp_lag1`     | AR + mean temp lag 1     |
| `climate_precip_lag1`   | AR + total precip lag 1  |
| `gt_fumigation_lag1`    | AR + fumigation lag 1    |
| `gt_insecticide_lag1`   | AR + insecticide lag 1   |
| `gt_mosquito_lag1`      | AR + mosquito lag 1      |
| `gt_public_health_lag1` | AR + public_health lag 1 |

------------------------------------------------------------------------

## 6. Evaluation

| Stage | Metric |
|------------------------------------|------------------------------------|
| Prior PPC | Ribbons + coverage_95 on **counts** (`05_…`) |
| National forecast eval | CRPS, WIS, 95% coverage; CRPSS/WISS vs `ar` (`06_…`) |
| Seasonal skill | Mean CRPS by calendar month × horizon |
| Sensitivity tables | Same metrics with calendar-year **2020** targets dropped (`*_excl_2020`) |
| INLA diagnostics | `mode.status`, CPO failures, `mlik` (no WAIC) |

### 6.1 Forecast protocol (`06_national_rolling_forecast_eval.R`)

-   Train start **2015-01**; score targets **2020-01 … 2022-12**
-   Expanding window: fit on `[2015-01, origin]`, recursive horizon $h=1$ only
-   Generation: `inla.posterior.sample` → NegBin draws (needs package **`sn`**)
-   Parallel over origins (`--workers`); optional **progressr** bar
-   Plots: CRPS/WIS by target (incidence strip on top); seasonal CRPS (seasonal incidence profile on top); forecast timeseries in incidence and **log1p** incidence
-   Results: `results/06_national_rolling_forecast_eval/`

------------------------------------------------------------------------

## 7. Pipeline map

| Step | Script | Role |
|------------------------|------------------------|------------------------|
| EDA / term retention | `04_national_exploratory_analysis.R` | Drop list |
| Prior bake-off | `05_national_prior_predictive_checks.R` | PPC for full model grid |
| Forecast eval | `06_national_rolling_forecast_eval.R` | CRPS/WIS/coverage + plots |

------------------------------------------------------------------------

## 8. Open decisions / next lab entries

-   Inspect prior ribbons for true climate RW2 (`pc.prec`, $u=1$) vs DLNM vs lag-1 screens\
-   Joint multi-horizon latent only if returning to $h>1$\
-   **Later:** climate + selected GT; dynamic pipeline\
-   Scale to ADM1 (BYM2)

------------------------------------------------------------------------

## 9. Changelog

| Date | Entry |
|------------------------------------|------------------------------------|
| 2026-08-08 | Initial lab book (brms grid) |
| 2026-08-09 | Drop indep + multi-GT; per-term `gt_*_rw2` |
| 2026-08-09 | Switch **brms → INLA**; PC priors; seasonal-drivers-style PPC |
| 2026-08-09 | Prior tweak: intercept $u=1.5$; lag-12 one-sided $P(\beta>1.5)=0.05$; basis $u=2$; keep NegBin $P(1/\phi>7)=0.05$ |
| 2026-08-09 | GT screens: drop rw2/crossbasis; use raw lag-1 only (`gt_*_lag1`) to avoid P-spline df error on short lag grids |
| 2026-08-09 | AR lag-1 $\sim\mathcal{N}(1,1^2)$ (informative); NegBin $P(1/\phi>7)=0.05$ (after $u=4$ trial was too tight) |
| 2026-08-09 | Split slopes: climate basis $u=1.2$; GT lag-1 kept at $u=1.5$ |
| 2026-08-09 | Intercept PC $u=1.5$; climate rw2 lag basis `df = 2` (was 4) |
| 2026-08-09 | `climate_rw2`: true INLA RW2 on standardised lag coefs; `pc.prec` $P(\sigma>1)=0.05$ (option B); keep `climate_dlnm` bake-off |
| 2026-08-09 | AR lag-1 $\sim\mathcal{N}(1,2^2)$; lag-12 $\sim\mathcal{N}(1,1^2)$ (drop PC on seasonal lag) |
| 2026-08-09 | GT lag-1 PC tightened: $P(\lvert\beta\rvert>1.2)=0.05$ (was 1.5) |
| 2026-08-09 | AR lag-1 sd $2\to 1.25$; lag-12 sd $1\to 0.5$; NegBin $P(1/\phi>6)=0.05$ |
| 2026-08-09 | AR lag-1 sd $1.25\to 1.5$; lag-12 sd $0.5\to 1$ (means stay at 1); keep NegBin $u=6$ |
| 2026-08-09 | NegBin $u$ reverted $6\to 7$; AR stays $\mathcal{N}(1,1.5^2)$ / $\mathcal{N}(1,1^2)$ |
| 2026-08-09 | AR lag-1 sd $1.5\to 2$; lag-12 stays $\mathcal{N}(1,1^2)$; NegBin $u=7$ |
| 2026-08-09 | GT + DLNM basis PC $u$: $1.2\to 1.1$ |
| 2026-08-09 | DLNM dfs: temp/precip/lag all set to 2 (was 3/2/3) |
| 2026-08-09 | GT lag-1: drop PC; informative $\mathcal{N}(1,0.25^2)$, then widened to $\mathcal{N}(1,0.5^2)$ |
| 2026-08-09 | DLNM basis PC $u$: $1.1\to 0.8$ (GT unchanged) |
| 2026-08-09 | Add `06_national_rolling_forecast_eval.R`: expanding 2015→; score 2020–2022; CRPS/WIS/coverage; CRPSS vs `ar`; calendar-month CRPS; parallel origins |
| 2026-08-09 | Sync lab book to code: offset $\log(\mathrm{pop}/10^5)$; add `climate_temp_lag1` / `climate_precip_lag1`; GT+climate lag-1 $\mathcal{N}(1,0.5^2)$; `06` horizon $h=1$; `*_excl_2020` metrics; rename PPC script; `pc.mgamma` version note |
