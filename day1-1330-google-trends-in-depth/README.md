# Day 1, 1:30 — Google Trends in depth

Instructor: **S. Yang + C. Djorno**. Afternoon lecture session. Mostly **talking through
the tricks and gotchas** of using Google Trends, grounded in two of our own papers. Less
live coding than the morning; the payoff is that students stop trusting a raw Google Trends
pull and know how to make it trustworthy.

> **Current deck: Candice's draft** (`slides/google-trends-in-depth.pdf`, 39 slides,
> "From Raw Search Data to Disease Forecasting and Population Insight"). This replaces the
> earlier auto-generated draft, which is archived in `slides/_draft-superseded/`. Candice
> holds the LaTeX source; it is not in this repo yet. **To be reconciled / updated later.**

## Roadmap (Candice's deck)

1. **What is Google Trends?** — the interface (interest over time, related/rising queries),
   how the data is generated (relative 0–100, sampled, privacy-thresholded), and practical
   pull settings (category, geography, time window, search type) that all redefine the
   normalization.
2. **Understanding Google Trends** — what the numbers really mean and the structural
   problems: missing values (zeros), sampling variability, noise, and pipeline drift.
3. **Statistical modeling of Google Trends** — the preprocessing recipe: combine related
   terms (Boolean "+" / clustering) to beat sparsity, prefer **topics** over raw strings,
   **smoothing-spline denoising** (evaluated by SNR), and **detrending** (ADF test; mean
   trend R² drops 0.82 → 0.14).
4. **Forecasting with Google Trends** — 1–4 week-ahead, state-level, out-of-sample flu
   **hospitalization** forecasts over 2022–2024 (ARIMAX, Seasonal ARIMAX, ARGO, LightGBM,
   AdaBoost); raw vs. preprocessed Google Trends vs. a no-search baseline.
5. **Beyond individual time series** — the **three-stage temporal structure** of search
   behavior (Pre / Active / Post), replicated across Google Trends and clinician searches
   for flu and RSV.
6. **Conclusion.**

## The two papers behind it

- *Restoring the Forecasting Power of Google Trends with Statistical Preprocessing*
  (Djorno, Yang, et al., *International Journal of Forecasting*, 2026): the preprocessing
  recipe and the headline result (preprocessed search improves an ARIMAX flu forecast
  ~58% nationally / 24% at the state level; raw search *hurts*).
- *Public and Clinician Search Behavior Reveal a Common Three-Stage Temporal Structure of
  Respiratory Disease* (Djorno, Slavin, Yang, Meyer, Santillana, in progress): the
  Pre (leads 4–7 wk) / Active (~0) / Post (lags 3–5 wk) timeline.

## Contents

```
slides/
  google-trends-in-depth.pdf   Candice's compiled deck (39 slides) — CANONICAL
  _draft-superseded/           the earlier auto-generated draft, kept for reference
    google-trends-in-depth.tex
    old-draft-google-trends-in-depth.pdf
    figs/
README.md
```

## Building / editing

The current deck is a PDF from Candice; its LaTeX source is **not in this repo**. To edit,
get the source from Candice (or rebuild from the archived draft in `_draft-superseded/`,
whose `.tex` still compiles). This will be reconciled later.

## Arc

This session makes the search signal *trustworthy* (preprocessing) and *complete* (related
queries, stage composites). Both feed **ARGO** on Day 2: preprocessed, stage-grouped
search terms as predictors alongside the disease's own history.
