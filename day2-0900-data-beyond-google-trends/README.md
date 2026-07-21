# Day 2, 9:00 — Data beyond Google Trends

Instructor: **S. Yang + C. Djorno**. Picks up the Day 1 agent-scraping workflow and applies
it, in depth, to two new streams that fail *differently* from search.

## What this session covers

- **Google Trends revisited: the API (do this first).** Yesterday's `pytrends` scraping is
  replaced by the official **Google Trends API** (`getGraph`): the **same normalized 0-100
  index**, but no HTTP 429s and **identical values on every pull** (verified). Unlock with
  `source scripts/unlock-gt-api-key.sh`, then restart the kernel. Includes the
  **term vs topic** language lesson, which is what makes today's multi-country work
  possible. See [`../docs/google-trends-api.md`](../docs/google-trends-api.md).
  - It fixes *delivery*, not the signal: search is still attention rather than infection,
    still zero-heavy in small places, and still drifts. Which is why we add other streams.

- **Wikipedia pageviews** — a free, hourly, official Wikimedia REST API (no key). A
  complementary, **multi-language** access signal: the Spanish/Portuguese/English articles
  give country-level and non-English signal where Google Trends is thin, and unlike Google
  Trends the series is **stable and reproducible**. Scraping prompt + gotchas (titles by
  language, redirects, access/agent filters, attention spikes, disambiguation).
- **Wastewater surveillance (CDC NWSS)** — a fundamentally different signal: it measures
  **biology (shedding)**, not behavior, so it is independent of search/care-seeking and can
  lead clinical reporting. Scraping prompt + gotchas (site coverage/gaps, flow/PMMoV
  normalization, single-site noise vs aggregate, reporting lag). Assess its lead time vs ILI
  honestly (Santillana wastewater preprint).
- **Continuing the Day 1 workflow** — same five-step loop, same verify habit, same Plan B /
  cache. Wikipedia and CDC are well-behaved (no datacenter block), so retries/proxy rarely
  come up here; Google Trends was the hard case. Ends with a small **multi-stream bundle**
  (Google Trends + Wikipedia + wastewater vs a reference) as raw material for the capstone.

These streams were previously previewed in the Day 1 3:30 session; that session is now
Google-Trends-only ("get everyone started"), and the depth lives here.

## Contents

```
slides/
  data-beyond-google-trends.tex   Beamer deck (metropolis theme)
  data-beyond-google-trends.pdf   compiled slides
notebooks/
  00_google_trends_api.ipynb          Lane A: prompts (Google Trends API revisit)
  00_google_trends_api_soln.ipynb     Lane B: worked solution
  01_wikipedia_pageviews.ipynb        Lane A: prompts you give the agent
  01_wikipedia_pageviews_soln.ipynb   Lane B: agent output captured (worked solution)
  02_wastewater_nwss.ipynb            Lane A: prompts
  02_wastewater_nwss_soln.ipynb       Lane B: worked solution
  README.md                           what the notebooks are + the data table
data/
  gt_api_dengue_mx_cached.csv             real: 265 weekly points, dengue MX (API)
  gt_api_flu_term_vs_topic_cached.csv     real: term vs topic, 4 countries (API)
  wikipedia_dengue_pageviews_cached.csv   real snapshot (120 months, en/es/pt)
  cdc_nwss_influenza_a_ga_cached.csv      real snapshot (7,093 GA flu-A samples, 27 sites)
README.md
```

## Build the PDF

```bash
cd slides && pdflatex data-beyond-google-trends.tex && pdflatex data-beyond-google-trends.tex
```

## Arc

Day 1 taught the method on Google Trends. This session is "more streams through the same
loop." Day 2 late morning adds mobility + news; Day 2 afternoon folds everything into ARGO;
Day 3 is bring-your-own-problem.

## Notebooks (built and verified)

Lane A (agent-prompt) and Lane B (worked-solution) notebooks for **Wikipedia pageviews** and
**CDC NWSS wastewater** are in `notebooks/`, with real cached snapshots in `data/`. Both
Lane B notebooks were executed end-to-end on the pinned Codespace environment (Python 3.12,
the course `requirements.txt`): **0 errors**, live pulls succeed with automatic cache
fallback, plots render inline. They need only `pandas`, `matplotlib`, and the standard
library (no `pytrends`). See `notebooks/README.md` for the stream-by-stream detail.
