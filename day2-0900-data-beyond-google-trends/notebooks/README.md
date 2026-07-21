# Day 2 notebooks: Google Trends (API) + Wikipedia + wastewater (Lane A / Lane B)

Same two-lane structure as the Day 1 sessions. Each stream has:

- **Lane A** (`*_.ipynb` without `_soln`): the prompts you give a coding agent
  (Codex / Claude Code / Antigravity CLI); you run the code it writes.
- **Lane B** (`*_soln.ipynb`): the agent's output captured, pre-filled, as a backup
  and reference. Each cell names the prompt that produced it.

Both lanes pull **live** and fall back to the cached snapshot in `../data/` so nobody is
blocked. Only `pandas`, `matplotlib`, and the standard library are needed (already in the
course `requirements.txt`); no `pytrends`.

**Run `00_` first.** It replaces yesterday's `pytrends` scraping with the Google Trends
API: same normalized 0-100 signal, but no HTTP 429s and identical values on every pull.
Unlock the key in the terminal first with `source scripts/unlock-gt-api-key.sh`, then
restart the kernel. Without a key it falls back to the cached snapshots.

| Stream | Notebooks | Live source | Cache (`../data/`) |
|--------|-----------|-------------|--------------------|
| **Google Trends revisited (API)** | `00_google_trends_api.ipynb`, `00_google_trends_api_soln.ipynb` | Google Trends API `getGraph` (**needs `GT_API`**) | `gt_api_dengue_mx_cached.csv` (265 weekly points), `gt_api_flu_term_vs_topic_cached.csv` (4 countries) |
| Wikipedia pageviews (Dengue, en/es/pt) | `01_wikipedia_pageviews.ipynb`, `01_wikipedia_pageviews_soln.ipynb` | Wikimedia REST pageviews API (no key) | `wikipedia_dengue_pageviews_cached.csv` (120 months, 2016–2025) |
| Wastewater, Influenza A, Georgia | `02_wastewater_nwss.ipynb`, `02_wastewater_nwss_soln.ipynb` | CDC NWSS Socrata dataset `ymmh-divb` | `cdc_nwss_influenza_a_ga_cached.csv` (7,093 samples, 27 sites, through Jul 2026) |

Both caches are **real snapshots** pulled from the live sources. Regenerate them by
re-running the fetch in the Lane B Step 0/Step 1 cells (they write over the same shape).

Teaching notes:
- **Google Trends (API)** is the *revisit*: students met pytrends' 429s and resampling
  yesterday. Measured payoff: two API pulls are byte-identical, and the term-vs-topic table
  shows the English term `flu` is nearly silent abroad (France max 3, mean 0.9) while topic
  `/m/0cycc` tracks the season everywhere (max 100, mean 16.3). That is what makes the
  multi-country work possible.
- **Wikipedia** is the *well-behaved* stream: public API, no key, no datacenter block,
  reproducible (pull twice, identical) — the deliberate contrast with Google Trends.
- **Wastewater** is a *different kind* of signal: biology (viral shedding), not behavior,
  so it can lead clinical reporting. Georgia flu-A ties to the capstone. Gotchas covered:
  single-site noise vs aggregate, changing site coverage, reporting lag.
