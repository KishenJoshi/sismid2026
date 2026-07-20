# Day 2 notebooks: Wikipedia + wastewater (Lane A / Lane B)

Same two-lane structure as the Day 1 sessions. Each stream has:

- **Lane A** (`*_.ipynb` without `_soln`): the prompts you give a coding agent
  (Codex / Claude Code / Antigravity CLI); you run the code it writes.
- **Lane B** (`*_soln.ipynb`): the agent's output captured, pre-filled, as a backup
  and reference. Each cell names the prompt that produced it.

Both lanes pull **live** and fall back to the cached snapshot in `../data/` so nobody is
blocked. Only `pandas`, `matplotlib`, and the standard library are needed (already in the
course `requirements.txt`); no `pytrends`.

| Stream | Notebooks | Live source | Cache (`../data/`) |
|--------|-----------|-------------|--------------------|
| Wikipedia pageviews (Dengue, en/es/pt) | `01_wikipedia_pageviews.ipynb`, `01_wikipedia_pageviews_soln.ipynb` | Wikimedia REST pageviews API (no key) | `wikipedia_dengue_pageviews_cached.csv` (120 months, 2016–2025) |
| Wastewater, Influenza A, Georgia | `02_wastewater_nwss.ipynb`, `02_wastewater_nwss_soln.ipynb` | CDC NWSS Socrata dataset `ymmh-divb` | `cdc_nwss_influenza_a_ga_cached.csv` (7,093 samples, 27 sites, through Jul 2026) |

Both caches are **real snapshots** pulled from the live sources. Regenerate them by
re-running the fetch in the Lane B Step 0/Step 1 cells (they write over the same shape).

Teaching notes:
- **Wikipedia** is the *well-behaved* stream: public API, no key, no datacenter block,
  reproducible (pull twice, identical) — the deliberate contrast with Google Trends.
- **Wastewater** is a *different kind* of signal: biology (viral shedding), not behavior,
  so it can lead clinical reporting. Georgia flu-A ties to the capstone. Gotchas covered:
  single-site noise vs aggregate, changing site coverage, reporting lag.
