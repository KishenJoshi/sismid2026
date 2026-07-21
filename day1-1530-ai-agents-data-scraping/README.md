# Day 1, 3:30 — Using AI agents for data scraping

Instructor: **S. Yang + C. Djorno**. The "get everyone started" session, **focused on
Google Trends only**: this morning students scraped one topic (dengue in Mexico); here they
get comfortable scraping Google Trends for **their own** disease and place, with the agent
as the coding tool woven through the rest of the course. Other streams (Wikipedia,
wastewater, mobility, news) move to **Day 2**.

## What this session covers

- **The general recipe** — the reusable five-step loop (fetch → tidy → plot → sanity-check →
  save), the anatomy of a good scrape prompt, scraping Google Trends for your own topic
  (swap `geo` + terms), and verifying the agent's output.
- **Scraping vs an API (the reliable path).** `pytrends` scrapes an unofficial endpoint:
  HTTP 429s, and values that **resample on every pull**. The **Google Trends API**
  (`getGraph`) returns the **same normalized 0-100 index** from a documented API with a
  key: no 429s, and **identical numbers every pull**. It is a drop-in replacement, so
  nothing downstream changes. The key ships encrypted as `secrets/gt-api/GT_API.enc`; students unlock it in the terminal
  with `source scripts/unlock-gt-api-key.sh` (Google Trends passcode), which exports
  `GT_API`. See
  [`../docs/google-trends-api.md`](../docs/google-trends-api.md).
  - **Quota 1,000 requests/day, health topics only, never commit the key** (unlock with `source scripts/unlock-gt-api-key.sh`;
    only `.enc` blobs may be committed).
- **Term vs topic (the language lesson).** The API takes a literal term or a Knowledge
  Graph topic id (`/m/0cycc` = Influenza). Measured: the English term `flu` is nearly
  silent abroad (France max 3, mean 0.9) while the topic tracks the season everywhere
  (France max 100, mean 16.3), because it covers *grippe* / *influenza* / *gripe*. Use
  topics for cross-country work. Reference list: `data/flu_topic_ids.csv` (21 flu topics).
- **When scraping is blocked: proxy and VPN** — moved here from the 9:45 first-encounter
  session (it was too early there). The retry → proxy/VPN → cache ladder, what a proxy/VPN
  is in plain terms, and asking an agent to set one up. A class proxy on a residential
  connection is **built and verified** (tinyproxy on a WSL box, exposed via ngrok; a real
  Codespace routed through it scraped Google Trends successfully). Full setup and the
  current usage line are in
  [`proxy-setup.md`](proxy-setup.md).
- **Plan B** — pre-filled notebooks (captured agent output) and cached snapshots for every
  stream.

## Note on the proxy move

The 9:45 deck (`day1-0945-agent-coding-introduction`) previously carried three proxy/VPN
slides. Those were trimmed to a single retry + cache slide that points here; the full
proxy/VPN discussion now lives in this session, where students are scraping seriously.

## Contents

```
slides/
  ai-agents-data-scraping.tex   Beamer deck (metropolis theme)
  ai-agents-data-scraping.pdf   compiled slides
notebooks/
  01_scrape_your_topic.ipynb        Lane A: the prompts you give the agent
  01_scrape_your_topic_soln.ipynb   Lane B: the agent's output captured (worked solution)
data/
  google_trends_flu_us_cached.csv   real snapshot (262 weekly points, flu/US, 2021-2026)
proxy-setup.md                  build a class proxy/VPN on a spare machine (verified: tinyproxy+ngrok)
README.md
```

## Build the PDF

```bash
cd slides && pdflatex ai-agents-data-scraping.tex && pdflatex ai-agents-data-scraping.tex
```

## Two lanes (built)

- **Lane A** (`01_scrape_your_topic.ipynb`): one prompt per step (build `gt_api_fetch`, pull
  your topic, term-vs-topic across countries, reproducibility check, sanity-check, save).
  Paste each into Codex / Claude Code / Antigravity CLI and run its output.
- **Lane B** (`01_scrape_your_topic_soln.ipynb`): the captured worked solution, built on the
  **Google Trends API**. Edit two lines (`MY_TERMS`, `MY_GEO`) in Step 0 and rerun for any
  disease and place. Falls back to the cached flu/US snapshot when no key is loaded.

Default example topic: **flu in the US**, shipped as a real cached snapshot
(`data/google_trends_flu_us_cached.csv`, 265 weekly points through 2026-07-19, pulled
through the API) so Lane B runs without a key. This differs from the 9:45 dengue/Mexico
example on purpose.

## Arc

This session locks in the reusable method on the stream students already know (Google
Trends). Day 2 morning (`day2-0900-data-beyond-google-trends`: Wikipedia + wastewater) and
Day 2 late morning (mobility + news) are "more streams through the same loop," feeding the
capstone.

## Verification

Both notebooks are valid JSON and every code cell parses. The Trends API path was tested
live against the class key:

- `getGraph` returns the **same normalized 0-100 index** as pytrends; resolution follows the
  window exactly as the website does (18 months -> 79 weekly points; 10 years -> 126 monthly).
- **Deterministic:** two identical pulls returned byte-identical values (same hash), unlike
  pytrends' resampled endpoint.
- Topic ids (`/m/0cycc`) and region geos (`US-GA`) both work.
- The term-vs-topic language result was measured, not assumed (France: term `flu` max 3 /
  mean 0.9 vs topic max 100 / mean 16.3).
- The cache was regenerated through the API, so cached and live values are consistent.

The notebooks were not executed end to end on the drafting machine (no pandas/matplotlib
there); the API calls and the client in `scripts/gt_api.py` were exercised directly.
