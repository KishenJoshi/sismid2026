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
- **Term vs topic** — a raw string is a *term* (literal, more zeros); a *topic* is Google's
  aggregated Knowledge Graph entity (more volume, fewer zeros). The notebook shows both ways
  to reach a topic-like signal: combine a few query phrases with `" + "` (an additive/OR
  query), or resolve the real topic entity via `pytrends` suggestions and pull it by its
  `mid`.
- **Making the scrape robust** — Google Trends is the messy case (no official API, an
  internal endpoint, rate limits, sampling instability); the agent handles pytrends,
  retry-on-429, and the whole loop while you keep the judgment.
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

- **Lane A** (`01_scrape_your_topic.ipynb`): one prompt per step (robust `gt_fetch` +
  `topic_mid` + `MY_TERMS`/`MY_GEO`, live pull, **term vs topic** comparison, instability
  check, sanity-check, save). You paste each into Codex / Claude Code / Antigravity CLI and
  run its output.
- **Lane B** (`01_scrape_your_topic_soln.ipynb`): the captured worked solution. It is
  **parameterized to your own topic**: edit two lines (`MY_TERMS`, `MY_GEO`) in Step 0 and
  rerun. It reuses the 9:45 `gt_fetch` (retry-on-429, small stagger) and **falls back to the
  cached flu/US example** so it runs even when the live pull is blocked.

Default example topic: **flu in the US**, shipped as a real cached snapshot
(`data/google_trends_flu_us_cached.csv`, 262 weekly points through July 2026) so Lane B runs
offline. This differs from the 9:45 dengue/Mexico example on purpose.

## Arc

This session locks in the reusable method on the stream students already know (Google
Trends). Day 2 morning (`day2-0900-data-beyond-google-trends`: Wikipedia + wastewater) and
Day 2 late morning (mobility + news) are "more streams through the same loop," feeding the
capstone.

## Verification

Both notebooks are valid JSON and every code cell parses. The cache is a real Google Trends
pull (flu/US, `today 5-y`) so Lane B's fallback path is genuine. The live pull needs
`pytrends` (in the course `requirements.txt`); it was not executed on the drafting machine
(pytrends/pandas not installed there), but the `gt_fetch` logic is the same proven code from
the 9:45 session, which was validated live from a Codespace.
