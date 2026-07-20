# Day 1, 9:45 — Agent Coding Introduction

Instructors: **S. Yang + C. Djorno**. First hands-on session, right after Mauricio's
opening overview. This folder holds everything for the session.

This is the 2026 redesign of the old *Coding introduction* (R. Garrido, SISMID25). The
old session set up R/Python and led into "Intro to Google Dengue Trends." The 2026
version keeps the setup, adds the **AI coding agents**, and turns the Google Trends intro
into a hands-on **data scrape**: participants pull the dengue search signal for Mexico
themselves, then model it in the 11:00 Dengue exercise. The "Intro to Google Dengue
Trends" narration now lives at the start of that 11:00 session, so this hour flows
straight into it.

## Learning goals

By the end of the hour every participant has:

1. an identical coding environment (GitHub Codespace, or clone, or Colab),
2. a working AI coding agent (Codex, Claude Code, or Antigravity CLI, all installed by
   `scripts/setup-agents.sh`) they set up themselves,
3. their first scraped data stream: **Google Trends dengue search interest for Mexico**,
   pulled two ways: a live handful of terms over the **past 5 years** (last point is the
   current week) and the historical **2004–2011** window that feeds the 11:00 exercise,
   plotted, sanity-checked, and saved as a tidy CSV.

## Session outline (~45 min of the 9:00–10:30 block)

| Time | What | Material |
|------|------|----------|
| 5 min  | Why identical environment + agent-as-equalizer | slides |
| 10 min | Launch environment (Codespace / clone / Colab), run smoke test | repo `notebooks/00_smoke_test.ipynb` |
| 10 min | Install and authenticate the agents | `scripts/setup-agents.sh`, `docs/agent-setup.md` |
| 5 min  | Intro to data scraping + the fetch→tidy→plot→check→save loop | slides |
| 15 min | **Exercise 0**: first scrape (Lane A agent / Lane B notebook) | this folder |

Pad generously. Novices can take far longer than you expect; triage early to Lane B.

## Contents of this folder

```
slides/
  agent-coding-intro.tex        Beamer deck for the session (metropolis theme)
  agent-coding-intro.pdf        compiled slides
exercise/
  exercise0_data_scraping.tex   the exercise handout (mirrors the old exercise_1.pdf)
  exercise0_data_scraping.pdf   compiled handout
notebooks/
  01_data_scraping_intro.ipynb        Lane A: the prompts you give the agent (you run its output)
  01_data_scraping_intro_soln.ipynb   Lane B: the agent's output captured, as a backup
data/
  google_trends_dengue_mx_cached.csv   historical 2004-2011 snapshot (offline fallback)
refresh_cache.py                       regenerate the caches live before class
README.md
```

## The two lanes

- **Lane A (agent) is the real experience.** `notebooks/01_data_scraping_intro.ipynb` is
  the prompts. You paste each into Codex, Claude Code, or Antigravity CLI, run the code
  it writes, and apply the sanity-check yourself.
- **Lane B is the backup: the agent's output, captured.** Every cell in
  `notebooks/01_data_scraping_intro_soln.ipynb` is an example of what those Lane A prompts
  produce, with a comment naming the prompt that produced it. Run it top to bottom if your
  agent is not set up; it pulls live and **falls back to the cached CSV** so nobody is
  blocked. It is a faithful stand-in for the agent, not a separately written solution.

Both lanes arrive at the same tidy CSV.

## Why Google Trends dengue as the first stream

- **Continuity.** It is the same search signal modeled in the 11:00 exercise, so the
  scrape feeds straight into "Intro to Google Dengue Trends" and least squares.
- **It is the course backbone.** Google search interest is the classic digital-disease
  signal; everything this week orbits it.
- **The awkwardness is the lesson.** Google Trends has no official API, rate-limits
  scrapers, and returns different numbers each pull. Seeing that instability first-hand
  motivates the afternoon's statistical preprocessing.

## Two windows: live-recent and historical

The notebook deliberately pulls **two** windows so the demo feels current *and* the
exercise stays reproducible:

- **Recent, live (`today 5-y`).** A last-5-years pull whose final point lands on the
  current week, so participants see *actual, present-day* dengue searches, not a frozen
  file. This is the "good to see live be the actual present" moment. It is live-only; if
  Google rate-limits it, the notebook says so and moves on.
- **Historical (2004–2011).** The window the 11:00 exercise needs, matched to the
  reported-case ground truth. This one is **cached**, so it always works.

## Cached snapshot and refreshing

`data/google_trends_dengue_mx_cached.csv` is a real snapshot of dengue search interest
for Mexico (monthly, 2004–2011: the same period as the 11:00 exercise), with the four
search-term columns from the original `MX_Dengue_trends.csv`
(`date, dengue, sintomas_de_dengue, mosquito, dengue_sintomas`). A live `pytrends` pull
targets the same window; landing on the cache is expected when Google rate-limits the room.

Run [`refresh_cache.py`](refresh_cache.py) on a working connection a day or two **before
the course** to regenerate the historical cache and snapshot a fresh recent window:

```bash
pip install pytrends pandas
python refresh_cache.py
```

Google Trends blocks datacenter IPs, so this must be run from an ordinary network, not
from CI or a cloud shell.

## Live pulls in Codespaces (tested)

GitHub Codespaces egress from **Azure datacenter IPs**. We expected Google Trends to block
these hard, but a real in-Codespace test told a better story: egress was
`AS8075 Microsoft Corporation`, the data call hit one **429**, waited, retried, and
**succeeded** with current data (last point today). So **retry, not a proxy, is the
primary path.** Re-check any room with `python scripts/gt_smoke_test.py`.

Reliability ladder, in order:

1. **Just retry.** The notebook's `gt_fetch` waits and retries on a 429 automatically; this
   alone got a Codespace through. Keep pulls small (a handful of terms) and staggered so
   the whole room does not burst at once.
2. **Proxy or VPN (backup).** If a room keeps getting blocked despite retries, route
   Codespace pulls through a machine with an ordinary residential IP. Setup for a tinyproxy
   HTTP proxy or a Tailscale VPN exit node is in the 3:30 session's
   [`proxy-setup.md`](../day1-1530-ai-agents-data-scraping/proxy-setup.md).
3. **The cache / Plan B (last resort).** Always works, no network needed, so no one is ever
   stuck.

## Build the PDFs

```bash
cd slides    && pdflatex agent-coding-intro.tex && pdflatex agent-coding-intro.tex
cd ../exercise && pdflatex exercise0_data_scraping.tex
```

## Instructor notes

- **Auth is deliberately open / TBD.** See `docs/agent-setup.md` in the course repo;
  never hard-code a shared key into a notebook.
- **Plan B is not a consolation prize.** Say so out loud. Some participants should be on
  it from minute one and still get everything conceptual.
- **Have the cached CSV ready.** If classroom Wi-Fi is bad, demo the fallback first so
  people see that a failed pull is a non-event.
