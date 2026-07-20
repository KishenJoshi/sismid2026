# Day 2, 11:00 — More novel data streams

Instructor: **S. Yang + C. Djorno**. Mobility (ground + air), weather, and news alerts.

This is the session that steps beyond our own primary data (search) into streams other
groups pioneered. It is framed honestly: **credit the field's established sources, then show
a free, agent-scrapable way into each**, with cached snapshots of the licensed ones.

## Distinct identity vs the 9:00 session (avoiding overlap)

- **9:00 (Wikipedia, wastewater):** "how much disease is *here* now?" — free, well-behaved
  activity proxies.
- **11:00 (mobility, weather, news):** *where it is coming from* (mobility), *what is
  driving it* (weather), *what is emerging* (news) — context signals, often licensed,
  borrowed from established work.

## What this session covers, and where it comes from

- **Human mobility.** Gold standard = **GLEAM** (Balcan et al., PNAS 2009; MOBS lab,
  Northeastern; Vespignani), which uses **OAG + IATA** air travel, **national commuting**
  statistics, and WorldPop. Those are **licensed**, so the hands-on angle uses free proxies:
  - Ground: **US Census LODES** commuting flows (rank neighbor counties), Google COVID-19
    Community Mobility (archived 2020–2022), ODT Flow Explorer; **SafeGraph** licensed →
    cached.
  - Air: **OpenSky Network** flight API (proxy for OAG/IATA), cross-checked with Wikipedia
    country exchange for importation risk.
- **Weather.** A modulator, not a case count. **Absolute humidity** drives influenza
  (Shaman & Kohn PNAS 2009; Shaman et al. PLoS Biology 2010, PNAS 2012); temperature +
  rainfall drive vector diseases. Free/scrapable via **Open-Meteo** (no key) and **ERA5**
  (Copernicus). Hands off the *modeling* of weather-as-modulator to Santillana's Day 2
  afternoon; here we just get the data.
- **News alerts.** **ProMED** (ISID, human-curated, flagged COVID ~1 day before official)
  and **HealthMap** (Brownstein/Freifeld; feeds WHO EIOS) are the established event-based
  systems; **GDELT** (free, global, API) is the agent-scrapable option. Connects to
  Kogan/Santillana multi-source COVID forecasting (searches + news + GLEAM), Science
  Advances 2021.

## The pattern

Gold-standard sources are often licensed; the agent scrapes a **free proxy** and we keep a
**cached snapshot** of the licensed one. Same five-step loop, same verify habit. These are
**context** signals (noisier than a local case proxy), so treat them as complements and
validate.

## Contents

```
slides/
  more-novel-data-streams.tex   Beamer deck (metropolis theme), ends with a credits slide
  more-novel-data-streams.pdf   compiled slides
README.md
```

## Build the PDF

```bash
cd slides && pdflatex more-novel-data-streams.tex && pdflatex more-novel-data-streams.tex
```

## Sources verified while drafting (July 2026)

- GLEAM data page (gleamproject.org/data): OAG + IATA air travel, national commuting
  statistics, WorldPop/GPW; Aedes occurrence + environmental layers for vector diseases.
- GLEAM model paper: Balcan et al., *Multiscale mobility networks...*, PNAS 2009.
- Free mobility: US Census LODES; OpenSky Network API; Google COVID-19 Community Mobility
  (no longer updated after 2022-10-15, historical remains); ODT Flow Explorer.
- Weather/humidity: Shaman & Kohn 2009; Shaman et al. 2010/2012; ERA5 (Copernicus);
  Open-Meteo (free, no key).
- News: ProMED (promedmail.org); HealthMap; WHO EIOS; GDELT (gdeltproject.org, free API).

## Open questions for you

- **SafeGraph / OAG / IATA are licensed.** Confirm we pre-cache small snapshots for the demo
  rather than attempting live pulls (matches the syllabus internal note).
- **How deep to go on GLEAM itself?** Currently one "gold standard" slide crediting it, then
  we pivot to free proxies. Can expand if you want to lecture the metapopulation idea more.
- **Weather division of labor** with Santillana's afternoon "weather as a modulator" — this
  deck is deliberately data-only to avoid stepping on that.

## Not built yet (say the word)

Lane A/Lane B notebooks for LODES commuting, OpenSky arrivals, Open-Meteo absolute humidity,
and a GDELT outbreak scan, plus cached snapshots, can be added to match the other sessions.
