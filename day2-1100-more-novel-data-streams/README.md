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
- **News alerts.** Four tools, in order of how easily we can use them:
  - **GDELT** (free, global, **no key**) is the agent-scrapable one we use in class.
  - **HealthMap** (Brownstein/Freifeld; feeds WHO EIOS) auto-reads news onto a world map.
  - **ProMED** (ISID, human-curated, flagged COVID ~1 day before official) is the expert
    counterpart.
  - **Media Cloud** (200M+ stories in curated collections; the scholars' media-analysis
    tool) needs a **free API key**, so it is a demo + take-home rather than an in-class pull.
  Connects to Kogan/Santillana multi-source COVID forecasting (searches + news + GLEAM),
  Science Advances 2021.

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
notebooks/
  01_mobility.ipynb        Lane A: prompts (ground LODES + air OpenFlights)
  01_mobility_soln.ipynb   Lane B: worked solution
  02_weather.ipynb         Lane A: prompts (Open-Meteo absolute humidity)
  02_weather_soln.ipynb    Lane B: worked solution
  03_news.ipynb            Lane A: prompts (GDELT outbreak watch)
  03_news_soln.ipynb       Lane B: worked solution
  04_bluesky.ipynb         Lane A: prompts (Bluesky health-account monitoring)
  04_bluesky_soln.ipynb    Lane B: worked solution
  05_delphi.ipynb          Lane A: prompts (CMU Delphi / Facebook symptom survey)
  05_delphi_soln.ipynb     Lane B: worked solution
  06_meta_data_for_good.ipynb       Lane A: prompts (Meta AI for Good via HDX)
  06_meta_data_for_good_soln.ipynb  Lane B: worked solution
data/
  lodes_fulton_inflow_top_counties.csv   real: top 15 counties feeding Fulton Co., GA
  openflights_atl_inbound.csv            real: 216 origin airports / 43 countries into ATL
  openmeteo_atlanta_weather.csv          real: 3,844 days Atlanta temp/dewpoint + abs. humidity
  gdelt_dengue_articles.csv              real: 231 dengue-outbreak articles (past month)
  bluesky_health_accounts.csv            real: 60 public-health accounts discovered
  bluesky_health_posts.csv               real: 99 disease-keyword posts from 1,131 scanned
  delphi_fb_survey_ga.csv                real: 663 days of GA %CLI (Facebook survey)
  meta_rwi_mexico.csv                    real: 77,083 RWI grid cells, Mexico (~2.4 km)
  meta_movement_mx_national_daily.csv    real: Mexico distance-from-home, 16 days (Jun 2026)
  meta_movement_mx_municipal_latest.csv  real: 1,700 municipalities, latest date
README.md
```

## Participant request: Meta's AI for Good datasets

Asked in advance: *"I am interested in working with one of Meta's AI for Good datasets."*
Checked against the **Humanitarian Data Exchange (HDX)** API, which is how Meta publishes:

| Requested | In class? | How |
|---|---|---|
| **Relative Wealth Index** | **yes** | free per-country CSVs on HDX |
| **Movement Distribution Maps** | **yes** | free on HDX, **CC BY**, current through June 2026 |
| Activity Space Maps | not directly | not published on HDX; Meta's own portal |
| Points of Interest | not directly | not on HDX under Meta (HDX POI results are OpenStreetMap/HOT) |

**224 datasets** are published under "AI for Good at Meta" on HDX, so there is far more than
the four: Social Connectedness Index, Commuting Zones, High Resolution Population Density
Maps, International Migration Flows, and the COVID-19 Trends and Impact Survey.

Notebook `06_meta_data_for_good` covers it: query the HDX catalogue programmatically, pull
**Mexico RWI** (77,083 grid cells, mapped) and **Mexico Movement Distribution** (16 days of
distance-from-home bands; mean "stayed home" share 0.384 on 2026-06-16, plus the most
home-bound vs most mobile of 1,700 municipalities).

**Honest limits, stated in the notebook:** RWI is a **model estimate** with an `error`
column, not measured income; Movement Distribution covers **Facebook users with location
history on**, a biased sample. Both are *context* (a confounder, a behaviour channel), not
case counts. Mexico was chosen deliberately so RWI can be joined to the Day 1 dengue
exercise as a candidate confounder.

## Notebooks (built, with real cached snapshots)

Three Lane A / Lane B pairs, one per stream, all free and no-key:

- **01 mobility.** Ground: **US Census LODES** commuting flows, aggregated to the counties
  sending the most commuters into Fulton County, GA. Real result: Fulton 27.4%, **DeKalb
  13.9%, Cobb 12.6%, Gwinnett 10.1%**, Forsyth 4.4% (855,185 jobs from 159 home counties).
  The live path downloads the 22 MB LODES file and is `LIVE = False` by default; the cached
  aggregate is tiny. Air: **OpenFlights** routes into ATL by origin country.
- **02 weather.** **Open-Meteo** archive (no key), daily temperature + dew point for Atlanta
  2016–2026, converted to **absolute humidity**
  (`e = 6.112·exp(17.67·Td/(Td+243.5))`, `AH = 216.7·e/(T+273.15)`), then weekly series and
  a by-month plot showing the winter minimum (the Shaman result, from free data).
- **03 news.** **GDELT DOC 2.0** (no key) outbreak watch: articles by source country and
  volume per day, plus a syndication check (duplicate headlines), because attention is not
  incidence.

Each Lane B pulls live and **falls back to the cached snapshot**, so it runs offline.

### Every notebook is self-contained

Both lanes of all six streams now open with an **"About this data source"** section: what the
source is in plain language, who makes it, why it exists, and a **browser link to explore it
live** (the same links used on the "Meet the source" slides, all verified). Lane B also
carries **"Prompt used (Lane A)"** above each step, so a student reading the worked solution
sees the prompt that produced the code, not just the code. Prompts are copied
programmatically from Lane A, so the two lanes cannot drift apart.

## Social media: what is actually reachable (tested July 2026)

| Source | Status | Evidence |
|---|---|---|
| **X / Twitter** posts | Closed | `HTTP 401`; free API withdrawn 2023, search needs a paid tier |
| **Facebook** posts | Closed | CrowdTangle shut down Aug 2024 → application-only research library |
| **Bluesky** full-text `searchPosts` | Needs auth | `HTTP 403` anonymous |
| **Bluesky** `searchActors` / `getAuthorFeed` / `resolveHandle` | **Open, no auth** | `HTTP 200` |
| **Facebook symptom survey** via CMU Delphi | **Open, no key** | real GA data: `smoothed_wcli` |

So the social stream is taught two ways, both free and login-free:

- **04 Bluesky.** Full-text search needs a login, but *account discovery and public feeds do
  not*, so we monitor **accounts** instead of searching keywords. Real run: 60 public-health
  accounts discovered, **1,131 posts scanned, 99 disease-keyword matches** (outbreak 57,
  covid 23, flu 17, rsv 13, measles 9, mpox 7), including genuine current chatter about an
  **Ebola outbreak in the DRC** and **cyclosporiasis in the US**.
  - It also carries the best sanity-check lesson in the course: our first pass used a naive
    substring test and flagged **"influence"** as flu. The notebook shows the bad and good
    matching side by side. The pipeline ran fine and returned confident nonsense.
- **05 CMU Delphi.** The honest answer to "can we use Facebook data?": not posts, but the
  **COVID-19 Trends and Impact Survey (CTIS)** that Facebook ran with CMU Delphi, free via
  the Epidata API. Delphi's own site labels it **"CTIS (The Facebook Covid Survey)"**, with a
  dedicated dashboard at <https://delphi.cmu.edu/covidcast/survey-results/>, background at
  <https://delphi.cmu.edu/epidemic-signals/ctis/>, and the signal dictionary defining
  `smoothed_wcli` at
  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/fb-survey.html>. Real cache: **663 days of Georgia %CLI, Sep 2020 → Jun 2022, ~5,011
  respondents/day**, and it ships **stderr and sample size**, which almost no other novel
  stream does. It **ended in June 2022**, which makes the fragility point for us.

**The teaching point.** `covid_traces_WA.csv` (used in the 3:30 COVID exercise) still has
**Twitter, Kinsa and Cuebiq** columns, and all three of those streams are now closed or
gone. Novel data streams are fragile; that is the argument for an agent that can re-scrape
whatever exists *today* rather than depending on one frozen pipeline.

## Correction: OpenSky is no longer anonymous

The first draft of the deck said to use the **OpenSky Network** API for air mobility.
Checked in July 2026: anonymous requests now return **HTTP 403**; OpenSky requires a free
account and OAuth2 client credentials. The deck and notebooks now use **OpenFlights**
(free, no key, static route table) as the in-class path, and mention OpenSky as a take-home
option. Note OpenFlights counts **routes, not passengers**, so it is connectivity structure
rather than traffic volume.

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

## Status

Slides, all six notebooks (3 Lane A / Lane B pairs), and four real cached snapshots are in
place. Notebooks are valid JSON and every code cell parses; each cache was generated from a
genuine live pull in July 2026 (see the numbers above). The notebooks need only `pandas` and
`matplotlib` from the course `requirements.txt` (no `pytrends`, no API keys).

## Media Cloud

**Media Cloud** (200M+ stories, curated collections) is added to the news notebook as an
optional bonus: the API needs a **free API key** (sign up at <https://search.mediacloud.org/>),
so the cell runs only if `MEDIACLOUD_API_KEY` is set and otherwise skips cleanly. GDELT
remains the no-key in-class path.
