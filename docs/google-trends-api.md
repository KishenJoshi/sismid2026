# Google Trends API (the reliable path)

`pytrends` scrapes an unofficial endpoint. It is fine for teaching the gotchas, but for
anything you actually rely on, use the **Google Trends API**. It returns the **same
normalized 0-100 index**, so nothing downstream changes: it is a drop-in replacement.

| | pytrends (scraping) | Trends API (`getGraph`) |
|---|---|---|
| Access | unofficial endpoint | documented API, needs a key |
| Failures | HTTP 429 roulette | none in normal use |
| Values | **resampled on every pull** | **identical on every pull** |
| Signal | normalized 0-100 | **the same** normalized 0-100 |
| Resolution | window-dependent | window-dependent (identical rule) |

## The key

The key ships **encrypted** in this repo (`secrets/gt-api/GT_API.enc`) and is unlocked with
the **Google Trends passcode** the instructor gives out in class. In the terminal:

```bash
source scripts/unlock-gt-api-key.sh     # asks for the passcode
```

That decrypts the blob and exports **`GT_API`** (masked confirmation is printed), and
appends it to `~/.bashrc` so new terminals and a Codespace restart stay unlocked.

> **Start Jupyter *after* unlocking**, or restart the kernel: a kernel launched before you
> ran the unlock will not have `GT_API` in its environment.

This is deliberately **separate** from `scripts/claude-login.sh`, which unlocks the agent
tokens in `secrets/*.enc`. Different secret, different script; the Trends blob lives in its
own subfolder so `claude-login.sh` never tries to decrypt it.

**Instructor:** to (re)create the blob, run on your own machine:

```bash
scripts/gt-api-encrypt.sh               # prompts for the key, then the passcode
```

AES-256-CBC, PBKDF2-SHA256, 600k iterations, salted, base64 armored. Only `.enc` files may
be committed; `.gitignore` blocks any plaintext that lands in `secrets/`.

**Rules of use (from the key owner):**

- **1,000 requests/day.** Google enforces a hard limit; if we blow through it, everyone is
  blocked for the day.
- **Health-related topics only.** No finance, crypto, or unrelated queries.
- **Never commit the plaintext key**, and do not paste it into a notebook.

## The call

```
GET https://www.googleapis.com/trends/v1beta/graph
    ?key=$GT_API
    &terms=dengue&terms=/m/0cycc        # repeat for each term; terms OR topic ids
    &restrictions.geo=MX                # ISO-3166: US, MX, US-GA, ...
    &restrictions.startDate=2021-07     # YYYY-MM (month granularity)
    &restrictions.endDate=2026-07
```

Response is `{"lines": [{"term": ..., "points": [{"date": "YYYY-MM-DD", "value": 0-100}]}]}`.

**Resolution is chosen by window length, exactly as on the Trends website:** about 18
months or less returns weekly points; a longer window returns monthly points. Verified:
`2025-01 → 2026-06` gives 79 weekly points; `2016-01 → 2026-06` gives 126 monthly points.

## Helper

`scripts/gt_api.py` wraps this:

```python
import sys; sys.path.insert(0, 'scripts')
from gt_api import gt_api_fetch
df = gt_api_fetch(['dengue', 'mosquito'], geo='MX', start='2021-01', end='2026-07')
```

Returns a tidy DataFrame: `date` plus one column per term, the same shape the pytrends
helper returns in the notebooks.

## Terms vs topics

The API accepts a literal **term** or a Knowledge Graph **topic id** (`/m/0cycc` =
Influenza). A topic folds together every spelling, synonym and **language** for a concept.
This matters enormously outside English-speaking countries (measured 2022-01 → 2026-07):

| Country | term `flu` max / mean | topic `/m/0cycc` max / mean |
|---|---|---|
| France | 3 / 0.9 | **100 / 16.3** |
| Italy | 4 / 1.6 | **100 / 17.7** |
| Germany | 5 / 1.4 | **100 / 16.7** |
| Mexico | 4 / 1.7 | **100 / 37.1** |

An English term is nearly silent abroad; the topic sees *grippe*, *influenza* and *gripe*
alike. Use topics for anything cross-country.

A reference list of 21 flu-related topic ids is in
`day1-1530-ai-agents-data-scraping/data/flu_topic_ids.csv`.

## Other methods on the same key

The discovery document (`https://www.googleapis.com/discovery/v1/apis/trends/v1beta/rest`)
also exposes `getGraphAverages`, `getTopTopics`, `getRisingTopics`, `getTopQueries`,
`getRisingQueries`, `regions.list`, and `getTimelinesForHealth`.

`getTimelinesForHealth` is the **Health Trends** method: it returns *raw, unscaled* weekly
counts back to 2004 rather than the 0-100 index. Useful for research, but it is a
**different signal**, so the course uses `getGraph` to stay consistent with pytrends.

## Credit

The API access, the extraction pipeline and the topic list come from Candice Djorno's
Google Trends pipeline (shared with the ISI Foundation collaboration, May 2026). Apply for
your own access at <https://support.google.com/trends/contact/trends_api>.
