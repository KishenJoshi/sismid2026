"""Google Trends API client (v1beta `getGraph`) for SISMID 2026.

A drop-in, reliable replacement for pytrends. Same signal, no scraping:

    pytrends (public endpoint)        this module (Trends API)
    -------------------------        -----------------------------------
    HTTP 429 roulette                 API key, no rate-limit gambling
    values resample every pull        deterministic: same numbers every time
    fragile unofficial endpoint       documented Google API

It returns the SAME normalized 0-100 index pytrends returns (method
`getGraph`, "search volume per time points, normalized"), so nothing about the
modelling downstream changes. Resolution is chosen by Google from the window
length, exactly as on the Trends website: a long window gives monthly points,
roughly 18 months or less gives weekly points.

Authentication
--------------
The API key is read from the environment and is NEVER stored in this repo:

    export GT_API="the key handed out in class"

Quota: 1,000 requests/day for the class key (one request = one call, which may
carry several terms). Please query health-related topics only.

Usage
-----
    from gt_api import gt_api_fetch
    df = gt_api_fetch(['dengue', 'mosquito'], geo='MX',
                      start='2021-01', end='2026-07')

Returns a tidy pandas DataFrame: a `date` column plus one column per term
(column names underscored), which is the same shape the pytrends helper in the
course notebooks returns.
"""
from __future__ import annotations

import os
import time
import urllib.error
import urllib.parse
import urllib.request
import json

ENDPOINT = "https://www.googleapis.com/trends/v1beta/graph"
DEFAULT_TIMEOUT = 45


class TrendsAPIError(RuntimeError):
    """Raised when the Trends API cannot satisfy the request."""


def _norm(name: str) -> str:
    """Column-safe version of a term ('sintomas de dengue' -> 'sintomas_de_dengue')."""
    return name.strip().replace(" ", "_").replace("/", "_").lstrip("_")


def get_api_key(explicit: str | None = None) -> str:
    key = explicit or os.getenv("GT_API")
    if not key:
        raise TrendsAPIError(
            "No API key. Set it first:  export GT_API='...'  "
            "(the key is handed out in class; never commit it)."
        )
    return key


def gt_api_raw(terms, geo="US", start="2021-01", end=None, category=None,
               prop=None, api_key=None, tries=3, timeout=DEFAULT_TIMEOUT):
    """Call getGraph and return the decoded JSON.

    terms : list[str]  search terms or topic mids (e.g. '/m/0cycc')
    geo   : ISO-3166 code, country ('MX') or region ('US-GA')
    start / end : 'YYYY-MM' (end defaults to the current month)
    """
    if isinstance(terms, str):
        terms = [terms]
    if end is None:
        end = time.strftime("%Y-%m")

    params = [("key", get_api_key(api_key))]
    params += [("terms", t) for t in terms]
    params += [("restrictions.geo", geo),
               ("restrictions.startDate", start),
               ("restrictions.endDate", end)]
    if category is not None:
        params.append(("restrictions.category", category))
    if prop is not None:
        params.append(("restrictions.property", prop))

    url = ENDPOINT + "?" + urllib.parse.urlencode(params)
    last = None
    for attempt in range(tries):
        try:
            with urllib.request.urlopen(url, timeout=timeout) as r:
                return json.loads(r.read())
        except urllib.error.HTTPError as e:
            body = e.read().decode("utf-8", "replace")[:300]
            last = f"HTTP {e.code}: {body}"
            # 403 usually means quota exhausted or key not enabled: do not hammer
            if e.code in (400, 401, 403):
                raise TrendsAPIError(last) from e
            if attempt < tries - 1:
                time.sleep(5 * (attempt + 1))
                continue
        except Exception as e:  # noqa: BLE001
            last = f"{type(e).__name__}: {e}"
            if attempt < tries - 1:
                time.sleep(5 * (attempt + 1))
                continue
    raise TrendsAPIError(last or "unknown error")


def gt_api_fetch(terms, geo="US", start="2021-01", end=None, **kw):
    """Tidy DataFrame: `date` + one column per term (0-100 normalized index)."""
    import pandas as pd  # imported lazily so the module is usable without pandas

    data = gt_api_raw(terms, geo=geo, start=start, end=end, **kw)
    lines = data.get("lines") or []
    if not lines:
        raise TrendsAPIError(f"empty response for {terms} in {geo}")

    frames = []
    for line in lines:
        pts = line.get("points") or []
        s = pd.DataFrame({
            "date": pd.to_datetime([p["date"] for p in pts]),
            _norm(line.get("term", "term")): [p["value"] for p in pts],
        })
        frames.append(s.set_index("date"))
    out = pd.concat(frames, axis=1).reset_index().sort_values("date")
    return out.reset_index(drop=True)


def resolution_of(df) -> str:
    """'weekly' or 'monthly', inferred from the spacing Google returned."""
    if len(df) < 3:
        return "unknown"
    gap = df["date"].diff().dt.days.median()
    return "weekly" if gap <= 10 else "monthly"


if __name__ == "__main__":  # smoke test:  GT_API=... python gt_api.py
    d = gt_api_raw(["dengue"], geo="MX", start="2025-01", end="2026-06")
    pts = d["lines"][0]["points"]
    print(f"ok: {len(pts)} points, {pts[0]['date']} -> {pts[-1]['date']}")
