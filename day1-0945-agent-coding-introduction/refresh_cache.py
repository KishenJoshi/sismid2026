#!/usr/bin/env python3
"""Regenerate the cached Google Dengue Trends snapshots for the Day 1 scraping session.

Run this on a machine with a working internet connection a day or two BEFORE the
course, so the "live" demo and the fallback both reflect current data:

    pip install pytrends pandas
    python refresh_cache.py

It writes two files into ./data/ :

  google_trends_dengue_mx_cached.csv          historical 2004-2011 (feeds the 11:00 exercise)
  google_trends_dengue_mx_recent_cached.csv   a recent window up to today (optional live fallback)

Google Trends rate-limits aggressively. If a pull comes back empty, wait a few
minutes and rerun; the historical file that ships with the repo is already valid,
so a failed refresh never breaks the class.
"""
import os
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.join(HERE, "data")
GEO = "MX"
TERMS = ["dengue", "sintomas de dengue", "mosquito", "dengue sintomas"]


def norm(c):
    return c.strip().replace(" ", "_")


def fetch(pt, kw_list, timeframe):
    pt.build_payload(kw_list, timeframe=timeframe, geo=GEO)
    df = pt.interest_over_time()
    if df.empty:
        return None
    df = df.drop(columns=[c for c in ["isPartial"] if c in df.columns]).reset_index()
    return df.rename(columns={c: norm(c) for c in df.columns})


def main():
    try:
        from pytrends.request import TrendReq
    except ImportError:
        sys.exit("pytrends is not installed. Run: pip install pytrends pandas")

    os.makedirs(DATA, exist_ok=True)
    pt = TrendReq(hl="en-US", tz=360)

    print("Pulling historical window 2004-2011 ...")
    hist = fetch(pt, TERMS, "2004-01-01 2011-12-31")
    if hist is not None:
        out = os.path.join(DATA, "google_trends_dengue_mx_cached.csv")
        hist.to_csv(out, index=False)
        print(f"  wrote {out} ({len(hist)} rows)")
    else:
        print("  historical pull was rate-limited; keeping the existing cached file.")

    time.sleep(2)

    print("Pulling recent window (today 5-y) ...")
    recent = fetch(pt, ["dengue"], "today 5-y")
    if recent is not None:
        out = os.path.join(DATA, "google_trends_dengue_mx_recent_cached.csv")
        recent.to_csv(out, index=False)
        print(f"  wrote {out} ({len(recent)} rows, last point {recent['date'].max().date()})")
    else:
        print("  recent pull was rate-limited; try again in a few minutes.")


if __name__ == "__main__":
    main()
