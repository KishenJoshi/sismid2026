# Day 2, 3:30 — COVID-19 exercise (Part 1)

Instructor: **S. Yang + C. Djorno** (co-taught block; M. Santillana takes Part 2, "COVID-19
research"). Yang's Part 1 covers **accessing digital traces** and **detecting exponential
growth**, and previews the Day 3 capstone.

## Lineage: Mauricio's exercise 3, updated

The COVID exercise is Mauricio's **`exercise_3.pdf`** in `MIGHTE-lab/SISMID25` (verified:
6-panel trace plot → sliding-window growth factor alpha → sustained-growth outbreak
detection → multi-trace early warning). The 2026 version keeps that method and reframes it
as agent-driven, with a Lane A/Lane B pair.

## The question, and the method

- **Question:** not "how high?" (that was ARGO) but **"is it taking off?"** — the onset of
  exponential growth.
- **Method (from exercise 3):** over a sliding 11-day window, regress the last 10 days on
  the previous 10 through the origin; the slope **alpha** is the multiplicative growth factor
  (alpha > 1 = growing). A small state machine turns sustained alpha>1 into an outbreak
  start. Bridge to log-linear: alpha ≈ e^r, doubling time = ln2 / ln alpha.
- **Payoff:** run the detector on each digital trace and see which ones flag growth *before*
  cases — the seed of a multi-trace early-warning system (Kogan/Santillana, Science Advances
  2021).

## Data

`covid_traces_WA.csv` (Washington State, daily, 2020–2021, pre-smoothed): `new_cases`
(ground truth) plus five digital traces — `upToDate` (clinician search), `cdc_ili`,
`Twitter_RelatedTweets`, `google_fever`, `Kinsa_AnomalousFeverAbsolute`, and
`Cuebiq_Mobility`.

## Contents

```
slides/
  covid-exercise.tex     Beamer deck (metropolis theme), ends with a credits slide
  covid-exercise.pdf     compiled slides
exercise/
  exercise_covid.tex     updated handout (mirrors exercise 3, agent-friendly)
  exercise_covid.pdf     compiled handout
notebooks/
  03_covid_exercise.ipynb        Lane A: the prompts you give the agent
  03_covid_exercise_soln.ipynb   Lane B: the agent's output captured, as a backup
data/
  covid_traces_WA.csv    case counts + five digital traces (daily, WA)
README.md
```

## Verification

Notebooks are valid JSON and every code cell parses. The Lane B logic (growth_alpha +
detect_outbreaks) was run on the real data and detects starts that line up with the actual
Washington waves: **2020-03-10, 2020-06-06, 2020-09-27, 2021-03-28 (Alpha), 2021-07-21
(Delta), 2021-12-29 (Omicron)**. The slide quotes these same dates. (pandas/matplotlib were
not installable on the drafting machine, so the detector logic was reproduced with numpy;
the notebook runs as-is in the course Codespace.)

## Build the PDFs

```bash
cd slides    && pdflatex covid-exercise.tex && pdflatex covid-exercise.tex
cd ../exercise && pdflatex exercise_covid.tex
```

## Note on the Day 3 preview

The syllabus lists "Looking ahead to Day 3: bring your own problem" at the end of this
3:30 block, and you confirmed earlier it is yours (not Mauricio's). The deck's penultimate
slide handles it. Mauricio's COVID-19 research talk (Part 2) follows this exercise.
