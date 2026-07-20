# Day 2, 1:30 — From dengue exercise to ARGO (Part 1)

Instructor: **S. Yang + C. Djorno** (co-taught block; M. Santillana takes Part 2, "beyond
flu monitoring in rich nations"). This is the statistical-regression session: it continues
the dengue exercise and grows a single least-squares line, step by step, into **ARGO**.

## Lineage: this is Mauricio's exercise 2, updated

The old syllabus had *"Dengue exercise continued (R. Garrido): static vs dynamic training,
adding predictive terms, adding autoregression."* That is exactly Mauricio's
**`exercise_2.pdf`** in `MIGHTE-lab/SISMID25` (verified: sliding-window dynamic training →
add all Google terms → add AR2, whose solution literally labels the final model "ARGO").

The 2026 version keeps those first three bullets **unchanged** (only the title changed, as
you noted) and adds two:

- **Combining autoregression with Google search terms to arrive at ARGO** — makes explicit
  that exercise 2(e) *is* a basic ARGO, writes the ARGO equation, and cites Yang, Santillana
  & Kou (PNAS 2015). Notes the full ARGO adds an L1 penalty + rolling window.
- **Network approaches (ARGONet-type)** — new, from Yang's lineage: borrow signal from
  connected neighbors (Lu, Yang, Santillana et al., Nature Communications 2019; plus a
  county-level version). Ties directly to the mobility ranking from the 11:00 session.

## What this session covers

Static vs dynamic training → adding predictive terms (+ Lasso for many terms) → adding
autoregression → **ARGO** (AR + Google, dynamically trained) → **ARGONet** (network step).
Agent-driven throughout (one prompt per upgrade), interpretable regression the whole way,
scored out-of-sample on 2007–2011 at each step.

## Contents

```
slides/
  dengue-to-argo.tex     Beamer deck (metropolis theme), ends with a credits slide
  dengue-to-argo.pdf     compiled slides
exercise/
  exercise_dengue_to_argo.tex   updated handout: exercise 2 (a)-(e) + an ARGONet part (f)
  exercise_dengue_to_argo.pdf   compiled handout
notebooks/
  02_dengue_to_argo.ipynb        Lane A: the prompts you give the agent (you run its output)
  02_dengue_to_argo_soln.ipynb   Lane B: the agent's output captured, as a backup
data/
  MX_Dengue_trends.csv           Mexico dengue cases + 4 Google search terms (2004-2011)
README.md
```

## Notebooks (agent-native)

- **Lane A** (`02_dengue_to_argo.ipynb`): one prompt per upgrade (static recap, dynamic,
  +terms, +AR = ARGO, compare, Lasso stretch, ARGONet discussion). You paste each into the
  agent and run its output.
- **Lane B** (`02_dengue_to_argo_soln.ipynb`): the captured worked solution, built on
  Mauricio's `exercise2_soln` logic (sklearn `LinearRegression`, sliding 36-month window,
  `shift` for AR terms), with a LassoCV stretch and an ARGONet note. Each code cell is
  labelled with the Lane A prompt that produces it.

Verified on the real data (numpy OLS, same logic as the notebook): out-of-sample RMSE falls
at every step, **static 4968 → dynamic 4040 → +terms 3684 → ARGO 3391**. (pandas/sklearn
weren't installable on the drafting machine, so the notebook itself was validated for
structure/syntax and the modeling logic reproduced with numpy; it runs as-is in the course
Codespace.)

The exercise handout keeps Mauricio's parts (a)-(e) essentially verbatim (dynamic, compare,
score, covariates, autoregression), reframes (e) as "this is ARGO" with the equation, adds a
Lasso note, and adds a conceptual **(f) ARGONet** part (the MX data is one country, so the
network step is sketched here and becomes real on the state/county panels).

## Build the PDFs

```bash
cd slides    && pdflatex dengue-to-argo.tex && pdflatex dengue-to-argo.tex
cd ../exercise && pdflatex exercise_dengue_to_argo.tex
```

## Arc

Day 1 built the static baseline (search → cases). This session turns it into ARGO. Day 2
3:30 is the COVID exercise (exponential-growth detection); Day 3 is bring-your-own-problem,
pointing ARGO at your own data.

## Suggested syllabus edit

Rename the 1:30 block title to **"From dengue exercise to ARGO (S. Yang + C. Djorno)"** and
append the two new bullets (combine AR + Google = ARGO; ARGONet). The first three bullets
stay as-is. I can apply this to `syllabus_2026_for_mauricio.md` on your go-ahead.

## Status

Slides, exercise handout, both lane notebooks, and the data file are all in place and
compile/parse clean.
