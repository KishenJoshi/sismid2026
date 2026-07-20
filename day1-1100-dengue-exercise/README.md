# Day 1, 11:00 — Dengue exercise (Part 1)

Instructors: **S. Yang + C. Djorno** (co-taught block with M. Santillana). Follows the
9:45 agent-coding introduction: this morning students **scraped** the dengue search
signal for Mexico; here they **map that search onto reported dengue cases** and validate
it, all by driving a coding agent.

## What this session covers

- Intro to Google Dengue Trends (the digital-epidemiology origin story).
- The exercise (local 2026 version): `exercise/dengue-exercise-2026.pdf`, adapted from
  SISMID25 `exercises/exercise_1.pdf`.
- Visualize the data; least squares (fit search onto cases); map dengue search onto dengue
  activity in Mexico; validate out of sample.
- Stretch: a dynamic model + Lasso on the Zika-tutorial data
  (https://github.com/sarahhbellum/Zika-tutorial). Autoregression is deliberately held for
  Day 2 (ARGO).

Emphasis: **how to use an AI agent to do all of the above** (one prompt per step), with
the "verify what it produced" habit, and a Plan B pre-filled notebook so nobody is stuck.

## Teaching flow (the slides drive it)

The deck is built to teach from. It signposts every hand-off:

1. Slides set up the idea and show **the prompt** for each step.
2. At "The task in one line," a **▶ to the notebook** cue: hand out
   `exercise/dengue-exercise-2026.pdf` and open the notebook.
3. Students run steps 1–3 in the notebook (Lane A prompts or Lane B pre-filled).
4. A **◀ back to slides** cue brings the room back to verify the work and discuss.

See the "How this hour runs" slide (slide 4) for the map.

## Two lanes

- **Lane A (agent):** `notebooks/dengue_exercise.ipynb` — the prompts (they match the
  slides). Paste each into Codex / Claude Code / Antigravity CLI and run its output.
- **Lane B (pre-filled):** `notebooks/dengue_exercise_soln.ipynb` — the worked solution,
  a captured example of what the agent produces. **Executed clean** on the pinned
  environment (0 errors): correlation 0.81, out-of-sample RMSE ≈ 4968 cases. Needs only
  `pandas`, `numpy`, `matplotlib`, `scikit-learn`.

## Data

`data/MX_Dengue_trends.csv` (Mexico, monthly, 2004–2011), from the SISMID25 repo:
- `Dengue CDC` — reported dengue cases (ground truth).
- `dengue`, `sintomas de dengue`, `mosquito`, `dengue sintomas` — Google search interest.

Training window: 2004–2006 (first 36 months). Validation window: 2007–2011. This is the
same signal produced in the 9:45 session, now paired with the case ground truth.

## Contents

```
slides/
  dengue-exercise.tex   Beamer deck: agent-driven walkthrough, with flow cues (metropolis)
  dengue-exercise.pdf   compiled slides
exercise/
  dengue-exercise-2026.tex   the exercise handout (2026 version of SISMID25 exercise_1)
  dengue-exercise-2026.pdf   compiled handout
notebooks/
  dengue_exercise.ipynb        Lane A: the prompts you give the agent
  dengue_exercise_soln.ipynb   Lane B: the worked solution (executed, 0 errors)
data/
  MX_Dengue_trends.csv         Mexico dengue cases + search interest, 2004–2011
README.md
```

## Build the PDFs

```bash
cd slides    && pdflatex dengue-exercise.tex && pdflatex dengue-exercise.tex
cd ../exercise && pdflatex dengue-exercise-2026.tex
```

## Arc

Least squares here is the honest baseline. The stretch (dynamic training + Lasso) and the
afternoon's Google Trends preprocessing lead into **ARGO** on Day 2, which is where
**autoregression** (the disease's own history) enters, alongside selected search terms and
dynamic training.
