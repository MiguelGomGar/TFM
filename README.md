# Predicting atrial fibrillation recurrence in the PREDIMAR trial

Master's thesis project: machine learning models that predict recurrence of
atrial fibrillation (AF) after ablation, using the clinical and proteomic data
of the PREDIMAR clinical trial.

The work asks three questions:

1. Do ML models beat the published clinical risk scores (BASE-AF2, CHADS2,
   HATCH, MB-LATER) at predicting recurrence?
2. Does proteomic information add predictive value on top of clinical data?
3. Which model families perform best for each kind of data available?

## Requirements

The project runs on conda. Create the environment from the pinned specification:

```bash
conda env create -f env.yml
conda activate tfm
```

The raw data is not versioned (see `.gitignore`). Before the first run, place it
under `data/raw/`:

| File | Contents |
| --- | --- |
| `predimar_miguelgomez.dta` | PREDIMAR clinical database (Stata export) |
| `olink_baseline_wide.csv` | Olink baseline proteomic panel, wide format |
| `clinical_variables_review.xlsx` | Literature review of candidate risk factors |

## Running the analysis

Both entry points live at the repository root and must be run from there.

```bash
# Every table, model and dataset. Takes about 3 hours: the modelling phases
# run a randomized hyperparameter search over eight models and five phases.
python run_calculations.py

# Only the fast steps (pipelines 01 to 08). About 90 seconds.
python run_calculations.py --skip-modelling
```

```bash
# Every figure, redrawn from the tables already on disk. Nothing is refitted.
python run_plots.py

# Skip the modelling figures, whose input tables would not exist if the
# modelling was skipped above.
python run_plots.py --skip-modelling-plots
```

Execution stops at the first failure: each step consumes what the previous one
wrote, so continuing would only derive results from stale inputs. Every run is
logged to `logs/orchestrator/`, and a summary table with per-step timings is
printed at the end.

The pipelines are deterministic: a single `SEED` governs the splitting,
cross-validation, imputation and every estimator, so a rerun reproduces the
previous results byte for byte.

## Running the tests

```bash
pytest
```

The suite covers the pure, deterministic functions that produce the reported
numbers (metrics, threshold selection, path and artifact-name resolution).

## Layout

``` bash
run_calculations.py      Orchestrator: computes every result
run_plots.py             Orchestrator: draws every figure
src/
  config.py              Project-wide constants and hyperparameter spaces
  data/                  Cleaning, feature engineering, statistical analysis
  models/                Training, tuning, evaluation, result persistence
  visualization/         Figure builders
  utils/                 Paths, artifact names, I/O, logging, orchestration
  pipelines/
    calculations/        01-15: the analysis steps, in order
    plots/               Their figure-drawing mirrors
tests/                   pytest suite
data/                    raw -> intermediate -> clean (not versioned)
results/                 Tables, figures, models, workbooks (not versioned)
```

`src/pipelines/` holds orchestration only: each module defines a `main()` that
imports functions from `src/` and calls them. All reusable logic lives in the
domain packages above, so it can be tested and reused independently of the
pipeline that happens to run it.

Pipeline modules are prefixed with their position (`01_`, `02_`...), which makes
their names invalid Python identifiers. They are loaded through `importlib`, so
run them via the orchestrators rather than importing them directly.

## Analysis stages

| # | Stage | Output |
| --- | --- | --- |
| 01 | Clinical variables review | `results/data_collection/` |
| 02 | Data collection and renaming | `data/intermediate/` |
| 03 | Missing value diagnostics | `results/eda/clinical_features/` |
| 04 | Collinearity and VIF | `results/eda/clinical_features/` |
| 05 | Cleaning; builds the analysis datasets | `data/clean/` |
| 06-07 | Descriptive statistics, clinical and proteomic | `results/eda/` |
| 08 | Clinical risk score validation | `results/risk_scores_validation/` |
| 09 | Phase 1: clinical models | `results/models/clinical_data/` |
| 10 | Phase 1b: filtered clinical models | `results/models/clinical_data_filtered/` |
| 11 | Phase 2: proteomic models | `results/models/proteomic_data/` |
| 12 | Phase 3: multimodal and matched clinical models | `results/models/multimodal_data/` |
| 13 | Modality comparison | `results/models/modality_comparison/` |
| 14 | Threshold sensitivity of the best models | `results/models/best_model/` |
| 15 | Publication-ready Excel workbooks | `results/publication_tables/` |
