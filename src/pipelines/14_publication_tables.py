"""Consolidate the modelling results into a handful of publication-ready tables.

Every modelling phase (09-12) and the modality/filtering comparisons (13)
already save correct, granular results (per-fold metrics, per-model
Mean/Std summaries, per-model comparison CSVs). This pipeline does not
recompute anything: it only reads those artifacts back and reshapes them
into the small set of wide tables that belong in the manuscript body, one
per metric (ROC-AUC, PR-AUC), namely:

  - One performance table per modelling phase (clinical, clinical filtered,
    proteomic, multimodal) per metric: one row per model, with a "mean (SD)"
    column for the Train partition and one for the Validation partition of
    the internal cross-validation.
  - One incremental-value table per modality/filtering comparison (clinical
    vs multimodal, clinical vs clinical filtered) per metric: one row per
    model, with one column per modality and a Delta column, already computed
    by pipeline 13.
  - One hyperparameters table per model per modelling phase, named
    '{phase}_{model}.csv' (e.g. 'clinical_EN.csv'), with two columns
    ['Parameter', 'Value'] and scikit-learn pipeline prefixes (e.g. 'clf__',
    'clf__estimator__') stripped from the parameter names.

Everything else (per-fold metrics, feature selection lists, raw curve
coordinates) stays in results/models/ as supplementary material and is
intentionally not duplicated here.
"""

from pathlib import Path

from src.config import (
    COMPARISON_PHASES,
    PERFORMANCE_PHASES,
    PUBLICATION_METRICS,
    PUBLICATION_TABLES_DECIMALS,
)
from src.utils.logging_utils import setup_logger
from src.utils.paths import HYPERPARAMETERS_TABLES_DIR, PUBLICATION_TABLES_DIR
from src.utils.results_saving import (
    build_comparison_tables,
    build_hyperparameters_tables,
    build_performance_tables,
)

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    PUBLICATION_TABLES_DIR.mkdir(parents=True, exist_ok=True)
    HYPERPARAMETERS_TABLES_DIR.mkdir(parents=True, exist_ok=True)

    build_performance_tables(
        PERFORMANCE_PHASES,
        PUBLICATION_METRICS,
        PUBLICATION_TABLES_DIR,
        decimals=PUBLICATION_TABLES_DECIMALS,
        logger=logger,
    )
    build_comparison_tables(
        COMPARISON_PHASES,
        PUBLICATION_METRICS,
        PUBLICATION_TABLES_DIR,
        decimals=PUBLICATION_TABLES_DECIMALS,
        logger=logger,
    )
    build_hyperparameters_tables(
        PERFORMANCE_PHASES, HYPERPARAMETERS_TABLES_DIR, logger=logger
    )

    logger.info(f"Publication tables saved to {PUBLICATION_TABLES_DIR}.")


if __name__ == "__main__":
    main()
