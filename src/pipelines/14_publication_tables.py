"""Consolidate the modelling results into a handful of publication-ready tables.

- One performance table per modelling phase per metric.
- One incremental-value table per modality/filtering comparison.
- One hyperparameters table per model per modelling phase.
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
from src.models.results_saving import (
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
