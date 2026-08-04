"""Consolidate the modelling results into publication-ready Excel workbooks.

Each workbook groups one family of tables as multiple sheets, with every
table itself a multi-indexed, richly formatted pandas table (not a plain
CSV) so several metrics/partitions/parameters can be read at a glance:

- performance_tables.xlsx: one sheet per modelling phase, rows = models,
  columns = (Metric, Partition).
- comparison_tables.xlsx: one sheet per modality/filtering comparison, rows =
  models, columns = (Metric, {baseline, comparison, Delta}).
- hyperparameters_tables.xlsx: a single sheet comparing every modelling
  phase, rows = (Model, Parameter), columns = (Value, Phase).
"""

from pathlib import Path

from src.config import (
    COMPARISON_PHASES,
    PERFORMANCE_PHASES,
    PUBLICATION_METRICS,
    PUBLICATION_TABLES_DECIMALS,
)
from src.utils.logging_utils import setup_logger
from src.utils.paths import PUBLICATION_TABLES_DIR
from src.models.results_saving import (
    build_comparison_tables,
    build_hyperparameters_tables,
    build_performance_tables,
)

logger = setup_logger(Path(__file__).stem)

PERFORMANCE_WORKBOOK = PUBLICATION_TABLES_DIR / "performance_tables.xlsx"
COMPARISON_WORKBOOK = PUBLICATION_TABLES_DIR / "comparison_tables.xlsx"
HYPERPARAMETERS_WORKBOOK = PUBLICATION_TABLES_DIR / "hyperparameters_tables.xlsx"


# %% Main function
def main() -> None:
    PUBLICATION_TABLES_DIR.mkdir(parents=True, exist_ok=True)

    build_performance_tables(
        PERFORMANCE_PHASES,
        PUBLICATION_METRICS,
        PERFORMANCE_WORKBOOK,
        decimals=PUBLICATION_TABLES_DECIMALS,
        logger=logger,
    )
    build_comparison_tables(
        COMPARISON_PHASES,
        PUBLICATION_METRICS,
        COMPARISON_WORKBOOK,
        decimals=PUBLICATION_TABLES_DECIMALS,
        logger=logger,
    )
    build_hyperparameters_tables(
        PERFORMANCE_PHASES, HYPERPARAMETERS_WORKBOOK, logger=logger
    )

    logger.info(f"Publication tables saved to {PUBLICATION_TABLES_DIR}.")


if __name__ == "__main__":
    main()
