"""
Compare modelling arms trained on the same partition, model by model.

Two comparisons are run:
  - Clinical vs clinical filtered: phases 1 and 1b, trained on the full clinical
  cohort.
  - Clinical matched vs proteomic vs multimodal: phases 2 and 3, trained on the
  matched subcohort.
"""

from pathlib import Path

from src.config import MODALITY_COMPARISONS
from src.utils.logging_utils import setup_logger
from src.visualization.modality_comparison import run_modality_comparison

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    for comparison_spec in MODALITY_COMPARISONS:
        run_modality_comparison(**comparison_spec, logger=logger)


if __name__ == "__main__":
    main()
