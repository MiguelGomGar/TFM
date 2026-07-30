"""Compare modelling arms trained on the same partition, model by model.

Two comparisons are run:
  - Clinical (matched) vs proteomic vs multimodal: the three arms of phase 3, trained on the
    same matched subcohort. Isolates the contribution of the proteomic panel, both alone and
    combined with the clinical data.
  - Clinical vs proteomic clinical filtered: phases 1 and 1b, trained on the full
    clinical cohort. Isolates the effect of the Elastic Net feature filtering.

No model is refitted here: each comparison is rebuilt from the metrics
tables, so the figures can be regenerated cheaply.
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
