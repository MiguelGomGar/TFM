"""Draw the modelling-arm comparisons built by pipeline 13."""

from pathlib import Path

from src.config import MODALITY_COMPARISONS
from src.utils.logging_utils import setup_logger
from src.visualization.modelling_figures import plot_arm_comparison

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    """Draw the modelling-arm comparison bar charts."""
    for comparison_spec in MODALITY_COMPARISONS:
        plot_arm_comparison(**comparison_spec, logger=logger)


if __name__ == "__main__":
    main()
