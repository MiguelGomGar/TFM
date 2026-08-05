"""Draw the threshold-analysis figures computed by pipeline 14."""

from pathlib import Path

from src.config import BEST_MODELS, BEST_MODELS_COLORS
from src.utils.filenames import (
    CONFUSION_MATRIX_FIGURE,
    CONFUSION_MATRIX_FILE,
    THRESHOLD_COMPARISON_FIGURE,
    THRESHOLD_COMPARISON_FILE,
    THRESHOLD_INFO_FILE,
)
from src.utils.io import read_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import BEST_MODEL_DIR
from src.visualization.best_model import (
    plot_confusion_matrix,
    plot_threshold_metrics_comparison,
)

INPUT_DIR = BEST_MODEL_DIR
OUTPUT_DIR = BEST_MODEL_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    """Draw the confusion matrices and threshold comparison chart."""
    # The threshold table carries the scenario label each confusion matrix was
    # computed at, which is what titles its figure.
    threshold_info = read_csv(INPUT_DIR / THRESHOLD_INFO_FILE).set_index("Model")

    for abbreviation in BEST_MODELS:
        logger.info(f"Plotting the confusion matrix of {abbreviation}...")
        confusion_matrix = read_csv(
            INPUT_DIR / CONFUSION_MATRIX_FILE.format(model=abbreviation), index_col=0
        )
        save_figure(
            plot_confusion_matrix(
                confusion_matrix,
                title=f"{abbreviation} — {threshold_info.loc[abbreviation, 'Scenario']}",
            ),
            OUTPUT_DIR / CONFUSION_MATRIX_FIGURE.format(model=abbreviation),
        )

    logger.info("Plotting the cross-model comparison at the optimal threshold...")
    comparison_table = read_csv(INPUT_DIR / THRESHOLD_COMPARISON_FILE)
    save_figure(
        plot_threshold_metrics_comparison(
            comparison_table,
            title="Hard metrics at the Youden-optimal threshold",
            palette=BEST_MODELS_COLORS,
        ),
        OUTPUT_DIR / THRESHOLD_COMPARISON_FIGURE,
    )


if __name__ == "__main__":
    main()
