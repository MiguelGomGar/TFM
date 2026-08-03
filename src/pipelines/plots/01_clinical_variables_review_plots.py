"""Draw the clinical variables review bar chart computed by pipeline 01."""

from pathlib import Path

from src.utils.io import read_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import DATA_COLLECTION_DIR
from src.visualization.clinical_variables_review import (
    build_clinical_variables_review_plot,
)

INPUT_FILE = DATA_COLLECTION_DIR / "clinical_variables_review_plot.csv"
OUTPUT_FILE = DATA_COLLECTION_DIR / "clinical_variables_review_plot.png"

logger = setup_logger(Path(__file__).stem)


# %% Main
def main() -> None:
    logger.info(f"Loading the review table from {INPUT_FILE}...")
    df = read_csv(INPUT_FILE)

    logger.info(f"Saving the plot to {OUTPUT_FILE}...")
    save_figure(build_clinical_variables_review_plot(df), OUTPUT_FILE, dpi=300)


if __name__ == "__main__":
    main()
