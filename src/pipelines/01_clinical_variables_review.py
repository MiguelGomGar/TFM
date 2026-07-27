"""Render the clinical variables review plot."""

from pathlib import Path

from src.config import VARIABLES_REVIEW_NROWS, VARIABLES_REVIEW_USECOLS
from src.utils.io import read_excel, save_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import DATA_COLLECTION_DIR, RISK_FACTORS_REVIEW_DATA_PATH
from src.visualization.data_collection import (
    build_clinical_variables_review_plot,
    prepare_clinical_variables_review_data,
)

INPUT_FILE = RISK_FACTORS_REVIEW_DATA_PATH
OUTPUT_FILE = DATA_COLLECTION_DIR / "clinical_variables_review_plot.png"
OUTPUT_DATA_FILE = DATA_COLLECTION_DIR / "clinical_variables_review_plot.csv"

logger = setup_logger(Path(__file__).stem)


# %% Main
def main() -> None:
    logger.info(f"Loading data from {INPUT_FILE}...")
    df = read_excel(
        INPUT_FILE,
        usecols=VARIABLES_REVIEW_USECOLS,
        nrows=VARIABLES_REVIEW_NROWS,
    )

    logger.info("Rearranging factor levels for plotting...")
    df = prepare_clinical_variables_review_data(df)

    logger.info("Generating the plot...")
    fig = build_clinical_variables_review_plot(df)

    # Ensure output directory exists
    OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)

    logger.info(f"Saving the plot to {OUTPUT_FILE}...")
    save_figure(fig, OUTPUT_FILE, dpi=300)

    logger.info(f"Saving the plot data to {OUTPUT_DATA_FILE}...")
    save_csv(df, OUTPUT_DATA_FILE)


if __name__ == "__main__":
    main()
