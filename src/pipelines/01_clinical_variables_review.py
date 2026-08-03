"""Prepare the clinical variables review table.

Its bar chart is drawn by
src/pipelines/plots/01_clinical_variables_review_plots.py.
"""

from pathlib import Path

from src.config import VARIABLES_REVIEW_NROWS, VARIABLES_REVIEW_USECOLS
from src.data.clinical_variables_review import prepare_clinical_variables_review_data
from src.utils.io import read_excel, save_csv
from src.utils.logging_utils import setup_logger
from src.utils.paths import DATA_COLLECTION_DIR, RISK_FACTORS_REVIEW_DATA_PATH

INPUT_FILE = RISK_FACTORS_REVIEW_DATA_PATH
OUTPUT_FILE = DATA_COLLECTION_DIR / "clinical_variables_review_plot.csv"

logger = setup_logger(Path(__file__).stem)


# %% Main
def main() -> None:
    OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading data from {INPUT_FILE}...")
    df = read_excel(
        INPUT_FILE,
        usecols=VARIABLES_REVIEW_USECOLS,
        nrows=VARIABLES_REVIEW_NROWS,
    )

    logger.info("Rearranging factor levels for plotting...")
    df = prepare_clinical_variables_review_data(df)

    logger.info(f"Saving the review table to {OUTPUT_FILE}...")
    save_csv(df, OUTPUT_FILE)


if __name__ == "__main__":
    main()
