"""Collect and prepare the clinical dataset."""

from pathlib import Path

from src.data.data_collection import (
    cast_clinical_numeric_columns,
    recode_clinical_categories,
    rename_clinical_features,
    select_clinical_features,
)
from src.utils.io import read_dta, save_parquet
from src.utils.logging_utils import setup_logger
from src.utils.paths import INTERMEDIATE_CLINICAL_DATA_PATH, RAW_CLINICAL_DATA_PATH

INPUT_FILE = RAW_CLINICAL_DATA_PATH
OUTPUT_FILE = INTERMEDIATE_CLINICAL_DATA_PATH

logger = setup_logger(Path(__file__).stem)


# %% Main
def main() -> None:
    """Select, rename and type the raw clinical variables."""
    logger.info(f"Loading raw data from {INPUT_FILE}...")
    df = read_dta(INPUT_FILE)

    logger.info("Selecting clinical features...")
    df = select_clinical_features(df)

    logger.info("Renaming clinical features...")
    df = rename_clinical_features(df)

    logger.info("Recode clinical categories...")
    df = recode_clinical_categories(df)

    logger.info("Casting clinical numeric columns...")
    df = cast_clinical_numeric_columns(df)

    logger.info(f"Saving prepared data to {OUTPUT_FILE}...")
    save_parquet(df, OUTPUT_FILE)


if __name__ == "__main__":
    main()
