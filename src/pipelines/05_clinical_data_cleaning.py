"""Clean the clinical and proteomic datasets."""

from pathlib import Path

import pandas as pd

from src.data.feature_engineering import compute_risk_scores
from src.data.data_cleaning import (
    drop_columns,
    drop_high_missingness_columns,
    drop_high_missingness_rows,
    drop_columns_by_prefix,
    inner_join,
)
from src.utils.io import read_csv, read_parquet, save_parquet
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    CLEAN_DATA_DIR,
    CLEANED_CLINICAL_DATA_PATH,
    CLEANED_PROTEOMIC_DATA_PATH,
    INTERMEDIATE_CLINICAL_DATA_PATH,
    RAW_PROTEOMIC_DATA_PATH,
    RISK_SCORES_DATA_PATH,
)
from src.config import (
    MISSING_RATE_THRESHOLD,
    IDENTIFIER_VARIABLE,
    RISK_SCORES_PREFIX,
    HIGHLY_CORRELATED_FEATURES,
    TARGET_VARIABLE,
    GROUP_ALLOCATION_VARIABLE,
)

CLINICAL_INPUT_FILE = INTERMEDIATE_CLINICAL_DATA_PATH
PROTEOMIC_INPUT_FILE = RAW_PROTEOMIC_DATA_PATH
OUTPUT_DIR = CLEAN_DATA_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading clinical data from {CLINICAL_INPUT_FILE}...")
    clinical_data = read_parquet(CLINICAL_INPUT_FILE)

    logger.info("Computing risk scores...")
    risk_scores_data = compute_risk_scores(clinical_data)

    logger.info(f"Saving risk scores data to {RISK_SCORES_DATA_PATH}...")
    save_parquet(risk_scores_data, RISK_SCORES_DATA_PATH)

    logger.info(f"Extracting merge subset for proteomic data...")
    clinical_merge_subset = clinical_data[
        [IDENTIFIER_VARIABLE, GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE]
    ]

    logger.info("Dropping identifier features...")
    clinical_data = drop_columns(clinical_data, IDENTIFIER_VARIABLE)

    logger.info("Dropping features with high missing rates...")
    clinical_data = drop_high_missingness_columns(
        clinical_data, threshold=MISSING_RATE_THRESHOLD
    )

    logger.info("Dropping rows with high missing rates...")
    clinical_data = drop_high_missingness_rows(
        clinical_data, threshold=MISSING_RATE_THRESHOLD
    )

    logger.info("Dropping highly correlated features...")
    clinical_data = drop_columns(clinical_data, HIGHLY_CORRELATED_FEATURES)

    logger.info("Dropping risk scores features...")
    clinical_data = drop_columns_by_prefix(clinical_data, RISK_SCORES_PREFIX)

    logger.info(f"Saving clinical data at {CLEANED_CLINICAL_DATA_PATH}...")
    save_parquet(clinical_data, CLEANED_CLINICAL_DATA_PATH)

    logger.info(f"Loading proteomic data from {PROTEOMIC_INPUT_FILE}...")
    proteomic_data = read_csv(PROTEOMIC_INPUT_FILE)

    logger.info(f"Merging proteomic and clinical data by {IDENTIFIER_VARIABLE}...")
    proteomic_data = inner_join(
        proteomic_data,
        clinical_merge_subset,
        on=IDENTIFIER_VARIABLE,
    )

    logger.info("Dropping the identifier column from the proteomic dataset...")
    proteomic_data = drop_columns(proteomic_data, IDENTIFIER_VARIABLE)

    logger.info(f"Saving cleaned proteomic data to {CLEANED_PROTEOMIC_DATA_PATH}...")
    save_parquet(proteomic_data, CLEANED_PROTEOMIC_DATA_PATH)


if __name__ == "__main__":
    main()
