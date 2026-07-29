"""Clean the clinical and proteomic datasets."""

from pathlib import Path

from src.data.feature_engineering import compute_risk_scores
from src.data.data_cleaning import (
    drop_columns,
    drop_high_missingness_columns,
    drop_high_missingness_rows,
    drop_columns_by_prefix,
    inner_join,
    mask_out_of_range_values,
)
from src.utils.io import read_csv, read_parquet, save_parquet
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    CLEAN_DATA_DIR,
    CLEANED_CLINICAL_DATA_PATH,
    CLEANED_CLINICAL_MATCHED_DATA_PATH,
    CLEANED_MULTIMODAL_DATA_PATH,
    CLEANED_PROTEOMIC_DATA_PATH,
    INTERMEDIATE_CLINICAL_DATA_PATH,
    RAW_PROTEOMIC_DATA_PATH,
    RISK_SCORES_DATA_PATH,
)
from src.config import (
    MISSING_RATE_THRESHOLD,
    PLAUSIBLE_RANGES,
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

    # Replace impossible values with NaN, and log the number of values replaced
    # for each column. The ranges are defined in the config file.
    logger.info("Masking physiologically implausible values...")
    clinical_data, masked_counts = mask_out_of_range_values(
        clinical_data, PLAUSIBLE_RANGES
    )
    if masked_counts:
        for column, count in masked_counts.items():
            logger.info(f"  Masked {count} out-of-range value(s) in '{column}'")
    else:
        logger.info("  No out-of-range values found.")

    # Feature engineering: compute risk scores and save them to a separate file.
    logger.info("Computing risk scores...")
    risk_scores_data = compute_risk_scores(clinical_data)

    logger.info(f"Saving risk scores data to {RISK_SCORES_DATA_PATH}...")
    save_parquet(risk_scores_data, RISK_SCORES_DATA_PATH)

    # Clinical data cleaning based on missingness and collinearity analysis.
    # The identifier column is set aside first: it is never missing, so
    # leaving it in would deflate every row's missing rate and change which
    # rows survive drop_high_missingness_rows.
    identifier_column = clinical_data[IDENTIFIER_VARIABLE]

    logger.info("Dropping features with high missing rates...")
    clinical_data = drop_high_missingness_columns(
        clinical_data.drop(columns=IDENTIFIER_VARIABLE),
        threshold=MISSING_RATE_THRESHOLD,
    )

    logger.info("Dropping rows with high missing rates...")
    clinical_data = drop_high_missingness_rows(
        clinical_data, threshold=MISSING_RATE_THRESHOLD
    )
    clinical_data.insert(0, IDENTIFIER_VARIABLE, identifier_column.loc[clinical_data.index])

    logger.info("Dropping highly correlated features...")
    clinical_data = drop_columns(clinical_data, HIGHLY_CORRELATED_FEATURES)

    logger.info("Dropping risk scores...")
    clinical_data = drop_columns_by_prefix(clinical_data, RISK_SCORES_PREFIX)

    logger.info(f"Loading proteomic data from {PROTEOMIC_INPUT_FILE}...")
    proteomic_data = read_csv(PROTEOMIC_INPUT_FILE)

    # Save a full joined dataset with the clinical and proteomic data
    multimodal_data = inner_join(
        clinical_data,
        proteomic_data,
        on=IDENTIFIER_VARIABLE,
    )

    # Build the proteomic dataset by merging with the clinical dataset
    # (AF recurrence and group allocation variables) on the identifier feature.
    logger.info(f"Extracting merge subset for proteomic data...")
    clinical_merge_subset = clinical_data[
        [IDENTIFIER_VARIABLE, GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE]
    ]

    logger.info(f"Merging proteomic and clinical subset by {IDENTIFIER_VARIABLE}...")
    proteomic_data = inner_join(
        proteomic_data,
        clinical_merge_subset,
        on=IDENTIFIER_VARIABLE,
    )

    # Clinical data restricted to the proteomic cohort (same rows as the
    # multimodal dataset), selected before the identifier column is dropped.
    logger.info("Extracting clinical data matched to the proteomic cohort...")
    clinical_matched_data = multimodal_data[clinical_data.columns]

    # Drop identifier features to get the final cleaned datasets ready to be
    # used in modelling.
    logger.info("Dropping identifier features for all datasets...")
    clinical_data = drop_columns(clinical_data, IDENTIFIER_VARIABLE)
    proteomic_data = drop_columns(proteomic_data, IDENTIFIER_VARIABLE)
    clinical_matched_data = drop_columns(clinical_matched_data, IDENTIFIER_VARIABLE)

    logger.info(f"Saving clinical data at {CLEANED_CLINICAL_DATA_PATH}...")
    save_parquet(clinical_data, CLEANED_CLINICAL_DATA_PATH)

    logger.info(f"Saving cleaned proteomic data to {CLEANED_PROTEOMIC_DATA_PATH}...")
    save_parquet(proteomic_data, CLEANED_PROTEOMIC_DATA_PATH)

    logger.info(
        f"Saving clinical data matched to the proteomic cohort to "
        f"{CLEANED_CLINICAL_MATCHED_DATA_PATH}..."
    )
    save_parquet(clinical_matched_data, CLEANED_CLINICAL_MATCHED_DATA_PATH)

    logger.info(f"Saving multimodal dataset to {CLEANED_MULTIMODAL_DATA_PATH}...")
    save_parquet(multimodal_data, CLEANED_MULTIMODAL_DATA_PATH)


if __name__ == "__main__":
    main()
