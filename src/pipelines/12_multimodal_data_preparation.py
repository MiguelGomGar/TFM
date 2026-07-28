"""Build the matched multi-modal dataset used by the third modelling phase.

The cleaned clinical dataset drops the record identifier, so the clinical and
proteomic tables can no longer be matched. This pipeline replays the exact same
cleaning sequence as 05_clinical_data_cleaning on the intermediate clinical
data, keeps the identifier aside, and joins the surviving records against the
raw proteomic panel. The clinical part of the result is therefore identical to
data/clean/clinical_data.parquet, restricted to the proteomic subcohort.
"""

from pathlib import Path

from src.config import (
    HIGHLY_CORRELATED_FEATURES,
    IDENTIFIER_VARIABLE,
    MISSING_RATE_THRESHOLD,
    PLAUSIBLE_RANGES,
    RISK_SCORES_PREFIX,
    TARGET_VARIABLE,
)
from src.data.data_cleaning import (
    drop_columns,
    drop_columns_by_prefix,
    drop_high_missingness_columns,
    drop_high_missingness_rows,
    inner_join,
    mask_out_of_range_values,
)
from src.utils.io import read_csv, read_parquet, save_parquet
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    CLEAN_DATA_DIR,
    CLEANED_MULTIMODAL_DATA_PATH,
    INTERMEDIATE_CLINICAL_DATA_PATH,
    RAW_PROTEOMIC_DATA_PATH,
)

CLINICAL_INPUT_FILE = INTERMEDIATE_CLINICAL_DATA_PATH
PROTEOMIC_INPUT_FILE = RAW_PROTEOMIC_DATA_PATH
OUTPUT_DIR = CLEAN_DATA_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading clinical data from {CLINICAL_INPUT_FILE}...")
    clinical_data = read_parquet(CLINICAL_INPUT_FILE)

    logger.info("Masking physiologically implausible values...")
    clinical_data, masked_counts = mask_out_of_range_values(
        clinical_data, PLAUSIBLE_RANGES
    )
    for column, count in masked_counts.items():
        minimum, maximum = PLAUSIBLE_RANGES[column]
        logger.info(
            f"  {column}: {count} value(s) outside [{minimum}, {maximum}] set to NaN"
        )

    # Keep the identifier aside instead of carrying it through the cleaning
    # steps: drop_high_missingness_rows computes the missing rate over the
    # number of columns, so an extra never-missing column would change the
    # denominator and could alter which records survive.
    identifiers = clinical_data[IDENTIFIER_VARIABLE]

    logger.info("Applying the same cleaning sequence as pipeline 05...")
    clinical_data = drop_columns(clinical_data, IDENTIFIER_VARIABLE)
    clinical_data = drop_high_missingness_columns(
        clinical_data, threshold=MISSING_RATE_THRESHOLD
    )
    clinical_data = drop_high_missingness_rows(
        clinical_data, threshold=MISSING_RATE_THRESHOLD
    )
    clinical_data = drop_columns(clinical_data, HIGHLY_CORRELATED_FEATURES)
    clinical_data = drop_columns_by_prefix(clinical_data, RISK_SCORES_PREFIX)
    logger.info(
        f"Cleaned clinical data: {clinical_data.shape[0]} records, "
        f"{clinical_data.shape[1]} columns."
    )

    logger.info("Restoring the identifier on the surviving records...")
    clinical_data = clinical_data.join(identifiers)

    logger.info(f"Loading proteomic data from {PROTEOMIC_INPUT_FILE}...")
    proteomic_data = read_csv(PROTEOMIC_INPUT_FILE)

    logger.info(f"Merging both modalities by {IDENTIFIER_VARIABLE}...")
    multimodal_data = inner_join(
        proteomic_data,
        clinical_data,
        on=IDENTIFIER_VARIABLE,
    )
    multimodal_data = drop_columns(multimodal_data, IDENTIFIER_VARIABLE)

    prevalence = multimodal_data[TARGET_VARIABLE].eq("yes").mean()
    logger.info(
        f"Matched subcohort: {multimodal_data.shape[0]} records, "
        f"{multimodal_data.shape[1] - 1} predictors, "
        f"{TARGET_VARIABLE} prevalence {prevalence:.3f}."
    )
    logger.info(
        "Clinical predictors available for the reduced arm: "
        f"{[column for column in clinical_data.columns if column not in (IDENTIFIER_VARIABLE, TARGET_VARIABLE)]}"
    )

    logger.info(f"Saving multimodal data to {CLEANED_MULTIMODAL_DATA_PATH}...")
    save_parquet(multimodal_data, CLEANED_MULTIMODAL_DATA_PATH)


if __name__ == "__main__":
    main()
