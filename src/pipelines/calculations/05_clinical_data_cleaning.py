"""Clean the clinical and proteomic datasets."""

from pathlib import Path

from src.data.data_cleaning import build_analysis_datasets
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

CLINICAL_INPUT_FILE = INTERMEDIATE_CLINICAL_DATA_PATH
PROTEOMIC_INPUT_FILE = RAW_PROTEOMIC_DATA_PATH
OUTPUT_DIR = CLEAN_DATA_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main
def main() -> None:
    """Build the analysis-ready clinical, proteomic and multimodal datasets."""
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading clinical data from {CLINICAL_INPUT_FILE}...")
    clinical_data = read_parquet(CLINICAL_INPUT_FILE)

    logger.info(f"Loading proteomic data from {PROTEOMIC_INPUT_FILE}...")
    proteomic_data = read_csv(PROTEOMIC_INPUT_FILE)

    datasets = build_analysis_datasets(clinical_data, proteomic_data, logger=logger)

    for label, dataset, output_path in [
        ("risk scores", datasets.risk_scores, RISK_SCORES_DATA_PATH),
        ("clinical", datasets.clinical, CLEANED_CLINICAL_DATA_PATH),
        ("proteomic", datasets.proteomic, CLEANED_PROTEOMIC_DATA_PATH),
        (
            "clinical data matched to the proteomic cohort",
            datasets.clinical_matched,
            CLEANED_CLINICAL_MATCHED_DATA_PATH,
        ),
        ("multimodal", datasets.multimodal, CLEANED_MULTIMODAL_DATA_PATH),
    ]:
        logger.info(f"Saving {label} data to {output_path}...")
        save_parquet(dataset, output_path)


if __name__ == "__main__":
    main()
