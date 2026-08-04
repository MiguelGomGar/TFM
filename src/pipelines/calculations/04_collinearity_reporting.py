"""Compute binary correlation matrices and multicollinearity (VIF) diagnostics
for the clinical dataset.

This pipeline merges the former 04_binary_correlation_reporting and
05_multicollinearity_reporting into a single collinearity analysis pipeline.
Its heatmaps and bar chart are drawn by
src/pipelines/plots/04_collinearity_plots.py.
"""

from pathlib import Path

from src.data.collinearity_analysis import (
    compute_num_corr_matrix,
    compute_cat_corr_matrix,
    compute_vif_data,
)
from src.data.data_cleaning import (
    drop_columns,
    drop_high_missingness_columns,
    drop_columns_by_prefix,
)
from src.utils.filenames import CORRELATION_MATRIX_FILE, VIF_FILE
from src.utils.io import read_parquet, save_csv
from src.utils.logging_utils import setup_logger
from src.utils.paths import CLINICAL_EDA_DIR, INTERMEDIATE_CLINICAL_DATA_PATH
from src.config import (
    MISSING_RATE_THRESHOLD,
    IDENTIFIER_VARIABLE,
    RISK_SCORES_PREFIX,
    HIGHLY_CORRELATED_FEATURES,
    VIF_THRESHOLD,
)

INPUT_FILE = INTERMEDIATE_CLINICAL_DATA_PATH
OUTPUT_DIR = CLINICAL_EDA_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading data from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)

    logger.info("Dropping identifier features...")
    df = drop_columns(df, IDENTIFIER_VARIABLE)

    logger.info("Dropping features with high missing rates...")
    df = drop_high_missingness_columns(df, threshold=MISSING_RATE_THRESHOLD)

    logger.info("Dropping risk scores computed columns...")
    df = drop_columns_by_prefix(df, RISK_SCORES_PREFIX)

    logger.info("Computing numeric correlation matrix...")
    num_corr_data = compute_num_corr_matrix(df)
    save_csv(
        num_corr_data,
        OUTPUT_DIR / CORRELATION_MATRIX_FILE.format(dtype="num"),
        index=True,
    )

    logger.info("Computing categorical correlation matrix...")
    cat_corr_data = compute_cat_corr_matrix(df)
    save_csv(
        cat_corr_data,
        OUTPUT_DIR / CORRELATION_MATRIX_FILE.format(dtype="cat"),
        index=True,
    )

    logger.info("Dropping highly correlated features...")
    df = drop_columns(df, HIGHLY_CORRELATED_FEATURES)

    logger.info("Computing VIF diagnostics...")
    vif_series = compute_vif_data(df, threshold=VIF_THRESHOLD)
    save_csv(vif_series.to_frame(name="VIF"), OUTPUT_DIR / VIF_FILE, index=True)


if __name__ == "__main__":
    main()
