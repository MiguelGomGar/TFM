"""Compute the missing-value diagnostics, before and after cleaning.

Their heatmaps and bar charts are drawn by
src/pipelines/plots/03_missing_values_plots.py.
"""

from pathlib import Path

from src.config import MISSING_RATE_THRESHOLD, TARGET_VARIABLE
from src.data.missing_values_analysis import (
    compute_column_missingness_data,
    compute_missingness_heatmap_data,
    compute_row_missingness_data,
    compute_stratified_missingness_data,
)
from src.data.data_cleaning import (
    drop_high_missingness_columns,
    drop_high_missingness_rows,
)
from src.utils.filenames import (
    MISSING_HEATMAP_FILE,
    MISSING_PER_FEATURE_FILE,
    MISSING_PER_RECORD_FILE,
    MISSING_STRATIFIED_FILE,
)
from src.utils.io import read_parquet, save_csv
from src.utils.logging_utils import setup_logger
from src.utils.paths import CLINICAL_EDA_DIR, INTERMEDIATE_CLINICAL_DATA_PATH

INPUT_FILE = INTERMEDIATE_CLINICAL_DATA_PATH
OUTPUT_DIR = CLINICAL_EDA_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading data from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)

    logger.info("Computing the missing values heatmap...")
    heatmap_data = compute_missingness_heatmap_data(df)
    save_csv(
        heatmap_data, OUTPUT_DIR / MISSING_HEATMAP_FILE.format(stage="before"), index=True
    )

    logger.info("Computing missing values per feature...")
    col_na_data = compute_column_missingness_data(df)
    save_csv(col_na_data, OUTPUT_DIR / MISSING_PER_FEATURE_FILE)

    logger.info(f"Computing missing values stratified by {TARGET_VARIABLE}...")
    strat_na_data = compute_stratified_missingness_data(df, stratify_by=TARGET_VARIABLE)
    save_csv(
        strat_na_data,
        OUTPUT_DIR / MISSING_STRATIFIED_FILE.format(stage="before"),
        index=True,
    )

    logger.info("Computing the missing values distribution per row...")
    row_na_data = compute_row_missingness_data(df)
    save_csv(row_na_data, OUTPUT_DIR / MISSING_PER_RECORD_FILE.format(stage="before"))

    logger.info("Dropping features with high missingness...")
    df_filtered = drop_high_missingness_columns(df, MISSING_RATE_THRESHOLD)

    logger.info("Dropping rows with high missingness...")
    df_filtered = drop_high_missingness_rows(df_filtered, MISSING_RATE_THRESHOLD)

    logger.info("Recomputing missing values per row...")
    row_na_data_filtered = compute_row_missingness_data(df_filtered)
    save_csv(
        row_na_data_filtered, OUTPUT_DIR / MISSING_PER_RECORD_FILE.format(stage="after")
    )

    logger.info(f"Recomputing missing values stratified by {TARGET_VARIABLE}...")
    strat_na_data = compute_stratified_missingness_data(
        df_filtered, stratify_by=TARGET_VARIABLE
    )
    save_csv(
        strat_na_data,
        OUTPUT_DIR / MISSING_STRATIFIED_FILE.format(stage="after"),
        index=True,
    )

    logger.info("Recomputing the missing values heatmap...")
    heatmap_data_filtered = compute_missingness_heatmap_data(df_filtered)
    save_csv(
        heatmap_data_filtered,
        OUTPUT_DIR / MISSING_HEATMAP_FILE.format(stage="after"),
        index=True,
    )


if __name__ == "__main__":
    main()
