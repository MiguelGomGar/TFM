"""Generate missing-value diagnostic plots."""

from pathlib import Path

from src.config import MISSING_RATE_THRESHOLD
from src.data.missing_values_analysis import (
    compute_column_missingness_data,
    compute_row_missingness_data,
)
from src.data.data_cleaning import (
    drop_high_missingness_columns,
    drop_high_missingness_rows,
)
from src.utils.io import read_parquet, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import CLINICAL_EDA_DIR, INTERMEDIATE_CLINICAL_DATA_PATH
from src.visualization.missing_values_analysis import (
    plot_column_missingness,
    plot_row_missingness,
)

INPUT_FILE = INTERMEDIATE_CLINICAL_DATA_PATH
OUTPUT_DIR = CLINICAL_EDA_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading data from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)

    logger.info("Plotting missing values per feature...")
    col_na_data = compute_column_missingness_data(df)
    col_na_plot = plot_column_missingness(col_na_data)
    save_figure(col_na_plot, OUTPUT_DIR / "missing_values_per_feature.png")

    logger.info("Plotting missing values per row...")
    row_na_data = compute_row_missingness_data(df)
    row_na_plot = plot_row_missingness(row_na_data)
    save_figure(row_na_plot, OUTPUT_DIR / "missing_values_per_record_before.png")

    logger.info("Dropping features with high missingness...")
    df_filtered = drop_high_missingness_columns(df, MISSING_RATE_THRESHOLD)

    logger.info("Dropping rows with high missingness...")
    df_filtered = drop_high_missingness_rows(df_filtered, MISSING_RATE_THRESHOLD)

    logger.info("Plotting missing values per row (after dropping columns)...")
    row_na_data_filtered = compute_row_missingness_data(df_filtered)
    row_na_plot_filtered = plot_row_missingness(row_na_data_filtered)
    save_figure(
        row_na_plot_filtered, OUTPUT_DIR / "missing_values_per_record_after.png"
    )

    logger.info("Plotting missing values per column (after dropping rows)...")
    col_na_data_filtered = compute_column_missingness_data(df_filtered)
    col_na_plot_filtered = plot_column_missingness(col_na_data_filtered)
    save_figure(
        col_na_plot_filtered, OUTPUT_DIR / "missing_values_per_feature_after.png"
    )


if __name__ == "__main__":
    main()
