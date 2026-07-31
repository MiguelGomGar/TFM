"""Generate missing-value diagnostic plots."""

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
from src.utils.io import read_parquet, save_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.visualization.missing_values_analysis import (
    plot_column_missingness,
    plot_missingness_heatmap,
    plot_row_missingness,
    plot_stratified_missingness,
)
from src.utils.paths import CLINICAL_EDA_DIR, INTERMEDIATE_CLINICAL_DATA_PATH

INPUT_FILE = INTERMEDIATE_CLINICAL_DATA_PATH
OUTPUT_DIR = CLINICAL_EDA_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading data from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)

    logger.info("Plotting missing values heatmap...")
    heatmap_data = compute_missingness_heatmap_data(df)
    heatmap_plot = plot_missingness_heatmap(heatmap_data)
    save_figure(heatmap_plot, OUTPUT_DIR / "missing_values_heatmap_before.png")
    save_csv(heatmap_data, OUTPUT_DIR / "missing_values_heatmap_before.csv", index=True)

    logger.info("Plotting missing values per feature...")
    col_na_data = compute_column_missingness_data(df)
    col_na_plot = plot_column_missingness(col_na_data, threshold=MISSING_RATE_THRESHOLD)
    save_figure(col_na_plot, OUTPUT_DIR / "missing_values_per_feature.png")
    save_csv(col_na_data, OUTPUT_DIR / "missing_values_per_feature.csv")

    logger.info(f"Plotting missing values stratified by {TARGET_VARIABLE}...")
    strat_na_data = compute_stratified_missingness_data(df, stratify_by=TARGET_VARIABLE)
    strat_na_plot = plot_stratified_missingness(
        strat_na_data, stratify_col_name=TARGET_VARIABLE
    )
    save_figure(strat_na_plot, OUTPUT_DIR / "missing_values_stratified_before.png")
    save_csv(
        strat_na_data, OUTPUT_DIR / "missing_values_stratified_before.csv", index=True
    )

    logger.info("Plotting missing values distribution per row...")
    row_na_data = compute_row_missingness_data(df)
    row_na_plot = plot_row_missingness(row_na_data)
    save_figure(row_na_plot, OUTPUT_DIR / "missing_values_per_record_before.png")
    save_csv(row_na_data, OUTPUT_DIR / "missing_values_per_record_before.csv")

    logger.info("Dropping features with high missingness...")
    df_filtered = drop_high_missingness_columns(df, MISSING_RATE_THRESHOLD)

    logger.info("Dropping rows with high missingness...")
    df_filtered = drop_high_missingness_rows(df_filtered, MISSING_RATE_THRESHOLD)

    logger.info("Plotting missing values per row...")
    row_na_data_filtered = compute_row_missingness_data(df_filtered)
    row_na_plot_filtered = plot_row_missingness(row_na_data_filtered)
    save_figure(
        row_na_plot_filtered, OUTPUT_DIR / "missing_values_per_record_after.png"
    )
    save_csv(row_na_data_filtered, OUTPUT_DIR / "missing_values_per_record_after.csv")

    logger.info(f"Plotting missing values stratified by {TARGET_VARIABLE}...")
    strat_na_data = compute_stratified_missingness_data(
        df_filtered, stratify_by=TARGET_VARIABLE
    )
    strat_na_plot = plot_stratified_missingness(
        strat_na_data, stratify_col_name=TARGET_VARIABLE
    )
    save_figure(strat_na_plot, OUTPUT_DIR / "missing_values_stratified_after.png")
    save_csv(
        strat_na_data, OUTPUT_DIR / "missing_values_stratified_after.csv", index=True
    )

    logger.info("Plotting new missing values heatmap...")
    heatmap_data_filtered = compute_missingness_heatmap_data(df_filtered)
    new_heatmap_plot = plot_missingness_heatmap(heatmap_data_filtered)
    save_figure(new_heatmap_plot, OUTPUT_DIR / "missing_values_heatmap_after.png")
    save_csv(
        heatmap_data_filtered,
        OUTPUT_DIR / "missing_values_heatmap_after.csv",
        index=True,
    )


if __name__ == "__main__":
    main()
