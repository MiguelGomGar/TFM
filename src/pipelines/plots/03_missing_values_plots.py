"""Draw the missing-value diagnostics computed by pipeline 03."""

from pathlib import Path

from src.config import MISSING_RATE_THRESHOLD, TARGET_VARIABLE
from src.utils.filenames import (
    MISSING_HEATMAP_FIGURE,
    MISSING_HEATMAP_FILE,
    MISSING_PER_FEATURE_FIGURE,
    MISSING_PER_FEATURE_FILE,
    MISSING_PER_RECORD_FIGURE,
    MISSING_PER_RECORD_FILE,
    MISSING_STRATIFIED_FIGURE,
    MISSING_STRATIFIED_FILE,
)
from src.utils.io import read_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import CLINICAL_EDA_DIR
from src.visualization.missing_values_analysis import (
    plot_column_missingness,
    plot_missingness_heatmap,
    plot_row_missingness,
    plot_stratified_missingness,
)

INPUT_DIR = CLINICAL_EDA_DIR
OUTPUT_DIR = CLINICAL_EDA_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main
def main() -> None:
    # The heatmap and the stratified tables were saved with their index, which
    # carries the record and feature labels the figures rely on.
    for stage in ("before", "after"):
        logger.info(f"Plotting the missing values heatmap ({stage} cleaning)...")
        heatmap_data = read_csv(
            INPUT_DIR / MISSING_HEATMAP_FILE.format(stage=stage), index_col=0
        )
        save_figure(
            plot_missingness_heatmap(heatmap_data),
            OUTPUT_DIR / MISSING_HEATMAP_FIGURE.format(stage=stage),
        )

        logger.info(f"Plotting missing values per record ({stage} cleaning)...")
        row_na_data = read_csv(INPUT_DIR / MISSING_PER_RECORD_FILE.format(stage=stage))
        save_figure(
            plot_row_missingness(row_na_data),
            OUTPUT_DIR / MISSING_PER_RECORD_FIGURE.format(stage=stage),
        )

        logger.info(
            f"Plotting missing values stratified by {TARGET_VARIABLE} ({stage} cleaning)..."
        )
        strat_na_data = read_csv(
            INPUT_DIR / MISSING_STRATIFIED_FILE.format(stage=stage), index_col=0
        )
        save_figure(
            plot_stratified_missingness(
                strat_na_data, stratify_col_name=TARGET_VARIABLE
            ),
            OUTPUT_DIR / MISSING_STRATIFIED_FIGURE.format(stage=stage),
        )

    logger.info("Plotting missing values per feature...")
    col_na_data = read_csv(INPUT_DIR / MISSING_PER_FEATURE_FILE)
    save_figure(
        plot_column_missingness(col_na_data, threshold=MISSING_RATE_THRESHOLD),
        OUTPUT_DIR / MISSING_PER_FEATURE_FIGURE,
    )


if __name__ == "__main__":
    main()
