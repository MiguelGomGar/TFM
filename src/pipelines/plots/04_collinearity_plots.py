"""Draw the collinearity diagnostics computed by pipeline 04."""

from pathlib import Path

from src.config import VIF_THRESHOLD
from src.utils.filenames import (
    CORRELATION_MATRIX_FIGURE,
    CORRELATION_MATRIX_FILE,
    VIF_FIGURE,
    VIF_FILE,
)
from src.utils.io import read_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import CLINICAL_EDA_DIR
from src.visualization.collinearity_analysis import plot_corr_matrix, plot_vif

INPUT_DIR = CLINICAL_EDA_DIR
OUTPUT_DIR = CLINICAL_EDA_DIR

# Correlation matrix flavour: file fragment, plot label and tick size.
CORRELATION_MATRICES = (("num", "Numeric", 14), ("cat", "Categorical", 10))

logger = setup_logger(Path(__file__).stem)


# %% Main
def main() -> None:
    for dtype, label, ticks_size in CORRELATION_MATRICES:
        logger.info(f"Plotting the {label.lower()} correlation matrix...")
        corr_data = read_csv(
            INPUT_DIR / CORRELATION_MATRIX_FILE.format(dtype=dtype), index_col=0
        )
        figure = plot_corr_matrix(corr_data, dtype=label, axis_ticks_size=ticks_size)
        if figure is None:
            logger.warning(f"No {label.lower()} correlations to plot; skipping.")
            continue
        save_figure(figure, OUTPUT_DIR / CORRELATION_MATRIX_FIGURE.format(dtype=dtype))

    logger.info("Plotting VIF diagnostics...")
    vif_series = read_csv(INPUT_DIR / VIF_FILE, index_col=0)["VIF"]
    figure = plot_vif(vif_series, threshold=VIF_THRESHOLD)
    if figure is None:
        logger.warning("No VIF values to plot; skipping.")
        return
    save_figure(figure, OUTPUT_DIR / VIF_FIGURE)


if __name__ == "__main__":
    main()
