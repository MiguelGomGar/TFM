"""Draw the proteomic distribution figures computed by pipeline 07.

The cleaned dataset is read only to enumerate the protein names; every value
plotted comes from the tables pipeline 07 saved.
"""

from pathlib import Path

from src.config import GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE
from src.data.statistical_analysis import load_qq_data
from src.utils.dataframe_utils import get_proteomic_features
from src.utils.filenames import (
    DISTRIBUTION_FIGURE,
    DISTRIBUTION_FILE,
    QQ_FIGURE,
    QQ_FILE,
    STRATIFIED_DISTRIBUTION_FIGURE,
    STRATIFIED_DISTRIBUTION_FILE,
)
from src.utils.io import read_csv, read_parquet, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import CLEANED_PROTEOMIC_DATA_PATH, PROTEOMIC_EDA_DIR
from src.visualization.statistical_analysis import (
    plot_numeric_distribution,
    plot_qq,
    plot_stratified_numeric_distribution,
)

SCHEMA_FILE = CLEANED_PROTEOMIC_DATA_PATH
INPUT_DIR = PROTEOMIC_EDA_DIR
OUTPUT_DIR = PROTEOMIC_EDA_DIR

logger = setup_logger(Path(__file__).stem)


def main() -> None:
    """Draw the proteomic distribution, Q-Q and stratified figures."""
    logger.info(f"Reading the protein names from {SCHEMA_FILE}...")
    proteomic_data = read_parquet(SCHEMA_FILE)
    proteomic_features = get_proteomic_features(proteomic_data)

    logger.info("Plotting global distributions for proteomic features...")
    for feature in proteomic_features:
        numeric_values = read_csv(
            INPUT_DIR / DISTRIBUTION_FILE.format(feature=feature)
        )[feature]
        save_figure(
            plot_numeric_distribution(numeric_values, feature),
            OUTPUT_DIR / DISTRIBUTION_FIGURE.format(feature=feature),
        )

    logger.info("Plotting Q-Q plots for proteomic features...")
    for feature in proteomic_features:
        qq_data = load_qq_data(read_csv(INPUT_DIR / QQ_FILE.format(feature=feature)))
        save_figure(plot_qq(qq_data), OUTPUT_DIR / QQ_FIGURE.format(feature=feature))

    logger.info("Plotting stratified distributions for proteomic features...")
    for target_var in [GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE]:
        for feature in proteomic_features:
            stratified_data = read_csv(
                INPUT_DIR
                / STRATIFIED_DISTRIBUTION_FILE.format(feature=feature, group=target_var)
            )
            save_figure(
                plot_stratified_numeric_distribution(
                    stratified_data, feature, target_var
                ),
                OUTPUT_DIR
                / STRATIFIED_DISTRIBUTION_FIGURE.format(
                    feature=feature, group=target_var
                ),
            )


if __name__ == "__main__":
    main()
