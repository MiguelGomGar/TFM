"""Draw the clinical distribution figures computed by pipeline 06.

The cleaned dataset is read only to enumerate the numeric and categorical
feature names; every value plotted comes from the tables pipeline 06 saved.
"""

from pathlib import Path

from src.config import GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE
from src.data.statistical_analysis import load_qq_data
from src.utils.dataframe_utils import get_categorical_columns, get_numeric_columns
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
from src.utils.paths import CLEANED_CLINICAL_DATA_PATH, CLINICAL_EDA_DIR
from src.visualization.statistical_analysis import (
    plot_categorical_distribution,
    plot_numeric_distribution,
    plot_qq,
    plot_stratified_categorical_distribution,
    plot_stratified_numeric_distribution,
)

SCHEMA_FILE = CLEANED_CLINICAL_DATA_PATH
INPUT_DIR = CLINICAL_EDA_DIR
OUTPUT_DIR = CLINICAL_EDA_DIR

logger = setup_logger(Path(__file__).stem)


def main() -> None:
    logger.info(f"Reading the feature names from {SCHEMA_FILE}...")
    clinical_data = read_parquet(SCHEMA_FILE)
    numeric_features = get_numeric_columns(clinical_data)
    categorical_features = get_categorical_columns(clinical_data)

    logger.info("Plotting global distributions for numeric features...")
    for feature in numeric_features:
        numeric_values = read_csv(
            INPUT_DIR / DISTRIBUTION_FILE.format(feature=feature)
        )[feature]
        save_figure(
            plot_numeric_distribution(numeric_values, feature),
            OUTPUT_DIR / DISTRIBUTION_FIGURE.format(feature=feature),
        )

    logger.info("Plotting Q-Q plots for numeric features...")
    for feature in numeric_features:
        qq_data = load_qq_data(read_csv(INPUT_DIR / QQ_FILE.format(feature=feature)))
        save_figure(
            plot_qq(qq_data), OUTPUT_DIR / QQ_FIGURE.format(feature=feature)
        )

    logger.info("Plotting stratified distributions for numeric features...")
    for target_var in [GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE]:
        for feature in numeric_features:
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

    logger.info("Plotting global distributions for categorical features...")
    for feature in categorical_features:
        categorical_summary = read_csv(
            INPUT_DIR / DISTRIBUTION_FILE.format(feature=feature)
        )
        save_figure(
            plot_categorical_distribution(categorical_summary, feature),
            OUTPUT_DIR / DISTRIBUTION_FIGURE.format(feature=feature),
        )

    logger.info("Plotting stratified distributions for categorical features...")
    for target_var in [GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE]:
        for feature in categorical_features:
            if feature == target_var:
                continue
            # Saved with its index: it holds the levels of the stratifying
            # variable that label the bars.
            stratified_data = read_csv(
                INPUT_DIR
                / STRATIFIED_DISTRIBUTION_FILE.format(
                    feature=feature, group=target_var
                ),
                index_col=0,
            )
            figure = plot_stratified_categorical_distribution(
                stratified_data, feature, target_var
            )
            if figure is None:
                logger.warning(
                    f"Nothing to plot for {feature} stratified by {target_var}; skipping."
                )
                continue
            save_figure(
                figure,
                OUTPUT_DIR
                / STRATIFIED_DISTRIBUTION_FIGURE.format(
                    feature=feature, group=target_var
                ),
            )


if __name__ == "__main__":
    main()
