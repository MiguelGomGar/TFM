"""Generate statistical reporting plots and tables."""

from pathlib import Path

from src.data.statistical_analysis import (
    compute_numeric_distribution,
    compute_stratified_numeric_distribution,
    compute_qq,
)
from src.utils.io import read_parquet, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    CLEANED_PROTEOMIC_DATA_PATH,
    PROTEOMIC_EDA_DIR,
)
from src.visualization.statistical_analysis import (
    plot_numeric_distribution,
    plot_stratified_numeric_distribution,
    plot_qq,
)
from src.config import TARGET_VARIABLE, GROUP_ALLOCATION_VARIABLE

INPUT_FILE = CLEANED_PROTEOMIC_DATA_PATH
OUTPUT_DIR = PROTEOMIC_EDA_DIR

logger = setup_logger(Path(__file__).stem)


def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading data from {INPUT_FILE}...")
    proteomic_data = read_parquet(INPUT_FILE)

    proteomic_features = [
        col
        for col in proteomic_data.select_dtypes(include=["number"]).columns
        if col not in [GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE]
    ]

    logger.info("Plotting global distributions for proteomic features...")
    for feature in proteomic_features:
        numeric_values = compute_numeric_distribution(proteomic_data, feature)
        fig = plot_numeric_distribution(numeric_values, feature)
        save_figure(fig, OUTPUT_DIR / f"distribution_{feature}.png")

    logger.info("Generating Q-Q plots for proteomic data...")
    for feature in proteomic_features:
        qq_data = compute_qq(proteomic_data, feature)
        fig = plot_qq(qq_data)
        save_figure(fig, OUTPUT_DIR / f"distribution_{feature}_QQ.png")

    logger.info("Plotting stratified distributions for proteomic features...")
    for target_var in [GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE]:
        for feature in proteomic_features:
            stratified_data = compute_stratified_numeric_distribution(
                proteomic_data, feature, target_var
            )
            fig = plot_stratified_numeric_distribution(
                stratified_data, feature, target_var
            )
            save_figure(
                fig,
                OUTPUT_DIR / f"distribution_{feature}_stratified_by_{target_var}.png",
            )


if __name__ == "__main__":
    main()
