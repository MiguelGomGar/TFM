"""Compute the descriptive statistics of the proteomic dataset.

The distribution, Q-Q and stratified tables saved here are turned into figures
by src/pipelines/plots/07_proteomic_statistical_plots.py.
"""

from pathlib import Path

from src.data.statistical_analysis import (
    build_qq_table,
    compute_numeric_distribution,
    compute_stratified_numeric_distribution,
    compute_qq,
)
from src.utils.filenames import (
    DISTRIBUTION_FILE,
    QQ_FILE,
    STRATIFIED_DISTRIBUTION_FILE,
)
from src.utils.dataframe_utils import get_proteomic_features
from src.utils.io import read_parquet, save_csv
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    CLEANED_PROTEOMIC_DATA_PATH,
    PROTEOMIC_EDA_DIR,
)
from src.config import TARGET_VARIABLE, GROUP_ALLOCATION_VARIABLE

INPUT_FILE = CLEANED_PROTEOMIC_DATA_PATH
OUTPUT_DIR = PROTEOMIC_EDA_DIR

logger = setup_logger(Path(__file__).stem)


def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading data from {INPUT_FILE}...")
    proteomic_data = read_parquet(INPUT_FILE)

    proteomic_features = get_proteomic_features(proteomic_data)

    logger.info("Computing global distributions for proteomic features...")
    for feature in proteomic_features:
        numeric_values = compute_numeric_distribution(proteomic_data, feature)
        save_csv(
            numeric_values.to_frame(name=feature),
            OUTPUT_DIR / DISTRIBUTION_FILE.format(feature=feature),
        )

    logger.info("Computing Q-Q coordinates for proteomic features...")
    for feature in proteomic_features:
        qq_data = compute_qq(proteomic_data, feature)
        save_csv(build_qq_table(qq_data), OUTPUT_DIR / QQ_FILE.format(feature=feature))

    logger.info("Computing stratified distributions for proteomic features...")
    for target_var in [GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE]:
        for feature in proteomic_features:
            stratified_data = compute_stratified_numeric_distribution(
                proteomic_data, feature, target_var
            )
            save_csv(
                stratified_data,
                OUTPUT_DIR
                / STRATIFIED_DISTRIBUTION_FILE.format(
                    feature=feature, group=target_var
                ),
            )


if __name__ == "__main__":
    main()
