"""Compute the descriptive statistics of the clinical dataset, plus table 1.

The distribution, Q-Q and stratified tables saved here are turned into figures
by src/pipelines/plots/06_clinical_statistical_plots.py.
"""

from pathlib import Path

from src.data.statistical_analysis import (
    build_qq_table,
    compute_numeric_distribution,
    compute_categorical_distribution,
    compute_stratified_numeric_distribution,
    compute_stratified_categorical_distribution,
    compute_qq,
    create_table1,
)
from src.utils.dataframe_utils import (
    get_categorical_columns,
    get_numeric_columns,
    iter_stratified_pairs,
)
from src.utils.filenames import (
    DISTRIBUTION_FILE,
    QQ_FILE,
    STRATIFIED_DISTRIBUTION_FILE,
)
from src.utils.io import read_parquet, save_csv
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    CLEANED_CLINICAL_DATA_PATH,
    CLINICAL_EDA_DIR,
    TABLE1_PATH,
)
from src.config import TARGET_VARIABLE, GROUP_ALLOCATION_VARIABLE, NON_NORMAL_VARIABLES

INPUT_FILE = CLEANED_CLINICAL_DATA_PATH
OUTPUT_DIR = CLINICAL_EDA_DIR

# Reporting order of the stratifying variables, kept in step with the plotting
# mirror so both write and read the same filenames.
STRATIFY_ORDER = [GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE]

logger = setup_logger(Path(__file__).stem)


def main() -> None:
    """Save the clinical distribution, Q-Q and stratified tables, plus table 1."""
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading clinical data from {INPUT_FILE}...")
    clinical_data = read_parquet(INPUT_FILE)

    numeric_features = get_numeric_columns(clinical_data)
    categorical_features = get_categorical_columns(clinical_data)

    logger.info("Computing global distributions for numeric features...")
    for feature in numeric_features:
        numeric_values = compute_numeric_distribution(clinical_data, feature)
        save_csv(
            numeric_values.to_frame(name=feature),
            OUTPUT_DIR / DISTRIBUTION_FILE.format(feature=feature),
        )

    logger.info("Computing Q-Q coordinates for numeric features...")
    for feature in numeric_features:
        qq_data = compute_qq(clinical_data, feature)
        save_csv(build_qq_table(qq_data), OUTPUT_DIR / QQ_FILE.format(feature=feature))

    logger.info("Computing stratified distributions for numeric features...")
    for feature, target_var in iter_stratified_pairs(numeric_features, STRATIFY_ORDER):
        stratified_data = compute_stratified_numeric_distribution(
            clinical_data, feature, target_var
        )
        save_csv(
            stratified_data,
            OUTPUT_DIR
            / STRATIFIED_DISTRIBUTION_FILE.format(feature=feature, group=target_var),
        )

    logger.info("Computing global distributions for categorical features...")
    for feature in categorical_features:
        categorical_summary = compute_categorical_distribution(clinical_data, feature)
        save_csv(
            categorical_summary, OUTPUT_DIR / DISTRIBUTION_FILE.format(feature=feature)
        )

    logger.info("Computing stratified distributions for categorical features...")
    for feature, target_var in iter_stratified_pairs(
        categorical_features, STRATIFY_ORDER
    ):
        stratified_data = compute_stratified_categorical_distribution(
            clinical_data, feature, target_var
        )
        save_csv(
            stratified_data,
            OUTPUT_DIR
            / STRATIFIED_DISTRIBUTION_FILE.format(feature=feature, group=target_var),
            index=True,
        )

    logger.info(f"Creating table 1 stratified by {TARGET_VARIABLE}...")
    table1 = create_table1(
        df=clinical_data,
        target_var=TARGET_VARIABLE,
        nonnormal_vars=NON_NORMAL_VARIABLES,
    )

    logger.info(f"Saving table 1 to {TABLE1_PATH}...")
    save_csv(table1, TABLE1_PATH, index=True)


if __name__ == "__main__":
    main()
