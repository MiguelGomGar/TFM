"""Generate statistical reporting plots and tables."""

from pathlib import Path

from src.data.statistical_analysis import (
    compute_numeric_distribution,
    compute_categorical_distribution,
    compute_stratified_numeric_distribution,
    compute_stratified_categorical_distribution,
    compute_qq,
    create_table1,
)
from src.utils.dataframe_utils import get_categorical_columns, get_numeric_columns
from src.utils.io import read_parquet, save_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    CLEANED_CLINICAL_DATA_PATH,
    CLEANED_PROTEOMIC_DATA_PATH,
    CLINICAL_EDA_DIR,
    TABLE1_PATH,
)
from src.visualization.statistical_analysis import (
    plot_numeric_distribution,
    plot_categorical_distribution,
    plot_stratified_numeric_distribution,
    plot_stratified_categorical_distribution,
    plot_qq,
)
from src.config import TARGET_VARIABLE, GROUP_ALLOCATION_VARIABLE, NON_NORMAL_VARIABLES

INPUT_FILE = CLEANED_CLINICAL_DATA_PATH
OUTPUT_DIR = CLINICAL_EDA_DIR

logger = setup_logger(Path(__file__).stem)


def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading clinical data from {INPUT_FILE}...")
    clinical_data = read_parquet(INPUT_FILE)

    numeric_features = get_numeric_columns(clinical_data)
    categorical_features = get_categorical_columns(clinical_data)

    logger.info("Plotting global distributions for numeric features...")
    for feature in numeric_features:
        numeric_values = compute_numeric_distribution(clinical_data, feature)
        fig = plot_numeric_distribution(numeric_values, feature)
        save_figure(fig, OUTPUT_DIR / f"distribution_{feature}.png")

    logger.info("Generating Q-Q plots for numeric features...")
    for feature in numeric_features:
        qq_data = compute_qq(clinical_data, feature)
        fig = plot_qq(qq_data)
        save_figure(fig, OUTPUT_DIR / f"distribution_{feature}_QQ.png")

    logger.info("Plotting stratified distributions for numeric features...")
    for target_var in [GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE]:
        for feature in numeric_features:
            stratified_data = compute_stratified_numeric_distribution(
                clinical_data, feature, target_var
            )
            fig = plot_stratified_numeric_distribution(
                stratified_data, feature, target_var
            )
            save_figure(
                fig,
                OUTPUT_DIR / f"distribution_{feature}_stratified_by_{target_var}.png",
            )

    logger.info("Plotting global distributions for categorical features...")
    for feature in categorical_features:
        categorical_summary = compute_categorical_distribution(clinical_data, feature)
        fig = plot_categorical_distribution(categorical_summary, feature)
        save_figure(fig, OUTPUT_DIR / f"distribution_{feature}.png")

    logger.info(f"Plotting stratified distributions for categorical features...")
    for feature in categorical_features:
        if feature == target_var:
            continue
        stratified_data = compute_stratified_categorical_distribution(
            clinical_data, feature, target_var
        )
        fig = plot_stratified_categorical_distribution(
            stratified_data, feature, target_var
        )
        save_figure(
            fig,
            OUTPUT_DIR / f"distribution_{feature}_stratified_by_{target_var}.png",
        )

    logger.info(f"Creating table 1 stratified by {TARGET_VARIABLE}...")
    categorical_features = get_categorical_columns(clinical_data)
    table1 = create_table1(
        data=clinical_data,
        strat_var=TARGET_VARIABLE,
        cat_vars=categorical_features + [TARGET_VARIABLE],
        nonnormal_vars=NON_NORMAL_VARIABLES,
    )

    logger.info(f"Saving table 1 to {TABLE1_PATH}...")
    save_csv(table1, TABLE1_PATH)


if __name__ == "__main__":
    main()
