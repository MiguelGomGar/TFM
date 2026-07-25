"""Generate statistical reporting plots and tables."""

from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

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
    PROTEOMIC_EDA_DIR,
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

CLINICAL_FILE = CLEANED_CLINICAL_DATA_PATH
PROTEOMIC_FILE = CLEANED_PROTEOMIC_DATA_PATH
CLINICAL_OUTPUT_DIR = CLINICAL_EDA_DIR
PROTEOMIC_OUTPUT_DIR = PROTEOMIC_EDA_DIR

logger = setup_logger(Path(__file__).stem)


def main() -> None:
    CLINICAL_OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    PROTEOMIC_OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading data from {CLINICAL_FILE}...")
    clinical_data = read_parquet(CLINICAL_FILE)

    numeric_features = get_numeric_columns(clinical_data)
    numeric_features = [
        f
        for f in numeric_features
        if f not in [GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE]
    ]
    categorical_features = get_categorical_columns(clinical_data)

    logger.info("Plotting numeric features distributions...")
    for feature in numeric_features:
        numeric_values = compute_numeric_distribution(clinical_data, feature)
        fig = plot_numeric_distribution(numeric_values, feature)
        save_figure(fig, CLINICAL_OUTPUT_DIR / f"distribution_{feature}.png")

    logger.info("Plotting categorical features distributions...")
    for feature in categorical_features:
        categorical_summary = compute_categorical_distribution(clinical_data, feature)
        fig = plot_categorical_distribution(categorical_summary, feature)
        save_figure(fig, CLINICAL_OUTPUT_DIR / f"distribution_{feature}.png")

    logger.info("Plotting stratified distributions...")
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
                CLINICAL_OUTPUT_DIR
                / f"distribution_{feature}_stratified_by_{target_var}.png",
            )

        for feature in categorical_features:
            if feature == target_var:
                continue
            stratified_data = compute_stratified_categorical_distribution(
                clinical_data, feature, target_var
            )
            if stratified_data is not None:
                fig = plot_stratified_categorical_distribution(
                    stratified_data, feature, target_var
                )
                if fig is not None:
                    save_figure(
                        fig,
                        CLINICAL_OUTPUT_DIR
                        / f"distribution_{feature}_stratified_by_{target_var}.png",
                    )

    logger.info("Generating Q-Q plots...")
    for feature in numeric_features:
        qq_data = compute_qq(clinical_data, feature)
        fig = plot_qq(qq_data)
        if fig is not None:
            save_figure(fig, CLINICAL_OUTPUT_DIR / f"distribution_{feature}_QQ.png")

    logger.info(f"Creating Table 1 stratified by {TARGET_VARIABLE}...")
    categorical_features = get_categorical_columns(clinical_data)
    table1 = create_table1(
        data=clinical_data,
        strat_var=TARGET_VARIABLE,
        cat_vars=categorical_features + [TARGET_VARIABLE],
        nonnormal_vars=NON_NORMAL_VARIABLES,
    )

    logger.info(f"Saving Table 1 to {TABLE1_PATH}...")
    save_csv(table1, TABLE1_PATH)

    logger.info(f"Loading data from {PROTEOMIC_FILE}...")
    proteomic_data = read_parquet(PROTEOMIC_FILE)

    proteomic_features = [
        col
        for col in proteomic_data.select_dtypes(include=["number"]).columns
        if col not in [GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE]
    ]

    logger.info("Plotting proteomic features distributions...")
    for feature in proteomic_features:
        numeric_values = compute_numeric_distribution(proteomic_data, feature)
        fig = plot_numeric_distribution(numeric_values, feature)
        save_figure(fig, PROTEOMIC_OUTPUT_DIR / f"distribution_{feature}.png")

    logger.info("Plotting stratified proteomic distributions...")
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
                PROTEOMIC_OUTPUT_DIR
                / f"distribution_{feature}_stratified_by_{target_var}.png",
            )

    logger.info("Generating Q-Q plots for proteomic data...")
    for feature in proteomic_features:
        qq_data = compute_qq(proteomic_data, feature)
        fig = plot_qq(qq_data)
        if fig is not None:
            save_figure(fig, PROTEOMIC_OUTPUT_DIR / f"distribution_{feature}_QQ.png")


if __name__ == "__main__":
    main()
