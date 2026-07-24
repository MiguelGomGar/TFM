# %% Configuration
print("Set paths and load modules...")

# Set paths
from src.utils.paths import (
    CLEAN_DATA_DIR,
    CLINICAL_EDA_DIR,
    PROTEOMIC_EDA_DIR,
)

CLINICAL_FILE = CLEAN_DATA_DIR / "clinical_data.parquet"
PROTEOMIC_FILE = CLEAN_DATA_DIR / "proteomic_data.parquet"
CLINICAL_OUTPUT_DIR = CLINICAL_EDA_DIR
PROTEOMIC_OUTPUT_DIR = PROTEOMIC_EDA_DIR

# Load modules
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from src.data.statistical_analysis import (
    create_table1,
    plot_categorical_distribution,
    plot_numeric_distribution,
    plot_qq,
    plot_stratified_categorical_distribution,
    plot_stratified_numeric_distribution,
)


def main() -> None:
    CLINICAL_OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    PROTEOMIC_OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    print(f"Loading data from {CLINICAL_FILE}...")
    df_clin = pd.read_parquet(CLINICAL_FILE)

    numeric_features = [
        col
        for col in df_clin.select_dtypes(include=["number"]).columns
        if col != "AF_recurrence" and col != "intervention"
    ]
    categorical_features = [
        col
        for col in df_clin.columns
        if col not in numeric_features
        and df_clin[col].dtype.name in ["category", "object", "bool"]
    ]

    print("Plotting numeric features' distributions...")
    for feature in numeric_features:
        fig = plot_numeric_distribution(df_clin, feature)
        fig.savefig(
            CLINICAL_OUTPUT_DIR / f"distribution_{feature}.png",
            bbox_inches="tight",
            dpi=300,
        )
        plt.close(fig)

    print("Plotting numeric features' distributions stratified by AF_recurrence...")
    for feature in numeric_features:
        fig = plot_stratified_numeric_distribution(df_clin, feature, "AF_recurrence")
        fig.savefig(
            CLINICAL_OUTPUT_DIR
            / f"distribution_{feature}_stratified_by_AF_recurrence.png",
            bbox_inches="tight",
            dpi=300,
        )
        plt.close(fig)

    print("Generating Q-Q plots for numeric features...")
    for feature in numeric_features:
        fig = plot_qq(df_clin, feature)
        if fig is not None:
            fig.savefig(
                CLINICAL_OUTPUT_DIR / f"distribution_{feature}_QQ.png",
                bbox_inches="tight",
                dpi=300,
            )
        plt.close(fig)

    print("Plotting numeric features' distributions stratified by intervention...")
    for feature in numeric_features:
        fig = plot_stratified_numeric_distribution(df_clin, feature, "intervention")
        fig.savefig(
            CLINICAL_OUTPUT_DIR
            / f"distribution_{feature}_stratified_by_intervention.png",
            bbox_inches="tight",
            dpi=300,
        )
        plt.close(fig)

    print("Plotting categorical features' distributions...")
    for feature in categorical_features:
        fig = plot_categorical_distribution(df_clin, feature)
        fig.savefig(
            CLINICAL_OUTPUT_DIR / f"distribution_{feature}.png",
            bbox_inches="tight",
            dpi=300,
        )
        plt.close(fig)

    print("Plotting categorical features' distributions stratified by AF_recurrence...")
    for feature in categorical_features:
        if feature == "AF_recurrence":
            continue
        fig = plot_stratified_categorical_distribution(
            df_clin, feature, "AF_recurrence"
        )
        if fig is not None:
            fig.savefig(
                CLINICAL_OUTPUT_DIR
                / f"distribution_{feature}_stratified_by_AF_recurrence.png",
                bbox_inches="tight",
                dpi=300,
            )
        plt.close(fig)

    print("Plotting categorical features' distributions stratified by intervention...")
    for feature in categorical_features:
        if feature == "intervention":
            continue
        fig = plot_stratified_categorical_distribution(df_clin, feature, "intervention")
        if fig is not None:
            fig.savefig(
                CLINICAL_OUTPUT_DIR
                / f"distribution_{feature}_stratified_by_intervention.png",
                bbox_inches="tight",
                dpi=300,
            )
        plt.close(fig)

    print("Creating Table 1 stratified by AF_recurrence...")
    table1 = create_table1(
        data=df_clin,
        strat_var="AF_recurrence",
        cat_vars=categorical_features + ["AF_recurrence"],
        nonnormal_vars=["AF_duration", "triglycerides"],
    )
    table1.to_csv(CLINICAL_OUTPUT_DIR / "table1.csv", index=False)

    print(f"Loading data from {PROTEOMIC_FILE}...")
    df_prot = pd.read_parquet(PROTEOMIC_FILE)

    proteomic_features = [
        col
        for col in df_prot.columns
        if col != "AF_recurrence" and col != "intervention"
    ]


if __name__ == "__main__":
    main()
