# %% Configuration
print("Set paths and load modules...")
# Set paths
from src.utils.paths import CLINICAL_EDA_DIR, INTERMEDIATE_DATA_DIR

INPUT_FILE = INTERMEDIATE_DATA_DIR / "clinical_data.parquet"
OUTPUT_DIR = CLINICAL_EDA_DIR

# Load modules
import matplotlib.pyplot as plt
import pandas as pd
from src.data.collinearity_analysis import plot_corr_matrix


# %% Main
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    print(f"Loading data from {INPUT_FILE}...")
    df = pd.read_parquet(INPUT_FILE)

    print("Dropping identifiers and features with high missing rates...")
    identifiers = ["code"]
    na_rates = df.isna().mean()
    high_missing_rate_features = na_rates[na_rates > 0.25].index.tolist()
    scores = [column for column in df.columns if column.startswith("score")]
    df = pd.read_parquet(INPUT_FILE).drop(
        columns=identifiers + high_missing_rate_features + scores, errors="ignore"
    )

    print("Plotting correlation matrix for numeric features...")
    num_cor_matrix = plot_corr_matrix(df)
    num_cor_matrix.savefig(
        OUTPUT_DIR / "correlation_matrix_num.png", dpi=300, bbox_inches="tight"
    )
    plt.close(num_cor_matrix)

    print("Plotting correlation matrix for categorical features...")
    cat_cor_matrix = plot_corr_matrix(df, dtype="Categorical", axis_ticks_size=10)
    cat_cor_matrix.savefig(
        OUTPUT_DIR / "correlation_matrix_cat.png", dpi=300, bbox_inches="tight"
    )
    plt.close(cat_cor_matrix)


if __name__ == "__main__":
    main()
