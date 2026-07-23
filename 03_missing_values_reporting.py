# %% Setup
print("Setting up paths and loading modules...")

# Set paths
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent
INPUT_FILE = PROJECT_ROOT / "data" / "intermediate" / "clinical_data.parquet"
OUTPUT_DIR = PROJECT_ROOT / "results" / "eda" / "clinical_features"

# Load modules
import matplotlib.pyplot as plt
import pandas as pd
from src.utils.data_wrangling.missing_values_analysis import (
    plot_column_missingness,
    plot_row_missingness,
)


# %% Main
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    print(f"Loading data from {INPUT_FILE}...")
    df = pd.read_parquet(INPUT_FILE)

    print("Plotting missing values per feature...")
    col_na_plot = plot_column_missingness(df)
    col_na_plot.savefig(
        OUTPUT_DIR / "missing_values_per_feature.png",
        bbox_inches="tight",
        dpi=300,
    )

    print("Plotting missing values per row...")

    # Before dropping columns with high missingness
    row_na_plot = plot_row_missingness(df)
    row_na_plot.savefig(
        OUTPUT_DIR / "missing_values_per_record_before.png",
        bbox_inches="tight",
        dpi=300,
    )

    # Identify columns with high missingness dinamically
    na_rates = df.isna().mean()
    drop_cols = na_rates[na_rates > 0.25].index.tolist()

    # After dropping columns with high missingness
    row_na_plot_2 = plot_row_missingness(df.drop(columns=drop_cols))
    row_na_plot_2.savefig(
        OUTPUT_DIR / "missing_values_per_record_after.png",
        bbox_inches="tight",
        dpi=300,
    )


if __name__ == "__main__":
    main()
