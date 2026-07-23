# %% Configuration
print("Set paths and load modules...")
# Set paths
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent
INPUT_FILE = PROJECT_ROOT / "data" / "intermediate" / "clinical_data.parquet"
OUTPUT_DIR = PROJECT_ROOT / "results" / "eda" / "clinical_features"

# Load modules
import matplotlib.pyplot as plt
import pandas as pd
from src.utils.data_wrangling.collinearity_analysis import plot_vif


# %% Main
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    print(f"Loading data from {INPUT_FILE}...")
    df = pd.read_parquet(INPUT_FILE)

    print(
        "Dropping identifiers, features with high missing rates risk scores and highly correlated features..."
    )
    identifiers = ["code"]
    na_rates = df.isna().mean()
    high_missing_rate_features = na_rates[na_rates > 0.25].index.tolist()
    scores = [column for column in df.columns if column.startswith("score")]
    highly_correlated_features = ["heart_failure"]
    df = pd.read_parquet(INPUT_FILE).drop(
        columns=identifiers
        + high_missing_rate_features
        + ["AF_recurrence"]
        + scores
        + highly_correlated_features,
        errors="ignore",
    )

    print("Plotting VIF diagnostics...")
    vif_plot = plot_vif(df)
    vif_plot.savefig(OUTPUT_DIR / "vif_diagnostics.png", dpi=300, bbox_inches="tight")
    plt.close(vif_plot)


if __name__ == "__main__":
    main()
