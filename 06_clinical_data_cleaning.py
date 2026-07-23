# %% Configuration
print("Set paths and load modules...")

# Set paths
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent
CLINICAL_INPUT_FILE = PROJECT_ROOT / "data" / "intermediate" / "clinical_data.parquet"
PROTEOMIC_INPUT_FILE = PROJECT_ROOT / "data" / "raw" / "olink_baseline_wide.csv"
OUTPUT_DIR = PROJECT_ROOT / "data" / "clean"

# Load modules
import pandas as pd
from src.utils.data_wrangling.feature_engineering import compute_risk_scores


# %% Main
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    print(f"Loading clinical data from {CLINICAL_INPUT_FILE}...")
    clinical_data = pd.read_parquet(CLINICAL_INPUT_FILE)

    print("Computing risk scores...")
    risk_scores_data = compute_risk_scores(clinical_data)
    risk_scores_columns = [
        col for col in risk_scores_data.columns if col.startswith("score")
    ]
    risk_scores_data = risk_scores_data[["AF_recurrence"] + risk_scores_columns]

    print(f"Saving risk scores data to {OUTPUT_DIR / 'risk_scores_data.parquet'}...")
    risk_scores_data.to_parquet(OUTPUT_DIR / "risk_scores_data.parquet", index=False)

    print("Identifying features with high missing rates dinamically...")
    na_rates = clinical_data.isna().mean()
    drop_cols_na = na_rates[na_rates > 0.25].index.tolist()

    print("Identifying highly correlated features manually...")
    drop_cols_collinearity = ["heart_failure"]

    print(f"Saving clinical data at {OUTPUT_DIR}...")

    clinical_data.drop(
        # Drop identifiers, risk scores, columns with high missing rates and highly correlated features
        columns=["code", "score_chad2ds2_vasc"] + drop_cols_na + drop_cols_collinearity,
        errors="ignore",
    ).to_parquet(OUTPUT_DIR / "clinical_data.parquet", index=False)

    print(f"Loading proteomic data from {PROTEOMIC_INPUT_FILE}...")
    proteomic_data = pd.read_csv(PROTEOMIC_INPUT_FILE)

    print("Joining proteomic data with clinical data on 'code'...")

    # Insert the AF recurrence and intervention columns into the proteomic dataset
    proteomic_data = proteomic_data.merge(
        clinical_data[["code", "AF_recurrence", "intervention"]], on="code", how="inner"
    )

    print(
        f"Saving cleaned proteomic data to {OUTPUT_DIR / 'proteomic_data.parquet'} without the identifiers..."
    )
    proteomic_data.drop(columns=["code"], errors="ignore").to_parquet(
        OUTPUT_DIR / "proteomic_data.parquet", index=False
    )


if __name__ == "__main__":
    main()
