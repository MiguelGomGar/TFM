"""Missing value data preparation helpers."""

import pandas as pd


def compute_row_missingness_data(df):
    """Calculate row-wise missingness summary for plotting."""

    if df.empty:
        print("The dataframe has no rows. Returning None.")
        return None

    row_na_count = df.isna().sum(axis=1)
    na_summary = row_na_count.value_counts().reset_index()
    na_summary.columns = ["row_na_count", "n_records"]
    na_summary = na_summary.sort_values("row_na_count")

    total_records = na_summary["n_records"].sum()
    na_summary["pct_label"] = (na_summary["n_records"] / total_records * 100).map(
        lambda x: f"{x:.1f}%"
    )

    return na_summary


def compute_column_missingness_data(df):
    """Calculate column-wise missingness summary for plotting."""

    missing_rate = df.isna().sum() / len(df)
    na_summary = pd.DataFrame(
        {"feature": missing_rate.index, "missing_rate": missing_rate.values}
    )

    na_summary = na_summary[na_summary["missing_rate"] > 0].sort_values("missing_rate")

    if na_summary.empty:
        print("No missing values found in the dataframe. Returning None.")
        return None

    return na_summary
