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


def compute_missingness_heatmap_data(df: pd.DataFrame) -> pd.DataFrame | None:
    """Prepare missingness boolean matrix for heatmap visualization."""

    if df.empty:
        print("The dataframe has no rows. Returning None.")
        return None

    return df.isna()


def compute_stratified_missingness_data(
    df: pd.DataFrame, stratify_by: str = "AF_recurrence"
) -> pd.DataFrame | None:
    """Calculate missingness per feature stratified by a target categorical column."""

    if df.empty:
        print("The dataframe has no rows. Returning None.")
        return None

    if stratify_by not in df.columns:
        print(f"Target column '{stratify_by}' not found in dataframe. Returning None.")
        return None

    feature_cols = [col for col in df.columns if col != stratify_by]

    missing_by_group = (
        df.groupby(stratify_by)[feature_cols].apply(lambda g: g.isna().sum()).T
    )

    missing_by_group["_total_missing"] = missing_by_group.sum(axis=1)
    missing_by_group = missing_by_group[missing_by_group["_total_missing"] > 0].copy()

    if missing_by_group.empty:
        print("No missing values found in the dataframe features. Returning None.")
        return None

    missing_by_group = missing_by_group.sort_values("_total_missing", ascending=True)
    missing_by_group = missing_by_group.drop(columns=["_total_missing"])

    return missing_by_group
