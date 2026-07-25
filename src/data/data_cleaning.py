"""Data cleaning helpers for clinical data."""

import pandas as pd

from src.config import TARGET_ENCODING


def drop_columns(df: pd.DataFrame, columns: str | list) -> pd.DataFrame:
    """Drop specified columns from the dataframe."""
    return df.drop(columns=columns, errors="ignore")


def drop_columns_by_prefix(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
    """Drop columns with a specific prefix from the dataframe."""
    columns_to_drop = [col for col in df.columns if col.startswith(prefix)]
    return df.drop(columns=columns_to_drop, errors="ignore")


def _select_high_missingness_columns(
    dataframe: pd.DataFrame, threshold: float
) -> pd.DataFrame:
    """Select columns whose missing-value rate exceeds the given threshold."""

    missing_rates = dataframe.isna().mean()
    return missing_rates[missing_rates > threshold].index.tolist()


def drop_high_missingness_columns(
    dataframe: pd.DataFrame, threshold: float
) -> pd.DataFrame:
    """Drop columns whose missing-value rate exceeds the given threshold."""

    drop_columns = _select_high_missingness_columns(dataframe, threshold)
    return dataframe.drop(columns=drop_columns)


def drop_high_missingness_rows(
    dataframe: pd.DataFrame, threshold: float
) -> pd.DataFrame:
    """Drop rows whose missing-value rate exceeds the given threshold."""

    missing_rates = dataframe.isna().mean(axis=1)
    keep_rows = missing_rates[missing_rates <= threshold].index
    return dataframe.loc[keep_rows]


def inner_join(df1: pd.DataFrame, df2: pd.DataFrame, on: str | list) -> pd.DataFrame:
    """Merge two dataframes on specified columns."""
    return df1.merge(df2, on=on, how="inner")


def remove_prefix_from_columns(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
    """Remove a specific prefix from column names in the dataframe."""
    df = df.copy()
    df.columns = [col.removeprefix(prefix) for col in df.columns]
    return df


def encode_target_variable(df: pd.DataFrame, target_column: str) -> pd.Series:
    """Encode the target variable as binary (0/1) based on specific string values."""
    encoded = df[target_column].astype(str).str.strip().str.lower().map(TARGET_ENCODING)
    if encoded.isna().any():
        unexpected_values = df.loc[encoded.isna(), target_column].dropna().unique()
        raise ValueError(f"Unexpected {target_column} values: {unexpected_values}")
    return encoded
