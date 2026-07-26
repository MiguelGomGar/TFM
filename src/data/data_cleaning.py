"""Data cleaning helpers for clinical data."""

import pandas as pd

from src.config import TARGET_ENCODING


def drop_columns(df: pd.DataFrame, columns: str | list) -> pd.DataFrame:
    """Drop specified columns from the dataframe.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.
    columns : str or list of str
        Column name(s) to drop. Non-existent columns are silently ignored.

    Returns
    -------
    pd.DataFrame
        DataFrame with specified columns removed.
    """
    return df.drop(columns=columns, errors="ignore")


def drop_columns_by_prefix(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
    """Drop all columns with a specific prefix from the dataframe.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.
    prefix : str
        Column name prefix to match. All columns starting with this string
        will be removed.

    Returns
    -------
    pd.DataFrame
        DataFrame with matching columns removed.
    """
    columns_to_drop = [col for col in df.columns if col.startswith(prefix)]
    return df.drop(columns=columns_to_drop, errors="ignore")


def _select_high_missingness_columns(
    dataframe: pd.DataFrame, threshold: float
) -> list[str]:
    """Select column names where missing-value rate exceeds the threshold.

    Parameters
    ----------
    dataframe : pd.DataFrame
        Input dataframe.
    threshold : float
        Missing-rate threshold (0-1). Columns with missing rate > threshold
        are selected.

    Returns
    -------
    list of str
        Column names with missing rate exceeding the threshold.
    """
    missing_rates = dataframe.isna().mean()
    return missing_rates[missing_rates > threshold].index.tolist()


def drop_high_missingness_columns(
    dataframe: pd.DataFrame, threshold: float
) -> pd.DataFrame:
    """Drop columns with missing-value rate exceeding the threshold.

    Parameters
    ----------
    dataframe : pd.DataFrame
        Input dataframe.
    threshold : float
        Missing-rate threshold (0-1). Columns with missing rate > threshold
        are dropped.

    Returns
    -------
    pd.DataFrame
        DataFrame with high-missingness columns removed.
    """
    drop_columns = _select_high_missingness_columns(dataframe, threshold)
    return dataframe.drop(columns=drop_columns)


def drop_high_missingness_rows(
    dataframe: pd.DataFrame, threshold: float
) -> pd.DataFrame:
    """Drop rows with missing-value rate exceeding the threshold.

    Parameters
    ----------
    dataframe : pd.DataFrame
        Input dataframe.
    threshold : float
        Missing-rate threshold (0-1). Rows with missing rate > threshold
        are dropped.

    Returns
    -------
    pd.DataFrame
        DataFrame with high-missingness rows removed.
    """
    missing_rates = dataframe.isna().mean(axis=1)
    keep_rows = missing_rates[missing_rates <= threshold].index
    return dataframe.loc[keep_rows]


def inner_join(df1: pd.DataFrame, df2: pd.DataFrame, on: str | list) -> pd.DataFrame:
    """Perform an inner join (merge) of two dataframes on specified column(s).

    Parameters
    ----------
    df1 : pd.DataFrame
        Left dataframe.
    df2 : pd.DataFrame
        Right dataframe.
    on : str or list of str
        Column name(s) to join on. Must exist in both dataframes.

    Returns
    -------
    pd.DataFrame
        Result of the inner join, containing only rows where keys match in both
        input dataframes.
    """
    return df1.merge(df2, on=on, how="inner")


def remove_prefix_from_columns(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
    """Remove a prefix from all column names.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.
    prefix : str
        Prefix to remove from the start of each column name.

    Returns
    -------
    pd.DataFrame
        Copy of the dataframe with the prefix removed from all column names.
    """
    df = df.copy()
    df.columns = [col.removeprefix(prefix) for col in df.columns]
    return df


def encode_target_variable(df: pd.DataFrame, target_column: str) -> pd.Series:
    """Encode the target variable to binary (0/1) using TARGET_ENCODING mapping.

    Parameters
    ----------
    df : pd.DataFrame
        Dataframe containing the target column.
    target_column : str
        Name of the column to encode.

    Returns
    -------
    pd.Series
        Encoded target variable as int (0 or 1).

    Raises
    ------
    ValueError
        If unexpected values (not in TARGET_ENCODING) are encountered.
    """
    encoded = df[target_column].astype(str).str.strip().str.lower().map(TARGET_ENCODING)
    if encoded.isna().any():
        unexpected_values = df.loc[encoded.isna(), target_column].dropna().unique()
        raise ValueError(f"Unexpected {target_column} values: {unexpected_values}")
    return encoded
