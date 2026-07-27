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


def mask_out_of_range_values(
    dataframe: pd.DataFrame, ranges: dict[str, tuple[float, float]]
) -> tuple[pd.DataFrame, dict[str, int]]:
    """Set physiologically impossible values to missing.

    Values falling outside the plausible range of a feature are recording
    errors rather than genuine variability. They are replaced by NaN so that
    the downstream imputation step handles them like any other missing value,
    instead of being clipped to the bound and treated as observed data.

    Parameters
    ----------
    dataframe : pd.DataFrame
        Input dataframe.
    ranges : dict of str to tuple of float
        Mapping of column name to its inclusive (minimum, maximum) bounds.
        Columns absent from the dataframe are silently ignored.

    Returns
    -------
    pd.DataFrame
        Copy of the dataframe with out-of-range values replaced by NaN.
    dict of str to int
        Number of values masked per affected column.
    """
    result = dataframe.copy()
    masked_counts = {}

    for column, (minimum, maximum) in ranges.items():
        if column not in result.columns:
            continue
        out_of_range = result[column].notna() & (
            (result[column] < minimum) | (result[column] > maximum)
        )
        count = int(out_of_range.sum())
        if count:
            result.loc[out_of_range, column] = pd.NA
            masked_counts[column] = count

    return result, masked_counts


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
