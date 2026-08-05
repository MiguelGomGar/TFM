"""Dataframe helper functions."""

from collections.abc import Iterator, Sequence

import pandas as pd

from src.config import STRATIFY_VARIABLES, TARGET_VARIABLE

# Dtypes treated as categorical throughout the project. 'str' is listed
# explicitly alongside 'object': pandas 3 still folds the two together in
# select_dtypes for backward compatibility, but warns that it will stop, and
# pandas 4 would silently drop every string column from the selection.
CATEGORICAL_DTYPES = ["category", "object", "str", "bool"]


def get_numeric_columns(df: pd.DataFrame) -> list[str]:
    """Get the numeric columns from a dataframe.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.

    Returns
    -------
    list of str
        Column names with numeric dtype (int, float, etc.).
    """
    return df.select_dtypes(include=["number"]).columns.tolist()


def get_categorical_columns(df: pd.DataFrame) -> list[str]:
    """Get the categorical columns from a dataframe.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.

    Returns
    -------
    list of str
        Column names with categorical dtype (category, object, bool).
    """
    return df.select_dtypes(include=CATEGORICAL_DTYPES).columns.tolist()


def iter_stratified_pairs(
    features: Sequence[str], stratify_variables: Sequence[str] | None = None
) -> Iterator[tuple[str, str]]:
    """Yield the (feature, grouping variable) pairs worth stratifying.

    A variable stratified by itself would produce a degenerate one-level-per-bar
    table, so those pairs are skipped. Enumerating them here keeps a computing
    pipeline and its plotting mirror from drifting into disagreeing about which
    files exist.

    Parameters
    ----------
    features : sequence of str
        Feature names to stratify.
    stratify_variables : sequence of str, optional
        Grouping variables, in reporting order. Defaults to
        config.STRATIFY_VARIABLES.

    Yields
    ------
    tuple of (str, str)
        The feature and the variable it is stratified by, grouped by variable.
    """
    if stratify_variables is None:
        stratify_variables = STRATIFY_VARIABLES
    for group in stratify_variables:
        for feature in features:
            if feature == group:
                continue
            yield feature, group


def get_proteomic_features(df: pd.DataFrame) -> list[str]:
    """Get proteomic features (numeric columns excluding target and stratification vars).

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe containing proteomic and other features.

    Returns
    -------
    list of str
        Column names of numeric proteomic features, excluding the target
        variable and stratification variables.
    """
    return [
        col
        for col in df.columns
        if col in get_numeric_columns(df)
        and col != TARGET_VARIABLE
        and col not in STRATIFY_VARIABLES
    ]


