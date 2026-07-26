"""Dataframe helper functions."""

import pandas as pd
from src.config import TARGET_VARIABLE, STRATIFY_VARIABLES


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
    return df.select_dtypes(include=["category", "object", "bool"]).columns.tolist()


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
