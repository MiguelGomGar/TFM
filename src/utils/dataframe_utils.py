"""Dataframe helper functions."""

import pandas as pd
from src.config import TARGET_VARIABLE, STRATIFY_VARIABLES


def get_numeric_columns(df: pd.DataFrame) -> list[str]:
    """Get the numeric columns of a dataframe.

    Args:
        df (pd.DataFrame): Input dataframe.

    Returns:
        list[str]: List of numeric columns.
    """
    return df.select_dtypes(include=["number"]).columns.tolist()


def get_categorical_columns(df: pd.DataFrame) -> list[str]:
    """Get the categorical columns of a dataframe.

    Args:
        df (pd.DataFrame): Input dataframe.

    Returns:
        list[str]: List of categorical columns.
    """
    return df.select_dtypes(include=["category", "object", "bool"]).columns.tolist()


def get_proteomic_features(df: pd.DataFrame) -> list[str]:
    """Get the proteomic features of a dataframe."""
    return [
        col
        for col in df.columns
        if col in get_numeric_columns(df)
        and col != TARGET_VARIABLE
        and col not in STRATIFY_VARIABLES
    ]
