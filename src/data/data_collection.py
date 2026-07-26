"""Clinical dataset collection helpers."""

from pathlib import Path

import pandas as pd

from src.config import (
    CATEGORICAL_SPECS,
    NEW_FEATURES_NAMES,
    SELECTED_FEATURES,
)
from src.utils.io import read_dta, save_parquet


def select_clinical_features(dataframe: pd.DataFrame) -> pd.DataFrame:
    """Select predefined clinical features from the raw dataset.

    Parameters
    ----------
    dataframe : pd.DataFrame
        Raw clinical dataframe with all original columns.

    Returns
    -------
    pd.DataFrame
        Copy of the dataframe containing only the features listed in
        SELECTED_FEATURES (defined in config.py).
    """
    return dataframe[SELECTED_FEATURES].copy()


def rename_clinical_features(dataframe: pd.DataFrame) -> pd.DataFrame:
    """Rename raw clinical variables to standardized analysis names.

    Parameters
    ----------
    dataframe : pd.DataFrame
        Dataframe with raw (Spanish) clinical variable names.

    Returns
    -------
    pd.DataFrame
        DataFrame with columns renamed according to NEW_FEATURES_NAMES mapping
        (e.g., 'edad' -> 'age', 'sexo' -> 'sex').
    """
    return dataframe.rename(columns=NEW_FEATURES_NAMES)


def recode_clinical_categories(dataframe: pd.DataFrame) -> pd.DataFrame:
    """Recode and order categorical variables according to standardized mappings.

    Applies value mappings (e.g., 'si' -> 'yes') and converts columns to
    ordered categorical dtype with levels specified in CATEGORICAL_SPECS.

    Parameters
    ----------
    dataframe : pd.DataFrame
        Clinical dataframe with raw categorical values.

    Returns
    -------
    pd.DataFrame
        DataFrame with categorical columns recoded and cast to ordered
        categorical dtype.
    """
    for column, spec in CATEGORICAL_SPECS.items():
        dataframe[column] = (
            dataframe[column]
            .astype(object)
            .replace(spec["mapping"])
            .astype("category")
            .cat.reorder_categories(spec["order"], ordered=True)
        )

    return dataframe


def cast_clinical_numeric_columns(dataframe: pd.DataFrame) -> pd.DataFrame:
    """Cast all numeric columns to float64 dtype for consistency.

    Parameters
    ----------
    dataframe : pd.DataFrame
        Clinical dataframe with mixed numeric dtypes.

    Returns
    -------
    pd.DataFrame
        DataFrame with all numeric columns cast to float64.
    """
    numeric_columns = dataframe.select_dtypes(include=["number"]).columns
    dataframe[numeric_columns] = dataframe[numeric_columns].astype("float64")
    return dataframe
