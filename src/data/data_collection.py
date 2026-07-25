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
    """Keep only the raw features required for the clinical pipeline."""

    return dataframe[SELECTED_FEATURES].copy()


def rename_clinical_features(dataframe: pd.DataFrame) -> pd.DataFrame:
    """Rename raw clinical variables to their analysis names."""

    return dataframe.rename(columns=NEW_FEATURES_NAMES)


def recode_clinical_categories(dataframe: pd.DataFrame) -> pd.DataFrame:
    """Map categorical variables to harmonized labels and orders."""

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
    """Cast numeric columns to float64 numeric dtype."""

    numeric_columns = dataframe.select_dtypes(include=["number"]).columns
    dataframe[numeric_columns] = dataframe[numeric_columns].astype("float64")
    return dataframe
