"""Dataframe helper functions."""

import pandas as pd
from src.config import NEW_FEATURES_NAMES, TARGET_VARIABLE, STRATIFY_VARIABLES


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


def split_feature_blocks(feature_names: list[str]) -> dict[str, list[str]]:
    """Split predictor names into the clinical and proteomic blocks.

    Classifies each name against the canonical set of clinical variable names
    (the English names produced by the data collection phase, NEW_FEATURES_NAMES);
    anything not in that set is a protein.

    Unlike get_proteomic_features, this works on a multimodal feature set:
    that function classifies by dtype, so on a multimodal frame it would label
    numeric clinical variables such as age or BMI as proteins.

    Parameters
    ----------
    feature_names : list of str
        Predictor names, e.g. as returned by the fitted preprocessor.

    Returns
    -------
    dict
        Keys 'Clinical' and 'Proteomic', each mapping to the subset of
        feature_names belonging to that block, in the input order. Either
        list may be empty for a single-modality feature set.
    """
    clinical_vocabulary = set(NEW_FEATURES_NAMES.values())
    return {
        "Clinical": [name for name in feature_names if name in clinical_vocabulary],
        "Proteomic": [name for name in feature_names if name not in clinical_vocabulary],
    }
