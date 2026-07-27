"""Preprocessing factories used by the model pipelines."""

import numpy as np
import pandas as pd
from sklearn.experimental import enable_iterative_imputer
from sklearn.compose import ColumnTransformer
from sklearn.impute import IterativeImputer
from sklearn.impute import SimpleImputer
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OrdinalEncoder, StandardScaler

from src.config import TARGET_ENCODING


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


def get_full_preprocessor(X: pd.DataFrame, seed: int) -> ColumnTransformer:
    """Create a preprocessor for distance-based models (SVM, etc.).

    Builds a ColumnTransformer with iterative imputation and standardization
    for numeric features, and ordinal encoding + scaling for categorical
    features. Essential for distance-sensitive algorithms like SVM where
    feature magnitude matters.

    Parameters
    ----------
    X : pd.DataFrame
        Input dataframe with numeric and/or categorical columns. Categorical
        columns must be of dtype 'category' with ordered levels.
    seed : int
        Random seed for reproducibility of iterative imputation.

    Returns
    -------
    sklearn.compose.ColumnTransformer
        Fitted preprocessor that can be used as the first step in a pipeline.
        Numeric pipeline: IterativeImputer(median) -> StandardScaler.
        Categorical pipelines: SimpleImputer(most_frequent) -> OrdinalEncoder
        (preserving pre-defined category order) -> StandardScaler.
    """
    # 1. Dynamically isolate numerical and categorical features
    numeric_features = X.select_dtypes(include=[np.number]).columns.tolist()
    categorical_features = X.select_dtypes(include=["category"]).columns.tolist()

    # 2. Define the standardized pipeline for continuous variables
    numeric_transformer = Pipeline(
        steps=[
            (
                "imputer",
                IterativeImputer(
                    max_iter=10, initial_strategy="median", random_state=seed
                ),
            ),
            ("scaler", StandardScaler()),
        ]
    )

    # 3. Harvest embedded hierarchies and build customized OrdinalEncoders per
    # feature
    transformers_list = [("num", numeric_transformer, numeric_features)]

    # Loop over every single factor (both binary and multi-class ordinals)
    for col in categorical_features:
        # Pull the pre-sorted levels array directly from the Parquet metadata
        # slot
        extracted_order = list(X[col].dtype.categories)

        categorical_pipeline = Pipeline(
            steps=[
                ("imputer", SimpleImputer(strategy="most_frequent")),
                (
                    "encoder",
                    OrdinalEncoder(
                        categories=[extracted_order],
                        handle_unknown="use_encoded_value",
                        unknown_value=-1,
                    ),
                ),
                ("scaler", StandardScaler()),
            ]
        )

        # Append as a clean, localized and independent processing lane
        transformers_list.append((f"ord_{col}", categorical_pipeline, [col]))

    # 4. Assemble the final unified two-lane preprocessor structural framework
    preprocessor = ColumnTransformer(transformers=transformers_list, remainder="drop")

    return preprocessor


def get_trees_preprocessor(X: pd.DataFrame, seed: int) -> ColumnTransformer:
    """Create a preprocessor for tree-based ensemble models (Random Forest, etc.).

    Builds a ColumnTransformer with iterative imputation for numeric features
    and ordinal encoding for categorical features. Omits scaling since tree
    models are invariant to monotonic transformations.

    Parameters
    ----------
    X : pd.DataFrame
        Input dataframe with numeric and/or categorical columns. Categorical
        columns must be of dtype 'category' with ordered levels.
    seed : int
        Random seed for reproducibility of iterative imputation.

    Returns
    -------
    sklearn.compose.ColumnTransformer
        Fitted preprocessor for tree-based pipelines. Numeric pipeline:
        IterativeImputer(median) only (no scaling). Categorical pipelines:
        SimpleImputer(most_frequent) -> OrdinalEncoder (preserving category order).
    """
    # 1. Dynamically isolate numerical and categorical features
    numeric_features = X.select_dtypes(include=[np.number]).columns.tolist()
    categorical_features = X.select_dtypes(include=["category"]).columns.tolist()

    # 2. Define the standardized pipeline for continuous variables
    numeric_transformer = Pipeline(
        steps=[
            (
                "imputer",
                IterativeImputer(
                    max_iter=10, initial_strategy="median", random_state=seed
                ),
            )
        ]
    )

    # 3. Harvest embedded hierarchies and build customized OrdinalEncoders per
    # feature
    transformers_list = [("num", numeric_transformer, numeric_features)]

    # Loop over every single factor (both binary and multi-class ordinals)
    for col in categorical_features:
        # Pull the pre-sorted levels array directly from the Parquet metadata
        # slot
        extracted_order = list(X[col].dtype.categories)

        categorical_pipeline = Pipeline(
            steps=[
                ("imputer", SimpleImputer(strategy="most_frequent")),
                (
                    "encoder",
                    OrdinalEncoder(
                        categories=[extracted_order],
                        handle_unknown="use_encoded_value",
                        unknown_value=-1,
                    ),
                ),
            ]
        )

        # Append as a clean, localized and independent processing lane
        transformers_list.append((f"ord_{col}", categorical_pipeline, [col]))

    # 4. Assemble the final unified two-lane preprocessor structural framework
    preprocessor = ColumnTransformer(transformers=transformers_list, remainder="drop")

    return preprocessor
