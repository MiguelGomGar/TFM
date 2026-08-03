"""Data preparation for the clinical variables review."""

from __future__ import annotations

import pandas as pd

from src.config import (
    VARIABLES_REVIEW_ORIGINAL_LEVELS,
    VARIABLES_REVIEW_SORT_LEVELS,
)


def prepare_clinical_variables_review_data(df: pd.DataFrame) -> pd.DataFrame:
    """Prepare clinical variables review dataframe for plotting.

    Converts the Predimar review status to categorical with proper sort and
    display order, and sorts rows for visualization.

    Parameters
    ----------
    df : pd.DataFrame
        Clinical variables review dataframe with columns ['Variable', 'Predimar', 'Scores'].

    Returns
    -------
    pd.DataFrame
        Prepared dataframe with categorical columns ordered for plotting.
    """
    prepared_df = df.copy()
    prepared_df["Predimar_sort"] = pd.Categorical(
        prepared_df["Predimar"].astype(str),
        categories=VARIABLES_REVIEW_SORT_LEVELS,
        ordered=True,
    )
    prepared_df["Predimar"] = pd.Categorical(
        prepared_df["Predimar"].astype(str),
        categories=VARIABLES_REVIEW_ORIGINAL_LEVELS,
        ordered=True,
    )
    prepared_df = prepared_df.sort_values(["Predimar_sort", "Scores"]).reset_index(
        drop=True
    )
    prepared_df["Variable"] = pd.Categorical(
        prepared_df["Variable"].astype(str),
        categories=prepared_df["Variable"].tolist(),
        ordered=True,
    )
    return prepared_df
