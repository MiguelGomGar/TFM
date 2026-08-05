"""Tests for the dataframe column selectors."""

import pandas as pd

from src.config import GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE
from src.utils.dataframe_utils import (
    get_categorical_columns,
    get_numeric_columns,
    get_proteomic_features,
)


def make_frame() -> pd.DataFrame:
    """Build a frame mixing proteins, the target and the stratifying variables."""
    return pd.DataFrame(
        {
            "PROT_A": [1.0, 2.0, 3.0],
            "PROT_B": [4, 5, 6],
            "sex": pd.Categorical(["male", "female", "male"]),
            "smoker": ["yes", "no", "yes"],
            "flag": [True, False, True],
            TARGET_VARIABLE: [0, 1, 0],
            GROUP_ALLOCATION_VARIABLE: ["control", "intervention", "control"],
        }
    )


def test_get_numeric_columns() -> None:
    """Numeric selection covers both floats and ints, including the target."""
    assert get_numeric_columns(make_frame()) == ["PROT_A", "PROT_B", TARGET_VARIABLE]


def test_get_categorical_columns() -> None:
    """Category, object and bool columns all count as categorical."""
    assert get_categorical_columns(make_frame()) == [
        "sex",
        "smoker",
        "flag",
        GROUP_ALLOCATION_VARIABLE,
    ]


def test_get_proteomic_features_excludes_target_and_strata() -> None:
    """Only the protein abundances may reach the model as predictors."""
    assert get_proteomic_features(make_frame()) == ["PROT_A", "PROT_B"]


def test_get_proteomic_features_preserves_column_order() -> None:
    """Column order drives the saved feature order, so it must be stable."""
    frame = make_frame()[["PROT_B", TARGET_VARIABLE, "PROT_A"]]
    assert get_proteomic_features(frame) == ["PROT_B", "PROT_A"]


def test_selectors_on_an_empty_frame() -> None:
    """An empty frame yields empty selections rather than raising."""
    empty = pd.DataFrame()
    assert get_numeric_columns(empty) == []
    assert get_categorical_columns(empty) == []
    assert get_proteomic_features(empty) == []
