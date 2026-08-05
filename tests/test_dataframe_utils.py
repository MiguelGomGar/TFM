"""Tests for the dataframe column selectors."""

import warnings

import pandas as pd

from src.config import GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE
from src.utils.dataframe_utils import (
    get_categorical_columns,
    get_numeric_columns,
    get_proteomic_features,
    iter_stratified_pairs,
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


def test_string_columns_count_as_categorical_without_warning() -> None:
    """Pin the dtype selection against the pandas 3 -> 4 string migration.

    pandas 3 still folds 'str' into 'object' for select_dtypes but warns that
    it will stop; under pandas 4 an implicit selection would silently drop
    every string column, quietly shrinking Table 1 and the correlation matrix.
    """
    frame = pd.DataFrame(
        {
            "text": pd.Series(["a", "b"], dtype="str"),
            "legacy": pd.Series(["a", "b"], dtype="object"),
            "flag": [True, False],
            "grade": pd.Categorical(["low", "high"]),
            "value": [1.0, 2.0],
        }
    )

    with warnings.catch_warnings():
        warnings.simplefilter("error")
        categorical = get_categorical_columns(frame)

    assert categorical == ["text", "legacy", "flag", "grade"]
    assert get_numeric_columns(frame) == ["value"]


def test_iter_stratified_pairs_skips_self_stratification() -> None:
    """A variable stratified by itself gives a degenerate one-level table."""
    pairs = list(
        iter_stratified_pairs(
            ["age", TARGET_VARIABLE, "BMI"],
            [GROUP_ALLOCATION_VARIABLE, TARGET_VARIABLE],
        )
    )

    assert (TARGET_VARIABLE, TARGET_VARIABLE) not in pairs
    assert (TARGET_VARIABLE, GROUP_ALLOCATION_VARIABLE) in pairs


def test_iter_stratified_pairs_groups_by_variable() -> None:
    """Pairs come out grouped by stratifying variable, in the given order.

    Both the computing pipeline and its plotting mirror derive filenames from
    this, so the enumeration has to be identical on both sides.
    """
    pairs = list(iter_stratified_pairs(["age", "BMI"], ["group", "outcome"]))

    assert pairs == [
        ("age", "group"),
        ("BMI", "group"),
        ("age", "outcome"),
        ("BMI", "outcome"),
    ]


def test_selectors_on_an_empty_frame() -> None:
    """An empty frame yields empty selections rather than raising."""
    empty = pd.DataFrame()
    assert get_numeric_columns(empty) == []
    assert get_categorical_columns(empty) == []
    assert get_proteomic_features(empty) == []
