"""Tests for the predictor selection shared by the modelling phases."""

import pandas as pd

from src.models.feature_selection import restrict_to_available


def make_frame() -> pd.DataFrame:
    """Build a frame missing one of the features an earlier phase kept."""
    return pd.DataFrame({"age": [1.0], "PROT_A": [2.0], "BMI": [3.0]})


def test_restrict_to_available_drops_absent_features() -> None:
    """Indexing with a column the frame lacks would raise, so it must be dropped."""
    kept = ["age", "PROT_MISSING", "BMI"]
    assert restrict_to_available(kept, make_frame()) == ["age", "BMI"]


def test_restrict_to_available_preserves_the_given_order() -> None:
    """The order drives the design matrix column order, so it must not be sorted."""
    kept = ["BMI", "PROT_A", "age"]
    assert restrict_to_available(kept, make_frame()) == ["BMI", "PROT_A", "age"]


def test_restrict_to_available_keeps_everything_when_all_present() -> None:
    """The common case must be a no-op."""
    kept = ["age", "PROT_A"]
    assert restrict_to_available(kept, make_frame()) == kept


def test_restrict_to_available_on_no_overlap() -> None:
    """A disjoint selection yields an empty list rather than raising."""
    assert restrict_to_available(["nothing", "here"], make_frame()) == []
