"""Tests for the shared artifact names.

These names are the contract between each calculation pipeline and its plotting
mirror: if one side renames a file, the other silently stops finding it. The
tests below pin the contract down.
"""

import pytest

from src.utils import filenames
from src.utils.filenames import (
    CURVES_FILE,
    DISTRIBUTION_FILE,
    MODELS_METRICS_FILE,
    STRATIFIED_DISTRIBUTION_FILE,
    metric_slug,
)


@pytest.mark.parametrize(
    ("metric", "expected"),
    [
        ("ROC-AUC", "roc_auc"),
        ("PR-AUC", "pr_auc"),
        ("Accuracy", "accuracy"),
        ("F1", "f1"),
    ],
)
def test_metric_slug(metric: str, expected: str) -> None:
    """Metric labels become the lowercase, underscored filename fragment."""
    assert metric_slug(metric) == expected


def test_metric_slug_is_idempotent() -> None:
    """Slugging an already-slugged label must not mangle it."""
    assert metric_slug(metric_slug("ROC-AUC")) == "roc_auc"


def test_templates_format_into_the_documented_names() -> None:
    """The placeholders must expand to the names actually written to disk."""
    assert DISTRIBUTION_FILE.format(feature="age") == "distribution_age.csv"
    assert CURVES_FILE.format(curve="roc") == "curves_roc.csv"
    assert (
        STRATIFIED_DISTRIBUTION_FILE.format(feature="BMI", group="AF_recurrence")
        == "distribution_BMI_stratified_by_AF_recurrence.csv"
    )


def test_non_template_names_have_no_placeholders() -> None:
    """A stray placeholder would be written to disk verbatim."""
    assert "{" not in MODELS_METRICS_FILE


def test_every_exported_name_has_a_file_extension() -> None:
    """Every constant names an artifact, so each must carry a suffix."""
    exported = {
        name: value
        for name, value in vars(filenames).items()
        if name.isupper() and isinstance(value, str)
    }
    assert exported
    for name, value in exported.items():
        assert value.endswith((".csv", ".png", ".xlsx", ".joblib")), f"{name}={value}"


# INTERNAL_VALIDATION is deliberately asymmetric: the tables are written one per
# model (internal_validation_{model}.csv) while the figures are drawn one per
# metric (internal_validation_{metric}.png), each combining every model.
ASYMMETRIC_PAIRS = {"INTERNAL_VALIDATION_FIGURE"}


def test_figures_and_tables_pair_up() -> None:
    """A _FIGURE constant must mirror its _FILE, differing only in extension.

    Keeping the stems identical is what lets a plotting pipeline locate the
    table its figure is drawn from without a second lookup table.
    """
    exported = {
        name: value
        for name, value in vars(filenames).items()
        if name.isupper() and isinstance(value, str)
    }
    checked = 0
    for name, figure in exported.items():
        if not name.endswith("_FIGURE") or name in ASYMMETRIC_PAIRS:
            continue
        table_name = name.replace("_FIGURE", "_FILE")
        if table_name not in exported:
            continue
        assert figure.endswith(".png"), name
        assert figure[: -len(".png")] == exported[table_name][: -len(".csv")], name
        checked += 1
    assert checked >= 10, "expected the pairing rule to cover most artifacts"
