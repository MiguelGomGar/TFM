"""Tests for the threshold selection used in the sensitivity analysis."""

import numpy as np
import pytest

from src.models.best_model import (
    apply_threshold,
    build_confusion_matrix_table,
    compute_youden_threshold,
)


def test_youden_threshold_on_a_separable_score() -> None:
    """A perfectly separable score has J = 1 at the gap between the classes."""
    y_true = np.array([0, 0, 1, 1])
    y_score = np.array([0.1, 0.2, 0.8, 0.9])

    result = compute_youden_threshold(y_true, y_score)

    assert result["youden_j"] == pytest.approx(1.0)
    # Any cut-off in (0.2, 0.8] separates the classes; roc_curve reports the
    # lowest score still labeled positive.
    assert 0.2 < result["threshold"] <= 0.8


def test_youden_threshold_on_an_uninformative_score() -> None:
    """A score carrying no signal cannot beat chance: J stays at 0."""
    y_true = np.array([0, 1, 0, 1])
    y_score = np.array([0.5, 0.5, 0.5, 0.5])

    assert compute_youden_threshold(y_true, y_score)["youden_j"] == pytest.approx(0.0)


def test_apply_threshold_is_strictly_greater() -> None:
    """A score equal to the cut-off is negative, as the docstring promises.

    compute_youden_threshold documents this asymmetry explicitly, so it is
    worth pinning: a >= comparison would move one patient across the boundary.
    """
    y_score = np.array([0.3, 0.5, 0.7])
    assert apply_threshold(y_score, 0.5).tolist() == [0, 0, 1]


def test_apply_threshold_handles_negative_scores() -> None:
    """SVM decision_function values are unbounded, not probabilities."""
    y_score = np.array([-2.0, -0.5, 0.0, 1.5])
    assert apply_threshold(y_score, -0.5).tolist() == [0, 0, 1, 1]


def test_confusion_matrix_table_counts() -> None:
    """The saved table must carry the same counts a hand tally gives."""
    y_true = np.array([1, 1, 0, 0, 1])
    y_pred = np.array([1, 0, 0, 1, 1])

    table = build_confusion_matrix_table(y_true, y_pred)

    assert table.to_numpy().sum() == len(y_true)
    assert table.to_numpy().tolist() == [[1, 1], [1, 2]]


def test_youden_threshold_reproduces_its_own_j() -> None:
    """Binarizing at the returned cut-off must recover the reported J."""
    rng = np.random.default_rng(0)
    y_true = rng.integers(0, 2, size=200)
    y_score = rng.random(200) * 0.5 + y_true * 0.3

    result = compute_youden_threshold(y_true, y_score)
    y_pred = apply_threshold(y_score, result["threshold"])

    true_positive_rate = y_pred[y_true == 1].mean()
    false_positive_rate = y_pred[y_true == 0].mean()

    assert true_positive_rate - false_positive_rate == pytest.approx(
        result["youden_j"], abs=0.01
    )
