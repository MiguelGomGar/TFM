"""Tests for the metric computations that end up in the manuscript."""

import numpy as np
import pytest

from src.models.model_evaluation import compute_hard_metrics


def test_compute_hard_metrics_against_hand_counts() -> None:
    """Check every metric against a confusion matrix counted by hand.

    y_true / y_pred below give TP=3, TN=3, FP=2, FN=2, so:
      Accuracy    = 6/10
      Precision   = 3/5
      Recall      = 3/5
      Specificity = 3/5
      F1          = 3/5
    """
    y_true = np.array([1, 1, 1, 1, 1, 0, 0, 0, 0, 0])
    y_pred = np.array([1, 1, 1, 0, 0, 1, 1, 0, 0, 0])

    metrics = compute_hard_metrics(y_true, y_pred)

    assert metrics["Accuracy"] == pytest.approx(0.6)
    assert metrics["Precision"] == pytest.approx(0.6)
    assert metrics["Recall"] == pytest.approx(0.6)
    assert metrics["Specificity"] == pytest.approx(0.6)
    assert metrics["F1"] == pytest.approx(0.6)


def test_compute_hard_metrics_on_a_perfect_prediction() -> None:
    """A perfect classifier must score 1.0 everywhere."""
    y_true = np.array([0, 1, 0, 1])
    metrics = compute_hard_metrics(y_true, y_true)
    assert set(metrics) == {"Accuracy", "Precision", "Recall", "Specificity", "F1"}
    assert all(value == pytest.approx(1.0) for value in metrics.values())


def test_specificity_is_not_recall() -> None:
    """The two are computed on opposite classes and must not be conflated.

    A model predicting every case positive catches every event (recall 1) while
    ruling out nothing (specificity 0) — the degenerate case a single-metric
    report would hide.
    """
    y_true = np.array([1, 1, 0, 0])
    y_pred = np.array([1, 1, 1, 1])

    metrics = compute_hard_metrics(y_true, y_pred)

    assert metrics["Recall"] == pytest.approx(1.0)
    assert metrics["Specificity"] == pytest.approx(0.0)


def test_no_positive_prediction_does_not_raise() -> None:
    """Precision is undefined here; zero_division must keep it at 0.0.

    Real runs hit this whenever a model predicts the majority class for every
    patient, which is a plausible outcome on an imbalanced recurrence cohort.
    """
    y_true = np.array([1, 0, 0, 0])
    y_pred = np.array([0, 0, 0, 0])

    metrics = compute_hard_metrics(y_true, y_pred)

    assert metrics["Precision"] == pytest.approx(0.0)
    assert metrics["Recall"] == pytest.approx(0.0)
    assert metrics["Specificity"] == pytest.approx(1.0)
    assert metrics["Accuracy"] == pytest.approx(0.75)
