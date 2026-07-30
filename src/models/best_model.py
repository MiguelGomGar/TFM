"""Threshold-sensitivity analysis for the project's best-performing model.

MLP on the multimodal (clinical + proteomic) data is the best-performing
model overall (see ``results/models/multimodal_data/models_metrics.csv``).
Every modelling phase elsewhere in the project reports its hard metrics at
the default 0.5 decision threshold only. This module scores the same fitted
MLP pipeline under three different ways of turning its continuous
probability into a binary call:

1. The default threshold (probability > 0.5).
2. The Youden-optimal threshold: the ROC cut-off that maximizes sensitivity
   + specificity - 1, a standard criterion for choosing clinical risk
   cut-offs.
3. A fuzzy threshold band (see ``config.FUZZY_THRESHOLD_BAND``): patients
   whose predicted probability falls inside the band are left out as
   'indeterminate', and the hard metrics are computed only on the remaining,
   more confidently classified patients.
"""

import numpy as np
import pandas as pd
from sklearn.metrics import confusion_matrix, roc_curve


def compute_youden_threshold(y_test, y_prob) -> dict:
    """Find the ROC cut-off that maximizes Youden's J statistic.

    Youden's J = sensitivity + specificity - 1 = true positive rate - false
    positive rate. The threshold maximizing J is a standard criterion for
    picking a single operating point on a ROC curve, widely used to select
    cut-offs for clinical risk scores.

    Only 'threshold' and 'youden_j' are returned. roc_curve's own
    sensitivity/specificity at that point assume a >= comparison, while
    apply_threshold (used to actually binarize the predictions everywhere in
    this module) is strict (>), for consistency with the fuzzy-threshold
    scenario's wording. Since the Youden threshold is, by construction, one
    of the observed probability values, the two conventions can disagree by
    one patient; report sensitivity/specificity from the actually applied
    predictions (model_evaluation.compute_hard_metrics) instead.

    Parameters
    ----------
    y_test : array-like
        Binary external validation target.
    y_prob : array-like
        Predicted probability of the positive class.

    Returns
    -------
    dict
        Keys 'threshold' (the selected cut-off) and 'youden_j'.
    """
    false_positive_rate, true_positive_rate, thresholds = roc_curve(y_test, y_prob)
    youden_j = true_positive_rate - false_positive_rate
    best_index = int(np.argmax(youden_j))
    return {
        "threshold": float(thresholds[best_index]),
        "youden_j": float(youden_j[best_index]),
    }


def apply_threshold(y_prob, threshold: float) -> np.ndarray:
    """Binarize probabilities at an arbitrary cut-off.

    Parameters
    ----------
    y_prob : array-like
        Predicted probability of the positive class.
    threshold : float
        Cut-off; probabilities strictly greater than this are labeled 1.

    Returns
    -------
    np.ndarray
        Binary predictions (0/1).
    """
    return (np.asarray(y_prob) > threshold).astype(int)


def apply_fuzzy_threshold(y_test, y_prob, low: float, high: float):
    """Binarize only the patients scored outside the indeterminate band.

    Patients with ``low <= probability <= high`` are dropped as
    'indeterminate'; the rest are binarized as 1 if probability > high, 0 if
    probability < low.

    Parameters
    ----------
    y_test : array-like
        Binary external validation target.
    y_prob : array-like
        Predicted probability of the positive class.
    low, high : float
        Bounds of the indeterminate band (inclusive on both ends).

    Returns
    -------
    y_test_determinate : np.ndarray
        Target restricted to the determinate patients.
    y_pred_determinate : np.ndarray
        Binary predictions for the determinate patients.
    n_indeterminate : int
        Number of patients excluded as indeterminate.
    """
    y_prob = np.asarray(y_prob)
    y_test = np.asarray(y_test)

    determinate = (y_prob < low) | (y_prob > high)
    y_pred_determinate = (y_prob[determinate] > high).astype(int)

    return y_test[determinate], y_pred_determinate, int((~determinate).sum())


def build_confusion_matrix_table(y_true, y_pred, labels=(0, 1)) -> pd.DataFrame:
    """Build a labeled confusion matrix.

    Parameters
    ----------
    y_true : array-like
        Binary target.
    y_pred : array-like
        Binary predictions.
    labels : tuple, default (0, 1)
        Class order (negative, positive).

    Returns
    -------
    pd.DataFrame
        2x2 table; rows are actual classes ('Actual: No'/'Actual: Yes'),
        columns are predicted classes ('Predicted: No'/'Predicted: Yes').
    """
    matrix = confusion_matrix(y_true, y_pred, labels=list(labels))
    class_names = {0: "No", 1: "Yes"}
    index = [f"Actual: {class_names[label]}" for label in labels]
    columns = [f"Predicted: {class_names[label]}" for label in labels]
    return pd.DataFrame(matrix, index=index, columns=columns)


def build_threshold_metrics_table(metrics_by_scenario: dict) -> pd.DataFrame:
    """Stack the hard metrics of several threshold scenarios into one table.

    Parameters
    ----------
    metrics_by_scenario : dict
        Mapping of scenario label to its hard-metrics dict (as returned by
        model_evaluation.compute_hard_metrics).

    Returns
    -------
    pd.DataFrame
        Columns ['Scenario', 'Metric', 'Score'].
    """
    frames = [
        pd.DataFrame({"Scenario": scenario, "Metric": list(metrics), "Score": list(metrics.values())})
        for scenario, metrics in metrics_by_scenario.items()
    ]
    return pd.concat(frames, ignore_index=True)
