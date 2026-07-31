"""Threshold-sensitivity analysis for the project's best-performing model.

MLP on the multimodal (clinical + proteomic) data is the best-performing
model overall (see ``results/models/multimodal_data/models_metrics.csv``).
Every modelling phase elsewhere in the project reports its hard metrics at
the default 0.5 decision threshold only. This module scores the same fitted
MLP pipeline under two different ways of turning its continuous probability
into a binary call:

1. The default threshold (probability > 0.5).
2. The Youden-optimal threshold: the ROC cut-off that maximizes sensitivity
   + specificity - 1, a standard criterion for choosing clinical risk
   cut-offs.
"""

from pathlib import Path

import joblib
import numpy as np
import pandas as pd
from sklearn.metrics import confusion_matrix, roc_curve
from sklearn.pipeline import Pipeline


def load_fitted_pipeline(model_dir, abbreviation: str) -> Pipeline:
    """Load one fitted pipeline persisted by a modelling phase.

    Parameters
    ----------
    model_dir : str or Path
        Output directory of the modelling phase (e.g. MULTIMODAL_MODELS_DIR).
    abbreviation : str
        Model abbreviation, matching the ``optimized_{abbreviation}.joblib``
        filename saved by ``results_saving.save_model``.

    Returns
    -------
    sklearn.pipeline.Pipeline
        Fitted pipeline with 'preprocessor' and 'clf' steps.

    Raises
    ------
    FileNotFoundError
        If no matching joblib file exists in model_dir.
    """
    file_path = Path(model_dir) / f"optimized_{abbreviation}.joblib"
    if not file_path.exists():
        raise FileNotFoundError(f"No fitted pipeline found at {file_path}.")
    return joblib.load(file_path)


def get_model_feature_columns(fitted_pipeline: Pipeline) -> list[str]:
    """Return the exact predictors a fitted pipeline was trained on.

    Reads ``feature_names_in_`` off the fitted preprocessor rather than
    re-deriving the feature set from ``features_kept.csv`` files, since those
    reflect the *current* state of the feature-selection phases and may have
    drifted from whatever feature set was in use when this particular model
    was actually fitted.

    Parameters
    ----------
    fitted_pipeline : sklearn.pipeline.Pipeline
        Fitted pipeline with a 'preprocessor' step.

    Returns
    -------
    list of str
        Predictor names, in the order the preprocessor expects them.
    """
    return list(fitted_pipeline.named_steps["preprocessor"].feature_names_in_)


def get_positive_class_probability(model, X) -> np.ndarray:
    """Return the calibrated probability of the positive class.

    Parameters
    ----------
    model : sklearn estimator
        Fitted classifier or pipeline exposing predict_proba.
    X : array-like or pd.DataFrame
        Samples to score.

    Returns
    -------
    np.ndarray
        Positive-class probability per sample.

    Raises
    ------
    AttributeError
        If the model does not expose predict_proba (e.g. an SVM fitted with
        probability=False).
    """
    if not hasattr(model, "predict_proba"):
        raise AttributeError(
            f"{type(model).__name__} does not expose predict_proba; the "
            "probability comparison requires a calibrated model."
        )
    return model.predict_proba(X)[:, 1]


def compute_youden_threshold(y_test, y_prob) -> dict:
    """Find the ROC cut-off that maximizes Youden's J statistic.

    Youden's J = sensitivity + specificity - 1 = true positive rate - false
    positive rate. The threshold maximizing J is a standard criterion for
    picking a single operating point on a ROC curve, widely used to select
    cut-offs for clinical risk scores.

    Only 'threshold' and 'youden_j' are returned. roc_curve's own
    sensitivity/specificity at that point assume a >= comparison, while
    apply_threshold (used to actually binarize the predictions everywhere in
    this module) is strict (>). Since the Youden threshold is, by
    construction, one of the observed probability values, the two
    conventions can disagree by one patient; report sensitivity/specificity
    from the actually applied predictions (model_evaluation.compute_hard_metrics)
    instead.

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
