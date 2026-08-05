"""Threshold-sensitivity analysis for the project's best-performing models.

EN, SVM and MLP on the multimodal (clinical + proteomic) data are the three
best-performing models overall, ranked by test PR-AUC (see
``results/models/multimodal_data/models_metrics.csv``). Every modelling
phase elsewhere in the project reports its hard metrics at the default 0.5
decision threshold only. This module rescores each fitted pipeline at its
Youden-optimal threshold instead: the ROC cut-off that maximizes sensitivity
+ specificity - 1, a standard criterion for choosing clinical risk cut-offs.
"""

from collections.abc import Sequence
from logging import Logger
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.metrics import average_precision_score, confusion_matrix, roc_auc_score, roc_curve
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline

from src.config import SEED, TEST_SIZE, THRESHOLD_COMPARISON_METRICS
from src.models.model_evaluation import compute_hard_metrics, get_decision_scores
from src.utils.io import read_joblib


def load_fitted_pipeline(model_dir: str | Path, abbreviation: str) -> Pipeline:
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
    return read_joblib(Path(model_dir) / f"optimized_{abbreviation}.joblib")


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


def compute_youden_threshold(
    y_test: pd.Series | np.ndarray, y_score: np.ndarray
) -> dict:
    """Find the ROC cut-off that maximizes Youden's J statistic.

    Youden's J = sensitivity + specificity - 1 = true positive rate - false
    positive rate. The threshold maximizing J is a standard criterion for
    picking a single operating point on a ROC curve, widely used to select
    cut-offs for clinical risk scores.

    Only 'threshold' and 'youden_j' are returned. roc_curve's own
    sensitivity/specificity at that point assume a >= comparison, while
    apply_threshold (used to actually binarize the predictions everywhere in
    this module) is strict (>). Since the Youden threshold is, by
    construction, one of the observed score values, the two conventions can
    disagree by one patient; report sensitivity/specificity from the
    actually applied predictions (model_evaluation.compute_hard_metrics)
    instead.

    Parameters
    ----------
    y_test : array-like
        Binary external validation target.
    y_score : array-like
        Continuous decision score of the positive class, as returned by
        model_evaluation.get_decision_scores (predict_proba when available,
        decision_function otherwise, e.g. for an SVM fitted without
        probability calibration).

    Returns
    -------
    dict
        Keys 'threshold' (the selected cut-off) and 'youden_j'.
    """
    false_positive_rate, true_positive_rate, thresholds = roc_curve(y_test, y_score)
    youden_j = true_positive_rate - false_positive_rate
    best_index = int(np.argmax(youden_j))
    return {
        "threshold": float(thresholds[best_index]),
        "youden_j": float(youden_j[best_index]),
    }


def apply_threshold(y_score: np.ndarray, threshold: float) -> np.ndarray:
    """Binarize a continuous decision score at an arbitrary cut-off.

    Parameters
    ----------
    y_score : array-like
        Continuous decision score of the positive class (probability or
        SVM decision_function value).
    threshold : float
        Cut-off; scores strictly greater than this are labeled 1.

    Returns
    -------
    np.ndarray
        Binary predictions (0/1).
    """
    return (np.asarray(y_score) > threshold).astype(int)


def build_confusion_matrix_table(
    y_true: pd.Series | np.ndarray,
    y_pred: np.ndarray,
    labels: tuple[int, int] = (0, 1),
) -> pd.DataFrame:
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


def build_ranking_metrics_table(ranking_metrics: dict[str, float]) -> pd.DataFrame:
    """Lay a model's threshold-free ranking metrics out as a saveable table.

    Parameters
    ----------
    ranking_metrics : dict of str to float
        Metric label to score, e.g. {'ROC-AUC': 0.71, 'PR-AUC': 0.54}. These
        are computed from the continuous decision score, so they do not depend
        on the chosen cut-off.

    Returns
    -------
    pd.DataFrame
        Columns ['Metric', 'Score'], one row per metric.
    """
    return pd.DataFrame(
        {"Metric": list(ranking_metrics), "Score": list(ranking_metrics.values())}
    )


def build_threshold_info_table(
    results_by_model: dict[str, dict], model_order: Sequence[str]
) -> pd.DataFrame:
    """Gather the chosen cut-off of every model into one table.

    Parameters
    ----------
    results_by_model : dict of str to dict
        Model abbreviation to its ``analyze_model`` result, which must carry a
        'threshold_rows' entry.
    model_order : sequence of str
        Model abbreviations, in reporting order.

    Returns
    -------
    pd.DataFrame
        One row per model and threshold scenario.
    """
    rows = [
        row
        for abbreviation in model_order
        for row in results_by_model[abbreviation]["threshold_rows"]
    ]
    return pd.DataFrame(rows)


def build_model_comparison_table(
    results_by_model: dict[str, dict],
    model_order: Sequence[str],
    metrics: Sequence[str] = THRESHOLD_COMPARISON_METRICS,
) -> pd.DataFrame:
    """Compare several models at their own optimal threshold, metric by metric.

    Restricted to ``metrics`` because the per-model tables already report every
    metric; this one feeds a grouped bar chart, which stops being readable past
    a handful of bars per model.

    Parameters
    ----------
    results_by_model : dict of str to dict
        Model abbreviation to its ``analyze_model`` result, which must carry a
        'metrics_optimal' entry.
    model_order : sequence of str
        Model abbreviations, in reporting order.
    metrics : sequence of str
        Hard metrics to include.

    Returns
    -------
    pd.DataFrame
        Columns ['Scenario', 'Metric', 'Score'], where Scenario is the model.
    """
    return build_threshold_metrics_table(
        {
            abbreviation: {
                metric: results_by_model[abbreviation]["metrics_optimal"][metric]
                for metric in metrics
            }
            for abbreviation in model_order
        }
    )


def analyze_model(
    abbreviation: str,
    model_dir: str | Path,
    df: pd.DataFrame,
    y: pd.Series,
    seed: int = SEED,
    test_size: float = TEST_SIZE,
    logger: Logger | None = None,
) -> dict:
    """Score one fitted pipeline at its Youden-optimal threshold.

    Parameters
    ----------
    abbreviation : str
        Model abbreviation, matching the ``optimized_{abbreviation}.joblib``
        filename saved by ``results_saving.save_model``.
    model_dir : str or Path
        Output directory of the modelling phase the pipeline was fitted in.
    df : pd.DataFrame
        Cleaned dataset (predictors + target columns).
    y : pd.Series
        Encoded binary target, aligned to df's index.
    seed : int, default SEED
        Random seed for the external validation split.
    test_size : float, default TEST_SIZE
        Size of the external validation split.
    logger : logging.Logger, optional
        Logger for progress and result reporting.

    Returns
    -------
    dict
        Everything the caller needs to save this model's results and feed
        the cross-model comparison chart: 'ranking_metrics', 'metrics_table',
        'metrics_optimal', 'cm_optimal', 'threshold_rows' (list of dicts for
        the combined threshold_info table), 'optimal_label'.
    """
    if logger:
        logger.info(f"Loading the fitted {abbreviation} pipeline from {model_dir}...")
    model = load_fitted_pipeline(model_dir, abbreviation)
    feature_columns = get_model_feature_columns(model)

    X = df[feature_columns]
    _, X_test, _, y_test = train_test_split(
        X, y, test_size=test_size, random_state=seed, shuffle=True, stratify=y
    )
    if logger:
        logger.info(
            f"[{abbreviation}] Reconstructed the external validation set: "
            f"{X_test.shape[0]} patients ({y_test.mean():.1%} recurrence)."
        )

    y_score = get_decision_scores(model, X_test)
    # SVC is fitted without probability calibration, so its scores come from
    # decision_function rather than predict_proba; label whichever applies.
    score_name = "p" if hasattr(model, "predict_proba") else "score"

    # Threshold-independent ranking performance, kept only as context: it
    # does not depend on where the cut-off is drawn.
    ranking_metrics = {
        "ROC-AUC": roc_auc_score(y_test, y_score),
        "PR-AUC": average_precision_score(y_test, y_score),
    }

    # ------------------------------------------------------------------
    # Youden-optimal threshold
    # ------------------------------------------------------------------
    youden = compute_youden_threshold(y_test, y_score)
    y_pred_optimal = apply_threshold(y_score, youden["threshold"])
    metrics_optimal = compute_hard_metrics(y_test, y_pred_optimal)
    cm_optimal = build_confusion_matrix_table(y_test, y_pred_optimal)
    if logger:
        logger.info(
            f"[{abbreviation}] Youden-optimal threshold: {youden['threshold']:.3f} "
            f"(J = {youden['youden_j']:.3f}); metrics: {metrics_optimal}"
        )

    optimal_label = f"Optimal (Youden, {score_name} > {youden['threshold']:.3f})"

    metrics_table = build_threshold_metrics_table({optimal_label: metrics_optimal})

    threshold_rows = [
        {
            "Model": abbreviation,
            "Scenario": optimal_label,
            "Threshold": youden["threshold"],
            "Method": f"Youden's J = {youden['youden_j']:.3f} "
            f"(sensitivity = {metrics_optimal['Recall']:.3f}, "
            f"specificity = {metrics_optimal['Specificity']:.3f})",
            "N_Test": int(X_test.shape[0]),
            "N_Scored": int(X_test.shape[0]),
        },
    ]

    return {
        "ranking_metrics": ranking_metrics,
        "metrics_table": metrics_table,
        "metrics_optimal": metrics_optimal,
        "cm_optimal": cm_optimal,
        "threshold_rows": threshold_rows,
        "optimal_label": optimal_label,
    }
