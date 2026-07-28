"""Evaluation of the fitted models on the external validation set."""

import numpy as np
import pandas as pd
from sklearn.metrics import (
    accuracy_score,
    average_precision_score,
    f1_score,
    precision_recall_curve,
    precision_score,
    recall_score,
    roc_auc_score,
    roc_curve,
)

from src.config import MODEL_ORDER


def get_decision_scores(model, X) -> np.ndarray:
    """Return continuous scores for the positive class.

    Uses predict_proba when available and falls back to decision_function,
    which is what SVC exposes since it is fitted without probability
    calibration.

    Parameters
    ----------
    model : sklearn estimator
        Fitted classifier or pipeline.
    X : array-like or pd.DataFrame
        Samples to score.

    Returns
    -------
    np.ndarray
        One score per sample; higher means more likely to be positive.

    Raises
    ------
    AttributeError
        If the model exposes neither predict_proba nor decision_function.
    """
    if hasattr(model, "predict_proba"):
        return model.predict_proba(X)[:, 1]
    if hasattr(model, "decision_function"):
        return model.decision_function(X)
    raise AttributeError(
        f"{type(model).__name__} exposes neither predict_proba nor "
        "decision_function, so ranking metrics cannot be computed."
    )


def evaluate_on_test(model, X_test, y_test):
    """Score a fitted model on the held-out external validation set.

    Parameters
    ----------
    model : sklearn estimator
        Fitted classifier or pipeline.
    X_test : pd.DataFrame
        External validation features.
    y_test : pd.Series or np.ndarray
        Binary external validation target (0/1).

    Returns
    -------
    metrics : pd.DataFrame
        Long-format frame with columns ['Metric', 'Dataset', 'Score', 'Fold'],
        where Dataset is 'Test' and Fold is NaN, matching the layout produced
        by the internal cross-validation.
    curves : dict
        Keys 'fpr', 'tpr', 'precision', 'recall', 'roc_auc' and 'pr_auc'.
    """
    y_pred = model.predict(X_test)
    y_score = get_decision_scores(model, X_test)

    roc_auc = roc_auc_score(y_test, y_score)
    pr_auc = average_precision_score(y_test, y_score)

    scores = {
        "Accuracy": accuracy_score(y_test, y_pred),
        "Precision": precision_score(y_test, y_pred, zero_division=0),
        "Recall": recall_score(y_test, y_pred, zero_division=0),
        "Specificity": recall_score(y_test, y_pred, pos_label=0, zero_division=0),
        "F1": f1_score(y_test, y_pred, zero_division=0),
        "ROC-AUC": roc_auc,
        "PR-AUC": pr_auc,
    }

    metrics = pd.DataFrame(
        {
            "Metric": list(scores),
            "Dataset": "Test",
            "Score": list(scores.values()),
            "Fold": np.nan,
        }
    )

    false_positive_rate, true_positive_rate, _ = roc_curve(y_test, y_score)
    precision, recall, _ = precision_recall_curve(y_test, y_score)

    curves = {
        "fpr": false_positive_rate,
        "tpr": true_positive_rate,
        "precision": precision,
        "recall": recall,
        "roc_auc": roc_auc,
        "pr_auc": pr_auc,
    }

    return metrics, curves


def build_auc_table(metrics_df: pd.DataFrame, metric: str) -> pd.DataFrame:
    """Extract the external validation scores of one metric, model by model.

    Parameters
    ----------
    metrics_df : pd.DataFrame
        Consolidated metrics with columns ['Model', 'Metric', 'Dataset',
        'Score'], as returned by save_metrics_results.
    metric : str
        Metric to extract, e.g. 'ROC-AUC' or 'PR-AUC'.

    Returns
    -------
    pd.DataFrame
        Columns ['Model', 'Metric', 'Score'], sorted by score (highest first).
    """
    selection = metrics_df[
        (metrics_df["Dataset"] == "Test") & (metrics_df["Metric"] == metric)
    ]
    table = selection[["Model", "Metric", "Score"]].copy()
    return table.sort_values(by="Score", ascending=False).reset_index(drop=True)


def summarize_curve_areas(curves_by_model: dict, area_key: str) -> dict:
    """Collect the area under the curve of every model.

    Parameters
    ----------
    curves_by_model : dict
        Mapping of model abbreviation to the curves dict returned by
        evaluate_on_test.
    area_key : str
        Either 'roc_auc' or 'pr_auc'.

    Returns
    -------
    dict
        Mapping of model abbreviation to the requested area.
    """
    return {model: curves[area_key] for model, curves in curves_by_model.items()}


def build_modality_comparison_table(
    clinical_metrics: pd.DataFrame,
    multimodal_metrics: pd.DataFrame,
    metrics=("ROC-AUC", "PR-AUC"),
) -> pd.DataFrame:
    """Pair the external validation scores of the clinical and multimodal arms.

    Both arms must have been trained on the same matched subcohort for the
    comparison to be meaningful.

    Parameters
    ----------
    clinical_metrics : pd.DataFrame
        Consolidated metrics of the clinical-only arm, with columns
        ['Model', 'Metric', 'Dataset', 'Score'].
    multimodal_metrics : pd.DataFrame
        Consolidated metrics of the multimodal arm, same layout.
    metrics : tuple of str, default ('ROC-AUC', 'PR-AUC')
        Metrics to compare.

    Returns
    -------
    pd.DataFrame
        Long-format frame with columns ['Model', 'Metric', 'Modality', 'Score'],
        ordered by MODEL_ORDER and then by the requested metric order.
    """
    frames = []
    for modality, metrics_df in (
        ("Clinical", clinical_metrics),
        ("Multimodal", multimodal_metrics),
    ):
        selection = metrics_df[
            (metrics_df["Dataset"] == "Test") & (metrics_df["Metric"].isin(metrics))
        ]
        frame = selection[["Model", "Metric", "Score"]].copy()
        frame["Modality"] = modality
        frames.append(frame)

    comparison = pd.concat(frames, ignore_index=True)
    comparison["Model"] = pd.Categorical(
        comparison["Model"], categories=MODEL_ORDER, ordered=True
    )
    comparison["Metric"] = pd.Categorical(
        comparison["Metric"], categories=list(metrics), ordered=True
    )
    comparison = comparison.sort_values(by=["Model", "Metric", "Modality"])
    comparison["Model"] = comparison["Model"].astype(str)
    comparison["Metric"] = comparison["Metric"].astype(str)

    return comparison[["Model", "Metric", "Modality", "Score"]].reset_index(drop=True)


def build_modality_delta_table(comparison: pd.DataFrame) -> pd.DataFrame:
    """Turn the long comparison frame into one row per model and metric.

    Parameters
    ----------
    comparison : pd.DataFrame
        Long-format comparison as returned by build_modality_comparison_table.

    Returns
    -------
    pd.DataFrame
        Columns ['Model', 'Metric', 'Clinical', 'Multimodal', 'Delta'], where
        Delta is the multimodal score minus the clinical one.
    """
    wide = comparison.pivot_table(
        index=["Model", "Metric"], columns="Modality", values="Score", observed=True
    ).reset_index()
    wide.columns.name = None

    for modality in ("Clinical", "Multimodal"):
        if modality not in wide.columns:
            wide[modality] = np.nan

    wide["Delta"] = wide["Multimodal"] - wide["Clinical"]
    wide["Model"] = pd.Categorical(wide["Model"], categories=MODEL_ORDER, ordered=True)
    wide = wide.sort_values(by=["Model", "Metric"])
    wide["Model"] = wide["Model"].astype(str)

    return wide[["Model", "Metric", "Clinical", "Multimodal", "Delta"]].reset_index(
        drop=True
    )
