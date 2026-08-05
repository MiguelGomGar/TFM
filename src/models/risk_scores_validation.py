"""Risk score evaluation and metrics calculation."""

from collections.abc import Sequence

import pandas as pd
from sklearn.metrics import (
    average_precision_score,
    precision_recall_curve,
    roc_auc_score,
    roc_curve,
)


def evaluate_risk_scores(
    df: pd.DataFrame, target: str, score_columns: Sequence[str]
) -> tuple[pd.DataFrame, list[tuple], list[tuple]]:
    """Evaluate performance of multiple risk scores (ROC-AUC and PR-AUC).

    Parameters
    ----------
    df : pd.DataFrame
        Dataframe containing the risk score columns.
    target : pd.Series
        Binary target variable (1 = event, 0 = no event).
    score_columns : list of str
        Names of risk score columns to evaluate.

    Returns
    -------
    evaluation_df : pd.DataFrame
        Summary metrics (columns: 'score', 'n_samples', 'roc_auc', 'average_precision'),
        sorted by ROC-AUC descending.
    roc_curves : list of tuple
        List of (score_name, fpr, tpr, roc_auc), one per score.
    pr_curves : list of tuple
        List of (score_name, recall, precision, average_precision), one per
        score.
    """
    evaluation_rows = []
    roc_curves = []
    pr_curves = []

    for column in score_columns:
        mask = df[column].notna() & target.notna()
        y_true = target.loc[mask].astype(int)
        y_score = df.loc[mask, column].astype(float)

        fpr, tpr, _ = roc_curve(y_true, y_score)
        precision, recall, _ = precision_recall_curve(y_true, y_score)
        roc_auc = roc_auc_score(y_true, y_score)
        average_precision = average_precision_score(y_true, y_score)

        evaluation_rows.append(
            {
                "score": column,
                "n_samples": int(mask.sum()),
                "roc_auc": roc_auc,
                "average_precision": average_precision,
            }
        )
        roc_curves.append((column, fpr, tpr, roc_auc))
        pr_curves.append((column, recall, precision, average_precision))

    evaluation_df = pd.DataFrame(evaluation_rows).sort_values(
        "roc_auc", ascending=False
    )

    return evaluation_df, roc_curves, pr_curves
