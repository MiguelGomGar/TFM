"""Risk score evaluation and metrics calculation."""

import pandas as pd
from sklearn.metrics import (
    average_precision_score,
    precision_recall_curve,
    roc_auc_score,
    roc_curve,
)


def evaluate_risk_scores(df, target, score_columns):
    """Evaluate multiple risk scores and return metrics plus curve data."""

    evaluation_rows = []
    roc_plot_data = []
    pr_plot_data = []

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
        roc_plot_data.append((column, fpr, tpr, roc_auc))
        pr_plot_data.append((column, recall, precision, average_precision))

    evaluation_df = pd.DataFrame(evaluation_rows).sort_values(
        "roc_auc", ascending=False
    )

    return evaluation_df, roc_plot_data, pr_plot_data
