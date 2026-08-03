"""Evaluation metrics visualization."""

import matplotlib
import pandas as pd

# Select a non-interactive backend before pyplot is imported (see io.save_figure).
matplotlib.use("Agg", force=True)

import matplotlib.pyplot as plt


def _curve_order(curves_df: pd.DataFrame) -> list:
    """List the scores of a curves table in their order of appearance."""
    return list(dict.fromkeys(curves_df["Model"]))


def plot_roc_curves(
    curves_df: pd.DataFrame,
    auc_by_score: dict,
    title: str = "ROC curves for risk scores",
) -> plt.Figure:
    """Plot ROC curves for multiple risk scores.

    Parameters
    ----------
    curves_df : pd.DataFrame
        Long-format curve coordinates with columns ['False Positive Rate',
        'True Positive Rate', 'Model'], as saved by save_curves_results. One
        curve is drawn per distinct 'Model' value, in order of appearance.
    auc_by_score : dict
        Mapping of score name to its ROC-AUC, shown in the legend.
    title : str, default 'ROC curves for risk scores'
        Plot title.

    Returns
    -------
    matplotlib.figure.Figure
        Figure object containing the ROC curves.
    """
    fig, ax = plt.subplots(figsize=(7, 6))

    for score in _curve_order(curves_df):
        curve = curves_df[curves_df["Model"] == score]
        ax.plot(
            curve["False Positive Rate"],
            curve["True Positive Rate"],
            linewidth=2,
            label=f"{score} (AUC = {auc_by_score[score]:.3f})",
        )

    ax.plot([0, 1], [0, 1], linestyle="--", color="grey", label="Random")
    ax.set_title(title)
    ax.set_xlabel("False positive rate")
    ax.set_ylabel("True positive rate")
    ax.legend(loc="lower right", fontsize=9)
    ax.grid(alpha=0.3)
    fig.tight_layout()

    return fig


def plot_pr_curves(
    curves_df: pd.DataFrame,
    pr_auc_by_score: dict,
    prevalence: float,
    title: str = "Precision-recall curves for risk scores",
) -> plt.Figure:
    """Plot Precision-Recall curves for multiple risk scores.

    Parameters
    ----------
    curves_df : pd.DataFrame
        Long-format curve coordinates with columns ['Recall', 'Precision',
        'Model'], as saved by save_curves_results. One curve is drawn per
        distinct 'Model' value, in order of appearance.
    pr_auc_by_score : dict
        Mapping of score name to its average precision, shown in the legend.
    prevalence : float
        Event rate (0-1) for the no-skill baseline reference line.
    title : str, default 'Precision-recall curves for risk scores'
        Plot title.

    Returns
    -------
    matplotlib.figure.Figure
        Figure object containing the PR curves.
    """
    fig, ax = plt.subplots(figsize=(7, 6))

    for score in _curve_order(curves_df):
        curve = curves_df[curves_df["Model"] == score]
        ax.plot(
            curve["Recall"],
            curve["Precision"],
            linewidth=2,
            label=f"{score} (AUC = {pr_auc_by_score[score]:.3f})",
        )

    ax.axhline(
        prevalence,
        linestyle="--",
        color="grey",
        label=f"No-skill baseline = {prevalence:.3f}",
    )
    ax.set_title(title)
    ax.set_xlabel("Recall")
    ax.set_ylabel("Precision")
    ax.legend(loc="lower right", fontsize=9)
    ax.grid(alpha=0.3)
    fig.tight_layout()

    return fig
