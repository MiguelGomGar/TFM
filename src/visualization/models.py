"""Evaluation metrics visualization."""

import matplotlib

# Select a non-interactive backend before pyplot is imported (see io.save_figure).
matplotlib.use("Agg", force=True)

import matplotlib.pyplot as plt


def plot_roc_curves(
    roc_plot_data: list[tuple], title: str = "ROC curves for risk scores"
):
    """Plot ROC curves for multiple models/scores.

    Parameters
    ----------
    roc_plot_data : list of tuple
        List of (label, fpr, tpr, auc) tuples for each curve.
    title : str, default 'ROC curves for risk scores'
        Plot title.

    Returns
    -------
    matplotlib.figure.Figure
        Figure object containing the ROC curves.
    """
    fig, ax = plt.subplots(figsize=(7, 6))

    for column, fpr, tpr, roc_auc in roc_plot_data:
        ax.plot(fpr, tpr, linewidth=2, label=f"{column} (AUC = {roc_auc:.3f})")

    ax.plot([0, 1], [0, 1], linestyle="--", color="grey", label="Random")
    ax.set_title(title)
    ax.set_xlabel("False positive rate")
    ax.set_ylabel("True positive rate")
    ax.legend(loc="lower right", fontsize=9)
    ax.grid(alpha=0.3)
    fig.tight_layout()

    return fig


def plot_pr_curves(
    pr_plot_data: list[tuple],
    prevalence: float,
    title: str = "Precision-recall curves for risk scores",
):
    """Plot Precision-Recall curves for multiple models/scores.

    Parameters
    ----------
    pr_plot_data : list of tuple
        List of (label, recall, precision, auc) tuples for each curve.
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

    for column, recall, precision, average_precision in pr_plot_data:
        ax.plot(
            recall,
            precision,
            linewidth=2,
            label=f"{column} (AUC = {average_precision:.3f})",
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
