"""ROC and Precision-Recall curve plotting."""

import matplotlib.pyplot as plt


def plot_roc_curves(roc_plot_data, title="ROC curves for risk scores"):
    """Render ROC curves from prepared curve data."""

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


def plot_pr_curves(pr_plot_data, prevalence, title="Precision-recall curves for risk scores"):
    """Render Precision-Recall curves from prepared curve data."""

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
