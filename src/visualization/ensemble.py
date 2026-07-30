"""Plots comparing the top multimodal models and their majority-voting ensemble."""

import matplotlib

# Select a non-interactive backend before pyplot is imported, consistent with
# every other visualization module in the project.
matplotlib.use("Agg", force=True)

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

from src.config import ENSEMBLE_COLOR
from src.visualization.model_evaluation import _model_colors, _style_axes


def plot_score_violin(
    scores_df: pd.DataFrame,
    title: str = "Predicted score distribution by model",
    figsize=(8, 6),
) -> plt.Figure:
    """Plot one violin per model with its test-set positive-class scores.

    Each model's score is on its own scale: EN and MLP contribute calibrated
    probabilities in [0, 1] (decision threshold at 0.5), while the SVM
    contributes its raw decision_function distance to the separating
    hyperplane (decision threshold at 0), since it is fitted with
    probability=False. No shared axis limit or threshold line is drawn, as
    neither would be meaningful across every violin.

    Parameters
    ----------
    scores_df : pd.DataFrame
        Long-format table with columns ['Model', 'Score'], as returned by
        ensemble.build_score_distribution_table.
    title : str, default 'Predicted score distribution by model'
        Plot title.
    figsize : tuple, default (8, 6)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
        Violin plot figure, one violin per model.
    """
    model_names = list(dict.fromkeys(scores_df["Model"]))
    colors = _model_colors(model_names)

    fig, ax = plt.subplots(figsize=figsize)
    sns.violinplot(
        data=scores_df,
        x="Model",
        y="Score",
        order=model_names,
        hue="Model",
        hue_order=model_names,
        palette=colors,
        legend=False,
        cut=0,
        inner="box",
        ax=ax,
    )
    sns.stripplot(
        data=scores_df,
        x="Model",
        y="Score",
        order=model_names,
        color="#2c3e50",
        size=2.5,
        alpha=0.35,
        jitter=0.2,
        ax=ax,
    )

    ax.set_title(title, fontsize=12, fontweight="bold", pad=15)
    ax.set_xlabel(None)
    ax.set_ylabel(
        "Positive-class score (probability or decision function)",
        fontsize=10,
        fontweight="bold",
        color="#2c3e50",
    )
    _style_axes(ax, grid_axis="y")
    fig.tight_layout()

    return fig


def ensemble_curve_colors(individual_models: list[str], ensemble_label: str = "Ensemble") -> dict:
    """Assign colors to the individual models plus a distinct ensemble color.

    Parameters
    ----------
    individual_models : list of str
        Individual model names, in reporting order (e.g. config.ENSEMBLE_MODELS).
    ensemble_label : str, default 'Ensemble'
        Label used for the ensemble series.

    Returns
    -------
    dict
        Mapping of model/ensemble label to hex color, ready to pass as the
        ``colors`` argument of plot_model_roc_curves / plot_model_pr_curves.
    """
    colors = _model_colors(individual_models)
    colors[ensemble_label] = ENSEMBLE_COLOR
    return colors
