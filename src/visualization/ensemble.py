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


def plot_probability_violin(
    probabilities_df: pd.DataFrame,
    title: str = "Predicted probability distribution by model",
    figsize=(8, 6),
) -> plt.Figure:
    """Plot one violin per model with its test-set positive-class probabilities.

    Parameters
    ----------
    probabilities_df : pd.DataFrame
        Long-format table with columns ['Model', 'Probability'], as returned
        by ensemble.build_probability_distribution_table.
    title : str, default 'Predicted probability distribution by model'
        Plot title.
    figsize : tuple, default (8, 6)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
        Violin plot figure, one violin per model.
    """
    model_names = list(dict.fromkeys(probabilities_df["Model"]))
    colors = _model_colors(model_names)

    fig, ax = plt.subplots(figsize=figsize)
    sns.violinplot(
        data=probabilities_df,
        x="Model",
        y="Probability",
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
        data=probabilities_df,
        x="Model",
        y="Probability",
        order=model_names,
        color="#2c3e50",
        size=2.5,
        alpha=0.35,
        jitter=0.2,
        ax=ax,
    )

    ax.axhline(
        0.5,
        linestyle="--",
        linewidth=1.2,
        color="#94a3b8",
        label="Decision threshold = 0.5",
    )
    ax.set_ylim(-0.02, 1.02)
    ax.set_title(title, fontsize=12, fontweight="bold", pad=15)
    ax.set_xlabel(None)
    ax.set_ylabel(
        "Predicted probability of recurrence",
        fontsize=10,
        fontweight="bold",
        color="#2c3e50",
    )
    ax.legend(loc="upper right", fontsize=9)
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
