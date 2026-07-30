"""Plots reporting the training and evaluation of the predictive models."""

import matplotlib

# Select a non-interactive backend before pyplot is imported. The modelling
# pipelines render dozens of figures in a loop, and GUI backends retain a figure
# manager plus native resources per figure, exhausting memory mid-run.
matplotlib.use("Agg", force=True)

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

from src.config import (
    BASELINE_COLOR,
    INTERNAL_VALIDATION_COLORS,
    MODALITY_COLORS,
    MODEL_BAR_COLOR,
    MODEL_PALETTE,
)
from src.visualization.plot_utils import label_bars


def _style_axes(ax, grid_axis: str = "y") -> None:
    """Apply the shared axis styling used across the project figures.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        Axes to style.
    grid_axis : str, default 'y'
        Axis along which the grid is drawn ('x', 'y' or 'both').

    Returns
    -------
    None
    """
    ax.tick_params(axis="both", labelcolor="#34495e", labelsize=10)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight("bold")
    ax.grid(axis=grid_axis, color="#eaeded", linewidth=0.6)
    ax.grid(axis="x", visible=False)
    ax.set_axisbelow(True)
    sns.despine(ax=ax)


def _model_colors(model_names) -> dict:
    """Assign a stable color to each model from the project palette.

    Parameters
    ----------
    model_names : iterable of str
        Model names, in reporting order.

    Returns
    -------
    dict
        Mapping of model name to hex color.
    """
    return {
        name: MODEL_PALETTE[index % len(MODEL_PALETTE)]
        for index, name in enumerate(model_names)
    }


def plot_internal_validation(
    summary_df: pd.DataFrame, model_name: str, figsize=(8, 6)
) -> plt.Figure:
    """Plot the overfitting analysis of one model on the internal folds.

    Draws one group of bars per metric, with a series for the training folds
    and another for the held-out validation fold. Bar heights are the mean
    across folds and the error bars show the standard deviation.

    Parameters
    ----------
    summary_df : pd.DataFrame
        Summary with columns ['Model', 'Metric', 'Dataset', 'Mean', 'Std',
        'N_Folds'], where Dataset is 'Train' or 'Validation'.
    model_name : str
        Model name shown in the title.
    figsize : tuple, default (8, 6)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
        Grouped bar chart figure.
    """
    metrics = list(dict.fromkeys(summary_df["Metric"]))
    datasets = [
        dataset
        for dataset in ("Train", "Validation")
        if dataset in set(summary_df["Dataset"])
    ]

    positions = np.arange(len(metrics), dtype=float)
    width = 0.8 / max(len(datasets), 1)

    fig, ax = plt.subplots(figsize=figsize)

    for index, dataset in enumerate(datasets):
        subset = summary_df[summary_df["Dataset"] == dataset].set_index("Metric")
        means = [subset.loc[metric, "Mean"] for metric in metrics]
        stds = [subset.loc[metric, "Std"] for metric in metrics]
        offsets = positions + (index - (len(datasets) - 1) / 2) * width

        ax.bar(
            offsets,
            means,
            width=width * 0.9,
            label=dataset,
            color=INTERNAL_VALIDATION_COLORS.get(dataset, MODEL_BAR_COLOR),
            edgecolor="#2c3e50",
            linewidth=0.5,
            alpha=0.85,
            yerr=stds,
            capsize=4,
            error_kw={"ecolor": "#2c3e50", "elinewidth": 0.9},
        )

    ax.set_xticks(positions)
    ax.set_xticklabels(metrics)
    ax.set_ylim(0, 1)
    ax.set_title(
        f"Overfitting analysis: {model_name}", fontsize=12, fontweight="bold", pad=15
    )
    ax.set_xlabel(None)
    ax.set_ylabel("Score", fontsize=10, fontweight="bold", color="#2c3e50")
    ax.legend(title=None, fontsize=9, loc="upper right", frameon=True)
    _style_axes(ax)
    fig.tight_layout()

    return fig


def plot_model_roc_curves(
    curves_df: pd.DataFrame,
    auc_by_model: dict,
    title: str = "ROC curves",
    figsize=(7, 6),
    legend_loc: str = "lower right",
    colors: dict | None = None,
) -> plt.Figure:
    """Plot the external validation ROC curve of every model on one figure.

    Parameters
    ----------
    curves_df : pd.DataFrame
        Long-format curve coordinates with columns ['False Positive Rate',
        'True Positive Rate', 'Model'], as returned by save_curves_results.
    auc_by_model : dict
        Mapping of model name to its ROC-AUC, shown in the legend.
    title : str, default 'ROC curves'
        Plot title.
    figsize : tuple, default (7, 6)
        Figure size in inches.
    legend_loc : str, default 'lower right'
        Matplotlib legend location.
    colors : dict, optional
        Mapping of model name to hex color. Defaults to one color per model
        from MODEL_PALETTE (see _model_colors).

    Returns
    -------
    matplotlib.figure.Figure
        Figure containing one ROC curve per model.
    """
    model_names = list(auc_by_model)
    colors = colors if colors is not None else _model_colors(model_names)

    fig, ax = plt.subplots(figsize=figsize)

    for model in model_names:
        curve = curves_df[curves_df["Model"] == model]
        ax.plot(
            curve["False Positive Rate"],
            curve["True Positive Rate"],
            linewidth=2,
            color=colors[model],
            label=f"{model} (AUC = {auc_by_model[model]:.3f})",
        )

    ax.plot(
        [0, 1], [0, 1], linestyle="--", linewidth=1.2, color="#94a3b8", label="Random"
    )
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.set_title(title, fontsize=12, fontweight="bold", pad=15)
    ax.set_xlabel(
        "False positive rate", fontsize=10, fontweight="bold", color="#2c3e50"
    )
    ax.set_ylabel("True positive rate", fontsize=10, fontweight="bold", color="#2c3e50")
    ax.legend(loc=legend_loc, fontsize=9)
    _style_axes(ax, grid_axis="both")
    fig.tight_layout()

    return fig


def plot_model_pr_curves(
    curves_df: pd.DataFrame,
    pr_auc_by_model: dict,
    prevalence: float,
    title: str = "Precision-recall curves",
    figsize=(7, 6),
    legend_loc: str = "lower right",
    colors: dict | None = None,
) -> plt.Figure:
    """Plot the external validation precision-recall curves on one figure.

    Parameters
    ----------
    curves_df : pd.DataFrame
        Long-format curve coordinates with columns ['Recall', 'Precision',
        'Model'], as returned by save_curves_results.
    pr_auc_by_model : dict
        Mapping of model name to its PR-AUC, shown in the legend.
    prevalence : float
        Event rate of the external validation set, drawn as the no-skill
        baseline.
    title : str, default 'Precision-recall curves'
        Plot title.
    figsize : tuple, default (7, 6)
        Figure size in inches.
    legend_loc : str, default 'lower right'
        Matplotlib legend location.
    colors : dict, optional
        Mapping of model name to hex color. Defaults to one color per model
        from MODEL_PALETTE (see _model_colors).

    Returns
    -------
    matplotlib.figure.Figure
        Figure containing one precision-recall curve per model.
    """
    model_names = list(pr_auc_by_model)
    colors = colors if colors is not None else _model_colors(model_names)

    fig, ax = plt.subplots(figsize=figsize)

    for model in model_names:
        curve = curves_df[curves_df["Model"] == model]
        ax.plot(
            curve["Recall"],
            curve["Precision"],
            linewidth=2,
            color=colors[model],
            label=f"{model} (AUC = {pr_auc_by_model[model]:.3f})",
        )

    ax.axhline(
        prevalence,
        linestyle="--",
        linewidth=1.2,
        color="#94a3b8",
        label=f"No-skill baseline = {prevalence:.3f}",
    )
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.set_title(title, fontsize=12, fontweight="bold", pad=15)
    ax.set_xlabel("Recall", fontsize=10, fontweight="bold", color="#2c3e50")
    ax.set_ylabel("Precision", fontsize=10, fontweight="bold", color="#2c3e50")
    ax.legend(loc=legend_loc, fontsize=9)
    _style_axes(ax, grid_axis="both")
    fig.tight_layout()

    return fig


def plot_metric_by_model(
    auc_table: pd.DataFrame,
    metric: str,
    baseline: float | None = None,
    title: str | None = None,
    figsize=(8, 6),
) -> plt.Figure:
    """Plot one bar per model with its score on the external validation set.

    Parameters
    ----------
    auc_table : pd.DataFrame
        Table with columns ['Model', 'Metric', 'Score'], one row per model.
    metric : str
        Metric being plotted, used for the axis label.
    baseline : float, optional
        Reference value drawn as a dashed horizontal line (the best clinical
        risk score). If None, no reference line is drawn.
    title : str, optional
        Plot title. Defaults to '<metric> by model'.
    figsize : tuple, default (8, 6)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
        Bar chart figure.
    """
    table = auc_table.sort_values(by="Score", ascending=False)

    fig, ax = plt.subplots(figsize=figsize)
    bars = ax.bar(
        table["Model"].astype(str),
        table["Score"],
        color=MODEL_BAR_COLOR,
        edgecolor="#2c3e50",
        linewidth=0.5,
        alpha=0.85,
        width=0.65,
    )

    label_bars(
        ax,
        bars,
        [f"{score:.3f}" for score in table["Score"]],
        offset=0.015,
        fontsize=9.5,
        color="#2c3e50",
    )

    if baseline is not None:
        ax.axhline(
            baseline,
            linestyle="--",
            linewidth=1.4,
            color=BASELINE_COLOR,
            label=f"Best clinical risk score = {baseline:.3f}",
        )
        ax.legend(loc="upper right", fontsize=9)

    ax.set_ylim(0, 1)
    ax.set_title(
        title if title is not None else f"{metric} by model",
        fontsize=12,
        fontweight="bold",
        pad=15,
    )
    ax.set_xlabel(None)
    ax.set_ylabel(metric, fontsize=10, fontweight="bold", color="#2c3e50")
    _style_axes(ax)
    fig.tight_layout()

    return fig


def plot_modality_comparison(
    comparison_df: pd.DataFrame,
    model_name: str,
    modality_order: tuple[str, ...] = ("Clinical", "Multimodal"),
    title: str | None = None,
    figsize=(8, 6),
) -> plt.Figure:
    """Compare two modelling arms of a single model.

    Draws one group of bars per metric, with a series for each modality, so
    that the added value of the second arm can be read directly.

    Parameters
    ----------
    comparison_df : pd.DataFrame
        Long-format frame with columns ['Model', 'Metric', 'Modality', 'Score']
        restricted to a single model.
    model_name : str
        Model name shown in the title.
    modality_order : tuple of str, default ('Clinical', 'Multimodal')
        Modalities to plot, in legend and bar order.
    title : str, optional
        Plot title. Defaults to '<baseline> vs <comparison>: <model_name>'.
    figsize : tuple, default (8, 6)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
        Grouped bar chart figure.
    """
    metrics = list(dict.fromkeys(comparison_df["Metric"]))
    modalities = [
        modality
        for modality in modality_order
        if modality in set(comparison_df["Modality"])
    ]

    positions = np.arange(len(metrics), dtype=float)
    width = 0.8 / max(len(modalities), 1)

    fig, ax = plt.subplots(figsize=figsize)

    for index, modality in enumerate(modalities):
        subset = comparison_df[comparison_df["Modality"] == modality].set_index(
            "Metric"
        )
        scores = [subset.loc[metric, "Score"] for metric in metrics]
        offsets = positions + (index - (len(modalities) - 1) / 2) * width

        bars = ax.bar(
            offsets,
            scores,
            width=width * 0.9,
            label=modality,
            color=MODALITY_COLORS.get(modality, MODEL_BAR_COLOR),
            edgecolor="#2c3e50",
            linewidth=0.5,
            alpha=0.85,
        )

        label_bars(
            ax,
            bars,
            [f"{score:.3f}" for score in scores],
            offset=0.015,
            fontsize=9,
            color="#2c3e50",
        )

    ax.set_xticks(positions)
    ax.set_xticklabels(metrics)
    ax.set_ylim(0, 1)
    ax.set_title(
        title
        if title is not None
        else f"{modality_order[0]} vs {modality_order[1]}: {model_name}",
        fontsize=12,
        fontweight="bold",
        pad=15,
    )
    ax.set_xlabel(None)
    ax.set_ylabel("Score", fontsize=10, fontweight="bold", color="#2c3e50")
    ax.legend(title=None, fontsize=9, loc="upper right", frameon=True)
    _style_axes(ax)
    fig.tight_layout()

    return fig
