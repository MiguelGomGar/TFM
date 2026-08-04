"""Plots reporting the training and evaluation of the predictive models."""

import matplotlib

# Select a non-interactive backend before pyplot is imported. The modelling
# pipelines render dozens of figures in a loop, and GUI backends retain a figure
# manager plus native resources per figure, exhausting memory mid-run.
matplotlib.use("Agg", force=True)

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.patches import Patch

from src.config import (
    BASELINE_COLOR,
    FEATURE_SELECTION_COLORS,
    INTERNAL_VALIDATION_COLORS,
    MODALITY_COLORS,
    MODEL_BAR_COLOR,
)
from src.visualization.plot_utils import _model_colors, _style_axes_b, _style_axes_h, _style_axes_v


def plot_internal_validation(
    summary_df: pd.DataFrame,
    metric: str,
    title: str | None = None,
    figsize=(9, 6),
) -> plt.Figure:
    """Plot the overfitting analysis of every model on the internal folds.

    Draws one group of bars per model, with a series for the training folds
    and another for the held-out validation fold, for a single metric. Bar
    heights are the mean across folds and the error bars show the standard
    deviation.

    Parameters
    ----------
    summary_df : pd.DataFrame
        Summary with columns ['Model', 'Metric', 'Dataset', 'Mean', 'Std',
        'N_Folds'], covering every model and restricted here to a single
        metric, where Dataset is 'Train' or 'Validation'.
    metric : str
        Metric being plotted, used to filter summary_df and for the axis
        label, e.g. 'ROC-AUC'.
    title : str, optional
        Plot title. Defaults to 'Overfitting analysis: <metric>'.
    figsize : tuple, default (9, 6)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
        Grouped bar chart figure.
    """
    subset = summary_df[summary_df["Metric"] == metric]
    models = list(dict.fromkeys(subset["Model"]))
    datasets = [
        dataset
        for dataset in ("Train", "Validation")
        if dataset in set(subset["Dataset"])
    ]

    positions = np.arange(len(models), dtype=float)
    width = 0.8 / max(len(datasets), 1)

    fig, ax = plt.subplots(figsize=figsize)

    for index, dataset in enumerate(datasets):
        dataset_subset = subset[subset["Dataset"] == dataset].set_index("Model")
        means = [dataset_subset.loc[model, "Mean"] for model in models]
        stds = [dataset_subset.loc[model, "Std"] for model in models]
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
    ax.set_xticklabels(models)
    ax.set_ylim(0, 1)
    ax.set_title(
        title if title is not None else f"Overfitting analysis: {metric}",
        fontsize=12,
        fontweight="bold",
        pad=15,
    )
    ax.set_xlabel(None)
    ax.set_ylabel(metric, fontsize=10, fontweight="bold", color="#2c3e50")
    ax.legend(title=None, fontsize=9, loc="upper right", frameon=True)
    _style_axes_h(ax)
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
    _style_axes_b(ax)
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
    _style_axes_b(ax)
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
    ax.bar_label(bars, fmt="%.3f", fontsize=9, color="#2c3e50", padding=3)

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
    _style_axes_h(ax)
    fig.tight_layout()

    return fig


def plot_modality_comparison(
    comparison_df: pd.DataFrame,
    metric: str,
    modality_order: tuple[str, ...] = ("Clinical", "Multimodal"),
    title: str | None = None,
    figsize=(9, 6),
) -> plt.Figure:
    """Compare modelling arms across every model, for a single metric.

    Draws one group of bars per model, with a series for each modality, so
    that the added value of the compared arm(s) can be read model by model.

    Parameters
    ----------
    comparison_df : pd.DataFrame
        Long-format frame with columns ['Model', 'Metric', 'Modality', 'Score'],
        covering every model and restricted here to a single metric.
    metric : str
        Metric being plotted, used to filter comparison_df and for the axis
        label, e.g. 'ROC-AUC'.
    modality_order : tuple of str, default ('Clinical', 'Multimodal')
        Modalities to plot, in legend and bar order.
    title : str, optional
        Plot title. Defaults to '<metric>: <baseline> vs <comparison>'.
    figsize : tuple, default (9, 6)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
        Grouped bar chart figure.
    """
    subset = comparison_df[comparison_df["Metric"] == metric]
    models = list(dict.fromkeys(subset["Model"]))
    modalities = [
        modality for modality in modality_order if modality in set(subset["Modality"])
    ]

    positions = np.arange(len(models), dtype=float)
    width = 0.8 / max(len(modalities), 1)

    fig, ax = plt.subplots(figsize=figsize)

    for index, modality in enumerate(modalities):
        modality_subset = subset[subset["Modality"] == modality].set_index("Model")
        scores = [modality_subset.loc[model, "Score"] for model in models]
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

    ax.set_xticks(positions)
    ax.set_xticklabels(models)
    ax.set_ylim(0, 1)
    ax.set_title(
        title if title is not None else f"{metric}: " + " vs ".join(modality_order),
        fontsize=12,
        fontweight="bold",
        pad=15,
    )
    ax.set_xlabel(None)
    ax.set_ylabel(metric, fontsize=10, fontweight="bold", color="#2c3e50")
    ax.legend(
        title=None,
        fontsize=9,
        loc="upper right",
        bbox_to_anchor=(1.05, 1.0),
        frameon=True,
    )
    _style_axes_h(ax)
    fig.tight_layout()

    return fig


def plot_feature_selection_coefficients(
    coefficients: pd.DataFrame,
    title: str | None = None,
    top_n: int | None = None,
    figsize=(9, 10),
) -> plt.Figure:
    """Plot the Elastic Net coefficients of the predictors it kept.

    One horizontal bar per selected predictor, ranked by absolute coefficient
    magnitude (largest influence at the top), colored by the sign of the
    coefficient so the direction of each predictor's association is visible
    at a glance. Predictors the Elastic Net zeroed out are left off the chart
    entirely, since they carry no influence to illustrate.

    Parameters
    ----------
    coefficients : pd.DataFrame
        Coefficient table from feature_selection._build_coefficient_table,
        with columns ['Feature', 'Coefficient', 'Selected'].
    title : str, optional
        Plot title. Defaults to 'Feature selection coefficients'.
    top_n : int, optional
        Number of predictors shown, largest magnitude first. None shows every
        selected predictor.
    figsize : tuple, default (9, 10)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
        Horizontal bar chart figure.
    """
    selected = coefficients[coefficients["Selected"]]
    ranked = selected.reindex(
        selected["Coefficient"].abs().sort_values(ascending=False).index
    )
    top = ranked.head(top_n) if top_n is not None else ranked
    top = top.iloc[::-1]

    colors = [
        FEATURE_SELECTION_COLORS["Positive"]
        if coefficient >= 0
        else FEATURE_SELECTION_COLORS["Negative"]
        for coefficient in top["Coefficient"]
    ]

    fig, ax = plt.subplots(figsize=figsize)
    ax.barh(
        top["Feature"],
        top["Coefficient"],
        color=colors,
        edgecolor="#2c3e50",
        linewidth=0.5,
        alpha=0.85,
    )
    ax.axvline(0, color="#2c3e50", linewidth=0.8)

    x_max = top["Coefficient"].abs().max() if len(top) else 1.0
    ax.set_xlim(-x_max * 1.1, x_max * 1.1)

    ax.set_title(
        title if title is not None else "Feature selection coefficients",
        fontsize=12,
        fontweight="bold",
        pad=15,
    )
    ax.set_xlabel(
        "Elastic Net coefficient", fontsize=10, fontweight="bold", color="#2c3e50"
    )
    ax.set_ylabel(None)
    _style_axes_v(ax)

    ax.legend(
        handles=[
            Patch(facecolor=FEATURE_SELECTION_COLORS["Positive"], label="Positive"),
            Patch(facecolor=FEATURE_SELECTION_COLORS["Negative"], label="Negative"),
        ],
        fontsize=9,
        loc="lower right",
        frameon=True,
    )
    fig.tight_layout()

    return fig
