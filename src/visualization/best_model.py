"""Plots for the best model's threshold-sensitivity analysis."""

import matplotlib

# Select a non-interactive backend before pyplot is imported, consistent with
# every other visualization module in the project.
matplotlib.use("Agg", force=True)

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from src.config import THRESHOLD_SCENARIO_COLORS
from src.visualization.model_evaluation import _style_axes



def plot_confusion_matrix(
    matrix: pd.DataFrame,
    title: str = "Confusion matrix",
    figsize=(5, 5),
) -> plt.Figure:
    """Plot a single confusion matrix heatmap for one threshold scenario.

    The percentage next to each cell's count is computed against its row
    total (i.e. the actual class), not against the overall sample size, so
    e.g. the true-positive cell shows the share of actual positives that
    were correctly identified as such.

    Parameters
    ----------
    matrix : pd.DataFrame
        2x2 confusion matrix DataFrame, as returned by
        best_model.build_confusion_matrix_table.
    title : str, default 'Confusion matrix'
        Plot title.
    figsize : tuple, default (5, 5)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
        Figure with a single heatmap.
    """
    values = matrix.to_numpy()
    row_totals = values.sum(axis=1, keepdims=True)

    fig, ax = plt.subplots(figsize=figsize)
    ax.imshow(values, cmap="Blues", vmin=0)
    ax.grid(False)

    for row in range(values.shape[0]):
        for col in range(values.shape[1]):
            count = values[row, col]
            row_total = row_totals[row, 0]
            share = 100 * count / row_total if row_total else 0.0
            text_color = "white" if count > values.max() * 0.6 else "#2c3e50"
            ax.text(
                col,
                row,
                f"{count}\n({share:.1f}%)",
                ha="center",
                va="center",
                fontsize=11,
                fontweight="bold",
                color=text_color,
            )

    ax.set_xticks(range(len(matrix.columns)))
    ax.set_xticklabels(
        [label.replace("Predicted: ", "") for label in matrix.columns],
        fontweight="bold",
    )
    ax.set_yticks(range(len(matrix.index)))
    ax.set_yticklabels(
        [label.replace("Actual: ", "") for label in matrix.index],
        fontweight="bold",
    )
    ax.set_xlabel("Predicted", fontsize=9, fontweight="bold", color="#2c3e50")
    ax.set_ylabel("Actual", fontsize=9, fontweight="bold", color="#2c3e50")
    ax.set_title(title, fontsize=12, fontweight="bold", pad=10)
    fig.tight_layout()

    return fig


def plot_threshold_metrics_comparison(
    metrics_table: pd.DataFrame,
    title: str = "Hard metrics by threshold scenario",
    figsize=(9, 6),
    palette: dict | None = None,
) -> plt.Figure:
    """Plot the hard metrics of several scenarios side by side.

    Despite the name, the grouping column ('Scenario') can hold anything the
    caller wants compared as parallel bar series — threshold scenarios, or
    (as used by the top-models comparison) model abbreviations.

    Parameters
    ----------
    metrics_table : pd.DataFrame
        Columns ['Scenario', 'Metric', 'Score'], as returned by
        best_model.build_threshold_metrics_table.
    title : str, default 'Hard metrics by threshold scenario'
        Plot title.
    figsize : tuple, default (9, 6)
        Figure size in inches.
    palette : dict, optional
        Mapping of scenario label to color. Defaults to
        THRESHOLD_SCENARIO_COLORS.

    Returns
    -------
    matplotlib.figure.Figure
        Grouped bar chart figure, one group per metric.
    """
    metrics = list(dict.fromkeys(metrics_table["Metric"]))
    scenarios = list(dict.fromkeys(metrics_table["Scenario"]))
    color_map = palette if palette is not None else THRESHOLD_SCENARIO_COLORS
    default_palette = list(color_map.values())

    positions = np.arange(len(metrics), dtype=float)
    width = 0.8 / max(len(scenarios), 1)

    fig, ax = plt.subplots(figsize=figsize)

    for index, scenario in enumerate(scenarios):
        subset = metrics_table[metrics_table["Scenario"] == scenario].set_index(
            "Metric"
        )
        scores = [
            subset.loc[metric, "Score"] if metric in subset.index else np.nan
            for metric in metrics
        ]
        offsets = positions + (index - (len(scenarios) - 1) / 2) * width

        color = color_map.get(scenario, default_palette[index % len(default_palette)])
        bars = ax.bar(
            offsets,
            scores,
            width=width * 0.9,
            label=scenario,
            color=color,
            edgecolor="#2c3e50",
            linewidth=0.5,
            alpha=0.9,
        )
        ax.bar_label(bars, fmt="%.2f", padding=2, fontsize=8, fontweight="bold")

    ax.set_xticks(positions)
    ax.set_xticklabels(metrics)
    ax.set_ylim(0, 1.08)
    ax.set_title(title, fontsize=12, fontweight="bold", pad=15)
    ax.set_xlabel(None)
    ax.set_ylabel("Score", fontsize=10, fontweight="bold", color="#2c3e50")
    ax.legend(title=None, fontsize=9, loc="upper right", frameon=True)
    _style_axes(ax)
    fig.tight_layout()

    return fig
