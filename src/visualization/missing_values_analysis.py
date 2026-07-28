"""Missing-value diagnostic plots."""

import pandas as pd
import numpy as np
import matplotlib

# Select a non-interactive backend before pyplot is imported (see io.save_figure).
matplotlib.use("Agg", force=True)

import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
import seaborn as sns

from src.config import CATEGORICAL_COLOR_MAP


def plot_row_missingness(
    na_summary: pd.DataFrame,
    title: str = "Missing Values per record",
    x_label: str = "Amount of Missing Values",
    y_label: str = "Number of Records",
):
    """Plot a bar chart of records by number of missing values.

    Parameters
    ----------
    na_summary : pd.DataFrame
        Summary table with columns ['row_na_count', 'n_records', 'pct_label'].
    title : str, default 'Missing Values per record'
        Plot title.
    x_label : str, default 'Amount of Missing Values'
        X-axis label.
    y_label : str, default 'Number of Records'
        Y-axis label.

    Returns
    -------
    matplotlib.figure.Figure or None
        Bar chart figure, or None if na_summary is None or empty.
    """
    if na_summary is None or na_summary.empty:
        return None

    total_records = na_summary["n_records"].sum()

    fig, ax = plt.subplots(figsize=(8, 6))
    bars = ax.bar(
        na_summary["row_na_count"],
        na_summary["n_records"],
        color="#2563eb",
        edgecolor="#1e3a8a",
        alpha=0.85,
        width=0.75,
        linewidth=0.4,
    )

    for bar, label in zip(bars, na_summary["pct_label"]):
        height = bar.get_height()
        ax.text(
            bar.get_x() + bar.get_width() / 2.0,
            height + (total_records * 0.015),
            label,
            ha="center",
            va="bottom",
            fontsize=9,
            fontweight="bold",
            color="#1e293b",
        )

    max_x = na_summary["row_na_count"].max()
    ax.set_xticks(range(0, int(max_x) + 1))

    max_y = na_summary["n_records"].max()
    ax.set_ylim(0, max_y * 1.18)

    if title:
        ax.set_title(title, fontsize=13, fontweight="bold", pad=20, loc="left")

    ax.set_xlabel(x_label, fontsize=11, fontweight="bold", color="#1e293b", labelpad=10)
    ax.set_ylabel(y_label, fontsize=11, fontweight="bold", color="#1e293b", labelpad=10)

    ax.tick_params(axis="both", labelcolor="#475569", labelsize=10)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight("bold")

    ax.grid(axis="x", color="#f1f5f9", linewidth=0.5)
    ax.set_axisbelow(True)
    sns.despine(ax=ax, left=True, bottom=True)
    fig.tight_layout()

    return fig


def plot_column_missingness(
    na_summary: pd.DataFrame,
    title: str = "Missing Values per feature",
    threshold: float | None = None,
    x_label: str = "Number of Missing Values",
    y_label: str = None,
):
    """Plot a horizontal bar chart of missing value counts per feature.

    Parameters
    ----------
    na_summary : pd.DataFrame
        Summary table with columns ['feature', 'missing_count', 'missing_rate'].
    title : str, default 'Missing Values per feature'
        Plot title.
    threshold : float, optional
        Threshold line to draw (e.g., maximum acceptable missing rate).
    x_label : str, default 'Number of Missing Values'
        X-axis label.
    y_label : str, optional
        Y-axis label. If None, no label is set.

    Returns
    -------
    matplotlib.figure.Figure or None
        Horizontal bar chart figure, or None if na_summary is None or empty.
    """
    if na_summary is None or na_summary.empty:
        return None

    if "missing_count" in na_summary.columns:
        counts = na_summary["missing_count"]
    else:
        counts = na_summary["missing_rate"]

    rates = (
        na_summary["missing_rate"] if "missing_rate" in na_summary.columns else counts
    )

    fig, ax = plt.subplots(figsize=(8, len(na_summary) * 0.35 + 2))
    bars = ax.barh(
        na_summary["feature"],
        counts,
        color="#2563eb",
        edgecolor="#1e3a8a",
        alpha=0.85,
        height=0.75,
        linewidth=0.4,
    )

    max_count = counts.max()
    offset = max_count * 0.015 if max_count > 0 else 0.1

    for bar, rate in zip(bars, rates):
        width = bar.get_width()
        y_pos = bar.get_y() + bar.get_height() / 2.0
        ax.text(
            width + offset,
            y_pos,
            f"{rate * 100:.1f}%",
            va="center",
            ha="left",
            fontsize=9,
            fontweight="bold",
            color="#1e293b",
        )

    if threshold is not None:
        if threshold <= 1.0 and (rates > 0).any():
            valid_mask = rates > 0
            total_records = (counts[valid_mask] / rates[valid_mask]).median()
            threshold_x = threshold * total_records
        else:
            threshold_x = threshold

        ax.axvline(x=threshold_x, color="#e11d48", linestyle="--", linewidth=1.2)
        ax.text(
            threshold_x,
            0,
            f"{threshold*100:.0f}%" if threshold <= 1.0 else f"{threshold}",
            color="#e11d48",
            style="italic",
            fontweight="bold",
            fontsize=10,
            va="bottom",
            ha="left",
        )

    ax.set_xlim(0, max_count * 1.18 if max_count > 0 else 1)

    if title:
        ax.set_title(title, fontsize=13, fontweight="bold", pad=20, loc="left")

    ax.set_xlabel(x_label, fontsize=11, fontweight="bold", color="#1e293b", labelpad=10)
    if y_label:
        ax.set_ylabel(y_label, fontsize=11, fontweight="bold", color="#1e293b")

    ax.tick_params(axis="both", labelcolor="#475569", labelsize=10)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight("bold")

    ax.grid(axis="y", color="#f1f5f9", linewidth=0.5)
    ax.set_axisbelow(True)
    sns.despine(ax=ax, left=True, bottom=True)
    fig.tight_layout()

    return fig


def plot_missingness_heatmap(
    na_matrix: pd.DataFrame,
    title: str = "Missing Values Heatmap",
    x_label: str = "Features",
    y_label: str = "Records",
):
    """Plot a heatmap showing missing value patterns across records and features.

    Parameters
    ----------
    na_matrix : pd.DataFrame
        Boolean matrix (rows: records, columns: features, True = NaN).
    title : str, default 'Missing Values Heatmap'
        Plot title.
    x_label : str, default 'Features'
        X-axis label.
    y_label : str, default 'Records'
        Y-axis label.

    Returns
    -------
    matplotlib.figure.Figure or None
        Heatmap figure, or None if na_matrix is None or empty.
    """
    if na_matrix is None or na_matrix.empty:
        return None

    fig, ax = plt.subplots(figsize=(10, 6))

    cmap = ListedColormap(["#f1f5f9", "#eb2525"])

    matrix_values = na_matrix.astype(int).to_numpy()
    ax.imshow(matrix_values, aspect="auto", cmap=cmap, interpolation="none")

    ax.set_xticks(np.arange(na_matrix.shape[1]) + 0.5)
    ax.set_xticklabels(labels=[])
    ax.set_yticks([])

    ax.set_xlabel(x_label, fontsize=11, fontweight="bold", color="#1e293b", labelpad=10)
    ax.set_ylabel(y_label, fontsize=11, fontweight="bold", color="#1e293b", labelpad=10)

    if title:
        ax.set_title(title, fontsize=13, fontweight="bold", pad=20, loc="left")

    ax.grid(axis="x", color="#000000", linewidth=0.5)
    fig.tight_layout()
    return fig


def plot_stratified_missingness(
    stratified_na: pd.DataFrame,
    stratify_col_name: str,
    title: str = "Missing Values Stratified by AF Recurrence",
    x_label: str = "Proportion of Missing Values",
    y_label: str = None,
):
    """Plot a stacked horizontal bar chart of feature missingness by group.

    Parameters
    ----------
    stratified_na : pd.DataFrame
        Missing value counts per feature per group (rows: features, columns: groups).
    stratify_col_name : str
        Name of the stratification variable (used in legend).
    title : str
        Plot title.
    x_label : str, default 'Proportion of Missing Values'
        X-axis label.
    y_label : str, optional
        Y-axis label. If None, no label is set.

    Returns
    -------
    matplotlib.figure.Figure or None
        Stacked bar chart figure, or None if stratified_na is None or empty.
    """
    if stratified_na is None or stratified_na.empty:
        return None

    row_totals = stratified_na.sum(axis=1)
    stratified_na_pct = stratified_na.div(row_totals, axis=0).fillna(0) * 100

    fig, ax = plt.subplots(figsize=(8, max(5, len(stratified_na_pct) * 0.35 + 1.5)))

    cat_colors = [
        CATEGORICAL_COLOR_MAP[i % len(CATEGORICAL_COLOR_MAP)]
        for i, col in enumerate(stratified_na_pct.columns)
    ]

    stratified_na_pct.plot(
        kind="barh",
        stacked=True,
        color=cat_colors,
        edgecolor="#1e3a8a",
        linewidth=0.4,
        width=0.75,
        ax=ax,
    )

    ax.set_xlim(0, 100)
    ax.xaxis.set_major_formatter(plt.FuncFormatter(lambda x, _: f"{x:.0f}%"))

    # Add percentage labels inside each stacked bar segment
    # Each label shows the proportion of missing values in that subgroup
    # relative to the total missing values for that feature (row total)
    for i, (_, row) in enumerate(stratified_na_pct.iterrows()):
        cumsum = 0
        for j, val in enumerate(row):
            if val > 0:
                ax.text(
                    cumsum + val / 2,
                    i,
                    f"{val:.1f}%",
                    ha="center",
                    va="center",
                    fontsize=8,
                    fontweight="bold",
                    color="white",
                )
            cumsum += val

    ax.set_yticklabels(
        stratified_na_pct.index, fontsize=10, fontweight="bold", color="#475569"
    )

    if title:
        ax.set_title(title, fontsize=13, fontweight="bold", pad=20, loc="left")

    ax.set_xlabel(x_label, fontsize=11, fontweight="bold", color="#1e293b", labelpad=10)
    if y_label:
        ax.set_ylabel(y_label, fontsize=11, fontweight="bold", color="#1e293b")

    ax.legend(
        title=stratify_col_name,
        title_fontsize="10",
        fontsize="9",
        frameon=True,
        facecolor="#f8fafc",
        edgecolor="#cbd5e1",
        loc="upper left",
        bbox_to_anchor=(1, 1),
    )

    ax.tick_params(axis="both", labelcolor="#475569", labelsize=10)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight("bold")

    ax.grid(axis="x", color="#f1f5f9", linewidth=0.5)
    ax.grid(axis="y", visible=False)
    ax.set_axisbelow(True)
    sns.despine(ax=ax, left=True, bottom=True)
    fig.tight_layout()

    return fig
