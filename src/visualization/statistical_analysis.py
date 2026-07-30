"""Statistical plots built from prepared data."""

import matplotlib
from matplotlib import patches as mpatches

# Select a non-interactive backend before pyplot is imported. The reporting
# pipelines render thousands of figures in a loop, and GUI backends retain a
# figure manager plus native resources per figure, exhausting memory mid-run.
matplotlib.use("Agg", force=True)

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

from src.config import CATEGORICAL_COLOR_MAP
from src.visualization.plot_utils import label_bars


def plot_numeric_distribution(series: pd.Series, col_name: str) -> plt.Figure:
    """Plot a histogram for a continuous variable.

    Parameters
    ----------
    series : pd.Series
        Numeric series to plot.
    col_name : str
        Column name (used for title and axis labels).

    Returns
    -------
    matplotlib.figure.Figure
        Histogram figure.
    """
    fig, ax = plt.subplots(figsize=(8, 6))
    sns.histplot(
        x=series,
        bins=30,
        color="#16a085",
        edgecolor="white",
        alpha=0.7,
        kde=False,
        ax=ax,
    )
    ax.set_title(f"Distribution of {col_name}", fontsize=12, fontweight="bold", pad=15)
    ax.set_xlabel(col_name, fontsize=10, fontweight="bold", color="#2c3e50")
    ax.set_ylabel("n", fontsize=10, fontweight="bold", color="#2c3e50")
    ax.tick_params(axis="both", labelcolor="#34495e", labelsize=10)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight("bold")
    ax.grid(visible=False)
    ax.set_axisbelow(True)
    sns.despine(ax=ax)
    fig.tight_layout()

    return fig


def plot_categorical_distribution(
    df_summary: pd.DataFrame, col_name: str
) -> plt.Figure:
    """Plot a horizontal bar chart for a categorical variable.

    Parameters
    ----------
    df_summary : pd.DataFrame
        Summary table with columns ['n', 'pct', 'pct_label'], one row per category.
    col_name : str
        Column name (used for title and axis labels).

    Returns
    -------
    matplotlib.figure.Figure
        Horizontal bar chart figure.
    """
    fig, ax = plt.subplots(figsize=(8, len(df_summary) * 0.5 + 2))
    bars = ax.barh(
        df_summary[col_name].astype(str),
        df_summary["n"],
        color="#16a085",
        edgecolor="#2c3e50",
        alpha=0.8,
        height=0.6,
        linewidth=0.5,
    )

    max_val = df_summary["n"].max()
    label_bars(
        ax,
        bars,
        df_summary["pct_label"],
        orientation="horizontal",
        offset=max_val * 0.02,
        fontsize=9.5,
        color="#2c3e50",
    )

    ax.set_xlim(0, max_val * 1.15)
    ax.set_title(f"Distribution of {col_name}", fontsize=12, fontweight="bold", pad=15)
    ax.set_xlabel("n", fontsize=10, fontweight="bold", color="#2c3e50")
    ax.set_ylabel(None)
    ax.tick_params(axis="both", labelcolor="#34495e", labelsize=10)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight("bold")
    ax.grid(visible=False)
    ax.set_axisbelow(True)
    sns.despine(ax=ax, left=True, bottom=True)
    fig.tight_layout()

    return fig


def plot_stratified_numeric_distribution(
    df_summary: pd.DataFrame, col_name: str, target_var: str
) -> plt.Figure:
    """Plot a violin plot of a numeric variable stratified by groups.

    Parameters
    ----------
    df_summary : pd.DataFrame
        Two-column dataframe with numeric feature and stratification variable.
    col_name : str
        Name of the numeric column.
    target_var : str
        Name of the categorical stratification column.

    Returns
    -------
    matplotlib.figure.Figure
        Violin plot figure.
    """
    levels = list(df_summary[target_var].cat.categories)
    palette_dict = {
        str(level): CATEGORICAL_COLOR_MAP[i % len(CATEGORICAL_COLOR_MAP)]
        for i, level in enumerate(levels)
    }

    # Use a copy with string types to avoid Seaborn categorical mapping bugs
    plot_df = df_summary.copy()
    plot_df[target_var] = plot_df[target_var].astype(str)
    string_levels = [str(lvl) for lvl in levels]

    fig, ax = plt.subplots(figsize=(8, 5))
    sns.violinplot(
        data=plot_df,
        y=col_name,
        x=target_var,
        hue=target_var,
        palette=palette_dict,
        order=string_levels,
        hue_order=string_levels,
        dodge=False,
        inner="quartile",
        cut=0,
        linewidth=1.1,
        ax=ax,
    )

    handles = [
        mpatches.Patch(color=palette_dict[str(lvl)], label=str(lvl)) for lvl in levels
    ]
    ax.legend(
        handles=handles,
        title=target_var,
        loc="center left",
        bbox_to_anchor=(1.02, 0.5),
        frameon=True,
        borderaxespad=0.0,
    )

    ax.set_title(
        f"Distribution of {col_name} stratified by {target_var}",
        fontsize=12,
        fontweight="bold",
        pad=15,
    )
    ax.set_xlabel(None)
    ax.set_ylabel(col_name, fontsize=10, fontweight="bold", color="#2c3e50")
    ax.tick_params(axis="x", labelsize=10, labelcolor="#34495e")
    ax.tick_params(axis="y", labelsize=10, labelcolor="#34495e")
    ax.grid(visible=False)
    ax.set_axisbelow(True)
    sns.despine(ax=ax)
    fig.tight_layout(rect=(0, 0, 0.82, 1))

    return fig


def plot_stratified_categorical_distribution(
    df_summary: pd.DataFrame, col_name: str, target_var: str
) -> plt.Figure | None:
    """Plot a stacked horizontal bar chart of a categorical variable by group.

    Parameters
    ----------
    df_summary : pd.DataFrame
        Cross-tabulation (rows: col_name, columns: target_var levels, values: percentages).
    col_name : str
        Name of the categorical feature.
    target_var : str
        Name of the categorical stratification variable.

    Returns
    -------
    matplotlib.figure.Figure or None
        Stacked bar chart figure, or None if col_name == target_var.
    """
    if col_name == target_var:
        return None

    palette = [
        CATEGORICAL_COLOR_MAP[i % len(CATEGORICAL_COLOR_MAP)]
        for i in range(len(df_summary.columns))
    ]

    fig, ax = plt.subplots(figsize=(9, len(df_summary) * 0.5 + 2))
    df_summary.plot(
        kind="barh",
        stacked=True,
        color=palette,
        edgecolor="#2c3e50",
        alpha=0.85,
        linewidth=0.4,
        ax=ax,
    )

    for p in ax.patches:
        width = p.get_width()
        if width > 5:
            x = p.get_x() + width / 2
            y = p.get_y() + p.get_height() / 2
            ax.text(
                x,
                y,
                f"{width:.0f}%",
                ha="center",
                va="center",
                color="white",
                fontweight="bold",
                fontsize=10,
            )

    ax.set_xlim(0, 100)
    ax.xaxis.set_major_formatter(plt.FuncFormatter(lambda val, _: f"{val:.0f}%"))
    ax.set_title(
        f"Distribution of {col_name} stratified by {target_var}",
        fontsize=12,
        fontweight="bold",
        pad=15,
    )
    ax.set_xlabel(None)
    ax.set_ylabel(None)
    ax.legend(
        title=target_var,
        loc="center left",
        bbox_to_anchor=(1.02, 0.5),
        frameon=True,
    )
    ax.tick_params(axis="both", labelcolor="#34495e", labelsize=10)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight("bold")
    ax.grid(visible=False)
    ax.set_axisbelow(True)
    sns.despine(ax=ax)
    fig.tight_layout(rect=(0, 0, 0.82, 1))

    return fig


def plot_qq(df_summary: dict, ci_level: float = 0.95) -> plt.Figure:
    """Plot a Q-Q plot with confidence bands for normality assessment.

    Parameters
    ----------
    df_summary : dict
        Dictionary from compute_qq() with keys: 'feature', 'ci_level', 'osm', 'osr',
        'slope', 'intercept', 'y_line_x', 'y_line_y', 'y_lower', 'y_upper'.
    ci_level : float, default 0.95
        Confidence level (unused; taken from df_summary['ci_level']).

    Returns
    -------
    matplotlib.figure.Figure
        Q-Q plot with diagonal reference line and confidence band.
    """
    feature = df_summary["feature"]
    ci_level = df_summary["ci_level"]

    fig, ax = plt.subplots(figsize=(7, 5))
    ax.scatter(
        df_summary["osm"],
        df_summary["osr"],
        color="#16a085",
        alpha=0.6,
        label="Observed",
    )
    ax.plot(
        df_summary["y_line_x"],
        df_summary["y_line_y"],
        color="#e74c3c",
        linestyle="-",
        linewidth=1.5,
        label="Normal Line",
    )
    ax.fill_between(
        df_summary["osm"],
        df_summary["y_lower"],
        df_summary["y_upper"],
        color="#16a085",
        alpha=0.15,
        label=f"{ci_level*100:.0f}% CI Band",
    )

    ax.set_title(f"Q-Q Plot: {feature}", fontsize=12, fontweight="bold")
    ax.set_xlabel(
        "Theoretical Quantiles (Standard Normal)", fontsize=10, color="#2c3e50"
    )
    ax.set_ylabel(f"Observed Values for {feature}", fontsize=10, color="#2c3e50")
    ax.legend(loc="upper left")
    ax.grid(color="#eaeded", linewidth=0.4)
    sns.despine(ax=ax)
    fig.tight_layout()

    return fig
