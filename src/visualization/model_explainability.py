"""Plots reporting the SHAP explainability analysis of the best models.

Every function builds and returns a matplotlib Figure; saving is left to the
caller (utils.io.save_figure), as in the other visualization modules.

Two of these plots delegate to shap's own renderers (beeswarm, waterfall),
which draw onto the active figure and return nothing, so they are called with
show=False and the current figure is captured afterwards. The remaining plots
are built directly with matplotlib, which is what allows them to color each
predictor by its block (clinical vs proteomic) — the distinction this project
cares about and that shap's built-ins have no notion of.
"""

import matplotlib

# Select a non-interactive backend before pyplot is imported, for the same
# reason as in the other visualization modules: figures are rendered in a loop.
matplotlib.use("Agg", force=True)

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import shap

from src.config import FEATURE_BLOCK_COLORS, SHAP_TOP_FEATURES
from src.visualization.model_evaluation import _style_axes


def _block_legend_handles(blocks_present) -> list:
    """Build legend handles for the feature blocks actually drawn.

    Parameters
    ----------
    blocks_present : iterable of str
        Block names present in the figure, in legend order.

    Returns
    -------
    list of matplotlib.patches.Patch
        One patch per block, colored with the project's block palette.
    """
    from matplotlib.patches import Patch

    return [
        Patch(facecolor=FEATURE_BLOCK_COLORS[block], label=block)
        for block in blocks_present
        if block in FEATURE_BLOCK_COLORS
    ]


def plot_shap_summary(
    explanation, title: str, max_display: int = SHAP_TOP_FEATURES
) -> plt.Figure:
    """Draw the SHAP beeswarm summary for one model.

    Each point is one patient. Horizontal position is that patient's SHAP value
    for the predictor, and color encodes whether their value of the predictor
    was high or low, so the plot shows magnitude *and* direction of effect at a
    glance.

    Parameters
    ----------
    explanation : shap.Explanation
        Explanation from model_explainability.compute_shap_values.
    title : str
        Figure title.
    max_display : int, default SHAP_TOP_FEATURES
        Number of predictors shown, most important first.

    Returns
    -------
    matplotlib.figure.Figure
    """
    shap.plots.beeswarm(explanation, max_display=max_display, show=False)
    figure = plt.gcf()
    figure.suptitle(title, fontsize=13, fontweight="bold")
    figure.tight_layout()
    return figure


def plot_shap_waterfall(
    explanation, case_index: int, title: str, max_display: int = 14
) -> plt.Figure:
    """Draw the SHAP waterfall explaining one individual patient.

    Shows how the model moves from its base value (what it would predict
    knowing nothing about this patient) to this patient's actual score, one
    predictor contribution at a time.

    The waterfall is preferred over shap's force plot because the modern force
    plot renders as HTML/JavaScript, which does not fit this project's
    "every figure is a 300 dpi PNG saved next to its CSV" convention.

    Parameters
    ----------
    explanation : shap.Explanation
        Explanation from model_explainability.compute_shap_values.
    case_index : int
        Positional index of the patient within the external validation set.
    title : str
        Figure title.
    max_display : int, default 14
        Number of individual contributions shown before the rest are pooled
        into a single "other features" row.

    Returns
    -------
    matplotlib.figure.Figure
    """
    shap.plots.waterfall(explanation[case_index], max_display=max_display, show=False)
    figure = plt.gcf()
    figure.suptitle(title, fontsize=12, fontweight="bold")
    figure.tight_layout()
    return figure


def plot_shap_bar(
    importance_table: pd.DataFrame,
    blocks: dict,
    title: str,
    top_n: int = SHAP_TOP_FEATURES,
    figsize=(9, 8),
) -> plt.Figure:
    """Plot mean absolute SHAP per predictor, colored by feature block.

    Parameters
    ----------
    importance_table : pd.DataFrame
        Global importance table from
        model_explainability.build_global_importance_table.
    blocks : dict
        Mapping of block name to predictor names, from
        dataframe_utils.split_feature_blocks.
    title : str
        Figure title.
    top_n : int, default SHAP_TOP_FEATURES
        Number of predictors shown, most important first.
    figsize : tuple, default (9, 8)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
    """
    block_of_feature = {
        feature: block for block, features in blocks.items() for feature in features
    }
    top = importance_table.head(top_n).iloc[::-1]
    colors = [
        FEATURE_BLOCK_COLORS.get(block_of_feature.get(feature), "#94a3b8")
        for feature in top["Feature"]
    ]

    figure, ax = plt.subplots(figsize=figsize)
    ax.barh(top["Feature"], top["Mean_Abs_SHAP"], color=colors)
    ax.set_xlabel("Mean |SHAP value|", fontsize=11, fontweight="bold")
    ax.set_title(title, fontsize=13, fontweight="bold")
    _style_axes(ax, grid_axis="x")
    ax.grid(axis="x", visible=True, color="#eaeded", linewidth=0.6)
    ax.grid(axis="y", visible=False)

    blocks_present = [
        block
        for block in blocks
        if any(block_of_feature.get(f) == block for f in top["Feature"])
    ]
    if len(blocks_present) > 1:
        ax.legend(
            handles=_block_legend_handles(blocks_present),
            frameon=False,
            loc="lower right",
        )

    figure.tight_layout()
    return figure


def plot_ranking_comparison(
    ranking_table: pd.DataFrame, top_n: int = SHAP_TOP_FEATURES, figsize=(8, 9)
) -> plt.Figure:
    """Show where the models agree and disagree on predictor importance.

    Heatmap of ranks (1 = most important) for the most important predictors on
    average. A row of similar shades means the models concur; a row that jumps
    from dark to light means one model relies on a predictor the others ignore.

    Parameters
    ----------
    ranking_table : pd.DataFrame
        Table from model_explainability.build_ranking_comparison_table.
    top_n : int, default SHAP_TOP_FEATURES
        Number of predictors shown, best mean rank first.
    figsize : tuple, default (8, 9)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
    """
    rank_columns = [
        column for column in ranking_table.columns if column.startswith("Rank_")
    ]
    top = ranking_table.head(top_n)
    matrix = top.set_index("Feature")[rank_columns]
    matrix.columns = [column.replace("Rank_", "") for column in rank_columns]

    figure, ax = plt.subplots(figsize=figsize)
    sns.heatmap(
        matrix,
        annot=True,
        fmt=".0f",
        cmap="viridis_r",
        cbar_kws={"label": "Importance rank (1 = most important)"},
        linewidths=0.5,
        linecolor="white",
        ax=ax,
    )
    ax.set_title("SHAP importance rank by model", fontsize=13, fontweight="bold")
    ax.set_ylabel("")
    ax.set_xlabel("")
    figure.tight_layout()
    return figure
