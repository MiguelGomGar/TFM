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
import numpy as np
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


def plot_shap_summary(explanation, title: str, max_display: int = SHAP_TOP_FEATURES) -> plt.Figure:
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
        block for block in blocks if any(block_of_feature.get(f) == block for f in top["Feature"])
    ]
    if len(blocks_present) > 1:
        ax.legend(handles=_block_legend_handles(blocks_present), frameon=False, loc="lower right")

    figure.tight_layout()
    return figure


def plot_block_contribution(
    block_table: pd.DataFrame, title: str, figsize=(11, 5)
) -> plt.Figure:
    """Compare the clinical and proteomic blocks across models, two ways.

    Left panel: each block's share of the model's total attribution. Right
    panel: mean absolute SHAP per predictor within the block. Both are shown
    together on purpose — the share is inflated by whichever block simply
    contributes more variables, and the per-feature mean is what corrects for
    that imbalance.

    Parameters
    ----------
    block_table : pd.DataFrame
        Concatenated block contribution tables for all models, from
        model_explainability.build_block_contribution_table.
    title : str
        Figure title.
    figsize : tuple, default (11, 5)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
    """
    models = list(dict.fromkeys(block_table["Model"]))
    blocks = list(dict.fromkeys(block_table["Block"]))
    positions = np.arange(len(models))
    width = 0.8 / max(len(blocks), 1)

    panels = [
        ("Share", "Share of total |SHAP|", "{:.0%}"),
        ("Mean_Abs_SHAP_Per_Feature", "Mean |SHAP| per predictor", "{:.3f}"),
    ]
    figure, axes = plt.subplots(1, 2, figsize=figsize)

    for ax, (column, ylabel, number_format) in zip(axes, panels):
        for index, block in enumerate(blocks):
            subset = block_table[block_table["Block"] == block].set_index("Model")
            heights = [float(subset.loc[model, column]) for model in models]
            offset = (index - (len(blocks) - 1) / 2) * width
            bars = ax.bar(
                positions + offset,
                heights,
                width,
                label=block,
                color=FEATURE_BLOCK_COLORS.get(block, "#94a3b8"),
            )
            ax.bar_label(
                bars,
                labels=[number_format.format(height) for height in heights],
                padding=2,
                fontsize=8,
                fontweight="bold",
                color="#1e293b",
            )
        ax.set_xticks(positions)
        ax.set_xticklabels(models)
        ax.set_ylabel(ylabel, fontsize=10, fontweight="bold")
        ax.margins(y=0.15)
        _style_axes(ax)

    # Annotate the number of predictors per block once, since it is the whole
    # reason the two panels can tell different stories.
    counts = block_table.drop_duplicates("Block").set_index("Block")["N_Features"]
    subtitle = ", ".join(f"{block}: {int(counts[block])} predictors" for block in blocks)
    axes[0].legend(frameon=False, loc="upper right")
    figure.suptitle(f"{title}\n{subtitle}", fontsize=12, fontweight="bold")
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
    rank_columns = [column for column in ranking_table.columns if column.startswith("Rank_")]
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


def plot_rank_correlation_heatmap(
    correlation_table: pd.DataFrame, models, figsize=(6, 5)
) -> plt.Figure:
    """Plot the pairwise Spearman agreement between the models' rankings.

    Parameters
    ----------
    correlation_table : pd.DataFrame
        Table from model_explainability.compute_rank_correlations.
    models : list of str
        Model abbreviations, in reporting order.
    figsize : tuple, default (6, 5)
        Figure size in inches.

    Returns
    -------
    matplotlib.figure.Figure
    """
    matrix = pd.DataFrame(np.eye(len(models)), index=models, columns=models)
    for _, row in correlation_table.iterrows():
        matrix.loc[row["Model_A"], row["Model_B"]] = row["Spearman_Rho"]
        matrix.loc[row["Model_B"], row["Model_A"]] = row["Spearman_Rho"]

    figure, ax = plt.subplots(figsize=figsize)
    sns.heatmap(
        matrix,
        annot=True,
        fmt=".3f",
        cmap="RdYlBu_r",
        vmin=-1,
        vmax=1,
        center=0,
        cbar_kws={"label": "Spearman rho"},
        linewidths=0.5,
        linecolor="white",
        ax=ax,
    )
    ax.set_title(
        "Agreement between models on the SHAP importance ranking",
        fontsize=12,
        fontweight="bold",
    )
    figure.tight_layout()
    return figure
