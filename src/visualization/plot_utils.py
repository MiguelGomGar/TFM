"""Shared helpers reused across the visualization modules."""

import pandas as pd
import seaborn as sns

from src.config import MODEL_PALETTE


def _style_axes_v(ax) -> None:
    """Style axes for horizontal barplots (ax.barh).

    Draws grid lines along the x axis, perpendicular to the horizontal bars,
    so they act as reference lines for bar length.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        Axes to style.

    Returns
    -------
    None
    """
    ax.tick_params(axis="both", labelcolor="#34495e", labelsize=10)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight("bold")
    ax.grid(axis="x", color="#eaeded", linewidth=0.6)
    ax.grid(axis="y", visible=False)
    ax.set_axisbelow(True)
    sns.despine(ax=ax)


def _style_axes_h(ax) -> None:
    """Style axes for vertical barplots (ax.bar).

    Draws grid lines along the y axis, perpendicular to the vertical bars,
    so they act as reference lines for bar height.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        Axes to style.

    Returns
    -------
    None
    """
    ax.tick_params(axis="both", labelcolor="#34495e", labelsize=10)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight("bold")
    ax.grid(axis="y", color="#eaeded", linewidth=0.6)
    ax.grid(axis="x", visible=False)
    ax.set_axisbelow(True)
    sns.despine(ax=ax)


def _style_axes_b(ax) -> None:
    """Style axes for non-barplot figures (line, scatter, violin plots...).

    Draws grid lines along both axes.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        Axes to style.

    Returns
    -------
    None
    """
    ax.tick_params(axis="both", labelcolor="#34495e", labelsize=10)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight("bold")
    ax.grid(axis="both", color="#eaeded", linewidth=0.6)
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


def category_levels(values: pd.Series) -> list:
    """List the levels of a grouping column in reporting order.

    Reads the order off the categorical dtype when it is still there, and falls
    back to the order of appearance otherwise: a CSV round-trip drops the dtype,
    and the tables saved by the computing pipelines are written in level order
    precisely so that the fallback reproduces it.

    Parameters
    ----------
    values : pd.Series
        Grouping column, categorical or plain.

    Returns
    -------
    list
        Levels, in the order they should be plotted.
    """
    if isinstance(values.dtype, pd.CategoricalDtype):
        return list(values.cat.categories)
    return list(pd.unique(values.dropna()))
