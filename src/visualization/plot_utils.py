"""Shared helpers reused across the visualization modules."""

from collections.abc import Iterable

import matplotlib.pyplot as plt
import pandas as pd


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


def label_bars(
    ax: plt.Axes,
    bars: Iterable,
    labels: Iterable,
    orientation: str = "vertical",
    offset: float = 0.0,
    fontsize: float = 9,
    color: str = "#1e293b",
) -> None:
    """Annotate each bar with a text label placed just past its end.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        Axes the bars were drawn on.
    bars : iterable of matplotlib.patches.Rectangle
        Bar patches, as returned by ax.bar / ax.barh.
    labels : iterable
        One label per bar, in the same order as `bars`.
    orientation : {'vertical', 'horizontal'}, default 'vertical'
        'vertical' labels above bars from ax.bar; 'horizontal' labels to the
        right of bars from ax.barh.
    offset : float, default 0.0
        Gap between the bar end and the label, in data units.
    fontsize : float, default 9
        Label font size.
    color : str, default '#1e293b'
        Label font color.

    Returns
    -------
    None
    """
    for bar, label in zip(bars, labels):
        if orientation == "horizontal":
            ax.text(
                bar.get_width() + offset,
                bar.get_y() + bar.get_height() / 2.0,
                label,
                ha="left",
                va="center",
                fontsize=fontsize,
                fontweight="bold",
                color=color,
            )
        else:
            ax.text(
                bar.get_x() + bar.get_width() / 2.0,
                bar.get_height() + offset,
                label,
                ha="center",
                va="bottom",
                fontsize=fontsize,
                fontweight="bold",
                color=color,
            )
