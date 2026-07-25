"""Missing-value diagnostic plots."""

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

from src.config import MISSING_RATE_THRESHOLD


def plot_row_missingness(
    na_summary: pd.DataFrame,
    title: str = "Missing Values per record",
    x_label: str = "Amount of Missing Values (NAs)",
    y_label: str = "Number of Records",
):
    """Render a row missingness bar chart from prepared summary data."""

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

    ax.grid(axis="y", color="#f1f5f9", linewidth=0.5)
    ax.set_axisbelow(True)
    sns.despine(ax=ax, left=True, bottom=True)
    fig.tight_layout()

    return fig


def plot_column_missingness(
    na_summary: pd.DataFrame,
    title: str = "Missing Values per feature",
    threshold: float | None = None,
    x_label: str = "Proportion of Missing Values (NAs)",
    y_label: str = None,
):
    """Render a column missingness bar chart from prepared summary data."""

    if na_summary is None or na_summary.empty:
        return None

    fig, ax = plt.subplots(figsize=(8, len(na_summary) * 0.35 + 2))
    bars = ax.barh(
        na_summary["feature"],
        na_summary["missing_rate"],
        color="#2563eb",
        edgecolor="#1e3a8a",
        alpha=0.85,
        height=0.75,
        linewidth=0.4,
    )

    if threshold is not None:
        ax.axvline(x=threshold, color="#e11d48", linestyle="--", linewidth=1.2)
        ax.text(
            threshold,
            0,
            f"{threshold*100:.0f}%",
            color="#e11d48",
            style="italic",
            fontweight="bold",
            fontsize=10,
            va="bottom",
        )

    ax.set_xlim(0, max(na_summary["missing_rate"].max() * 1.15, 0.3))
    ax.xaxis.set_major_formatter(plt.FuncFormatter(lambda y, _: f"{y*100:.0f}%"))

    if title:
        ax.set_title(title, fontsize=13, fontweight="bold", pad=20, loc="left")

    ax.set_xlabel(x_label, fontsize=11, fontweight="bold", color="#1e293b", labelpad=10)
    if y_label:
        ax.set_ylabel(y_label, fontsize=11, fontweight="bold", color="#1e293b")

    ax.tick_params(axis="both", labelcolor="#475569", labelsize=10)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight("bold")

    ax.grid(axis="x", color="#f1f5f9", linewidth=0.5)
    ax.set_axisbelow(True)
    sns.despine(ax=ax, left=True, bottom=True)
    fig.tight_layout()

    return fig
