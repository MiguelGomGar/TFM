"""Statistical plots built from prepared data."""

import logging
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

from src.utils.io import save_figure
from src.config import CATEGORICAL_COLOR_MAP, CATEGORICAL_DISTRIBUTION_COLOR_MAP


def plot_numeric_distribution(df_summary: pd.DataFrame, col_name: str) -> plt.Figure:
    """Render a histogram from a prepared numeric series."""

    fig, ax = plt.subplots(figsize=(8, 6))
    sns.histplot(
        x=df_summary,
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
    ax.grid(axis="y", color="#eaeded", linewidth=0.4)
    ax.set_axisbelow(True)
    sns.despine(ax=ax)
    fig.tight_layout()

    return fig


def plot_categorical_distribution(
    df_summary: pd.DataFrame, col_name: str
) -> plt.Figure:
    """Render a horizontal bar chart from categorical summary data."""

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
    for bar, label in zip(bars, df_summary["pct_label"]):
        width = bar.get_width()
        ax.text(
            width + (max_val * 0.02),
            bar.get_y() + bar.get_height() / 2,
            label,
            ha="left",
            va="center",
            fontsize=9.5,
            fontweight="bold",
            color="#2c3e50",
        )

    ax.set_xlim(0, max_val * 1.15)
    ax.set_title(f"Distribution of {col_name}", fontsize=12, fontweight="bold", pad=15)
    ax.set_xlabel("n", fontsize=10, fontweight="bold", color="#2c3e50")
    ax.set_ylabel(None)
    ax.tick_params(axis="both", labelcolor="#34495e", labelsize=10)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight("bold")
    ax.grid(axis="x", color="#eaeded", linewidth=0.4)
    ax.set_axisbelow(True)
    sns.despine(ax=ax, left=True, bottom=True)
    fig.tight_layout()

    return fig


def plot_stratified_numeric_distribution(
    df_summary: pd.DataFrame, col_name: str, target_var: str
) -> plt.Figure:
    """Render a violin plot from prepared stratified numeric data."""

    fig, ax = plt.subplots(figsize=(8, 5))
    sns.violinplot(
        data=df_summary,
        y=col_name,
        x=target_var,
        hue=target_var,
        palette=CATEGORICAL_DISTRIBUTION_COLOR_MAP,
        dodge=False,
        inner="quartile",
        cut=0,
        linewidth=1.1,
        ax=ax,
    )

    handles, labels = ax.get_legend_handles_labels()
    if handles:
        ax.legend(
            handles,
            labels,
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
    ax.grid(axis="x", color="#eaeded", linewidth=0.4)
    ax.set_axisbelow(True)
    sns.despine()
    plt.tight_layout(rect=(0, 0, 0.82, 1))

    return fig


def plot_stratified_categorical_distribution(
    df_summary: pd.DataFrame, col_name: str, target_var: str
) -> plt.Figure:
    """Render a stacked bar chart from prepared stratified categorical data."""

    if col_name == target_var:
        return None

    fig, ax = plt.subplots(figsize=(9, len(df_summary) * 0.5 + 2))
    cat_colors = [
        CATEGORICAL_DISTRIBUTION_COLOR_MAP.get(
            col, CATEGORICAL_COLOR_MAP[i % len(CATEGORICAL_COLOR_MAP)]
        )
        for i, col in enumerate(df_summary.columns)
    ]

    df_summary.plot(
        kind="barh",
        stacked=True,
        color=cat_colors,
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
    sns.despine(ax=ax)
    fig.tight_layout(rect=(0, 0, 0.82, 1))

    return fig


def plot_qq(df_summary: dict, ci_level: float = 0.95) -> plt.Figure:
    """Render a Q-Q plot from prepared quantile data."""

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
