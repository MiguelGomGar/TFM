"""Helpers for the clinical variables review plot."""

from __future__ import annotations

import matplotlib.patches as mpatches
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

from src.config import (
    VARIABLES_REVIEW_COLOR_MAP,
    VARIABLES_REVIEW_DEFAULT_COLOR,
    VARIABLES_REVIEW_ORIGINAL_LEVELS,
    VARIABLES_REVIEW_SORT_LEVELS,
)


def prepare_clinical_variables_review_data(df: pd.DataFrame) -> pd.DataFrame:
    """Prepare clinical variables review dataframe for plotting.

    Converts the Predimar review status to categorical with proper sort and
    display order, and sorts rows for visualization.

    Parameters
    ----------
    df : pd.DataFrame
        Clinical variables review dataframe with columns ['Variable', 'Predimar', 'Scores'].

    Returns
    -------
    pd.DataFrame
        Prepared dataframe with categorical columns ordered for plotting.
    """
    prepared_df = df.copy()
    prepared_df["Predimar_sort"] = pd.Categorical(
        prepared_df["Predimar"].astype(str),
        categories=VARIABLES_REVIEW_SORT_LEVELS,
        ordered=True,
    )
    prepared_df["Predimar"] = pd.Categorical(
        prepared_df["Predimar"].astype(str),
        categories=VARIABLES_REVIEW_ORIGINAL_LEVELS,
        ordered=True,
    )
    prepared_df = prepared_df.sort_values(["Predimar_sort", "Scores"]).reset_index(
        drop=True
    )
    prepared_df["Variable"] = pd.Categorical(
        prepared_df["Variable"].astype(str),
        categories=prepared_df["Variable"].tolist(),
        ordered=True,
    )
    return prepared_df


def build_clinical_variables_review_plot(df: pd.DataFrame) -> plt.Figure:
    """Build a horizontal bar chart of clinical variables review scores.

    Parameters
    ----------
    df : pd.DataFrame
        Prepared review dataframe with columns ['Variable', 'Predimar', 'Scores'].

    Returns
    -------
    matplotlib.figure.Figure
        Bar chart showing frequency counts (Scores) for each variable,
        colored by Predimar feasibility status.
    """
    colors = [
        VARIABLES_REVIEW_COLOR_MAP.get(str(value), VARIABLES_REVIEW_DEFAULT_COLOR)
        for value in df["Predimar"]
    ]

    sns.set_style("whitegrid")
    fig, ax = plt.subplots(figsize=(9, 7), dpi=300)
    y_positions = range(len(df))
    ax.barh(
        y=y_positions,
        width=df["Scores"],
        color=colors,
        edgecolor="#2c3e50",
        height=0.75,
        linewidth=0.4,
        alpha=0.85,
    )
    ax.set_yticks(y_positions)
    ax.set_yticklabels(
        df["Variable"].astype(str), fontweight="bold", color="#334155", fontsize=9.5
    )
    max_score = int(df["Scores"].max()) if not df["Scores"].isnull().all() else 0
    ax.set_xticks(range(0, max_score + 1))
    ax.set_xlabel(
        "Frequency Count Across Reviewed Scores",
        fontweight="bold",
        color="#1e293b",
        fontsize=10,
    )
    ax.set_xlim(0, max_score + max(1, int(max_score * 0.08)))
    ax.set_title("Risk Score Factors Review", fontweight="bold", fontsize=14, pad=20)
    ax.xaxis.set_tick_params(labelcolor="#475569")
    ax.grid(axis="x", color="#e2e8f0", linewidth=0.5)
    ax.grid(axis="y", visible=False)
    ax.set_axisbelow(True)
    legend_handles = [
        mpatches.Patch(color=VARIABLES_REVIEW_COLOR_MAP[key], label=key.capitalize())
        for key in VARIABLES_REVIEW_ORIGINAL_LEVELS
    ]
    ax.legend(
        handles=legend_handles,
        title="PREDIMAR Cohort Feasibility",
        loc="lower right",
        ncol=1,
        frameon=False,
    )
    plt.setp(
        ax.get_legend().get_title(), fontsize=9.5, fontweight="bold", color="#1e293b"
    )
    for spine in ["top", "right", "left", "bottom"]:
        ax.spines[spine].set_visible(False)

    return fig
