"""Collinearity and correlation diagnostic plots."""

from matplotlib.colors import LinearSegmentedColormap
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns


def plot_corr_matrix(
    matrix_data: pd.DataFrame,
    dtype: str = "Numeric",
    threshold: float = 0.5,
    axis_ticks_size: int = 14,
):
    """Plot a lower-triangular correlation heatmap."""

    if dtype == "Numeric":
        if matrix_data.empty:
            return None
        vmin, vmax = -1.0, 1.0
        cmap = LinearSegmentedColormap.from_list(
            "custom_num", ["#3182bd", "white", "#de2d26"]
        )
        cbar_label = "s"
    elif dtype == "Categorical":
        vmin, vmax = 0.0, 1.0
        cmap = LinearSegmentedColormap.from_list(
            "custom_cat", ["#fff5f5", "#fecaca", "#ef4444", "#7f1d1d"]
        )
        cbar_label = "V"
    else:
        raise ValueError(
            "Invalid dtype parameter. Please specify either 'Numeric' or 'Categorical'."
        )

    mask = np.triu(np.ones_like(matrix_data, dtype=bool))

    fig, ax = plt.subplots(figsize=(10, 8))

    annot_mask = (np.abs(matrix_data) >= threshold) & (~mask)
    annot_labels = matrix_data.map(lambda v: f"{v:.1f}" if not pd.isna(v) else "")
    annot_labels = np.where(annot_mask, annot_labels, "")

    sns.heatmap(
        matrix_data,
        mask=mask,
        vmin=vmin,
        vmax=vmax,
        cmap=cmap,
        annot=annot_labels,
        fmt="",
        annot_kws={"size": 10, "weight": "bold"},
        linewidths=0.4,
        linecolor="#e2e8f0",
        square=True,
        cbar_kws={"label": cbar_label, "shrink": 0.8},
        ax=ax,
    )

    for text in ax.texts:
        if text.get_text():
            try:
                val = float(text.get_text())
                if abs(val) > threshold:
                    text.set_color("white")
                else:
                    text.set_color("#2c3e50")
            except ValueError:
                text.set_color("#2c3e50")

    ax.set_title(
        f"{dtype} Correlation Matrix",
        fontsize=16,
        fontweight="bold",
        pad=15,
        loc="left",
    )
    ax.set_xticklabels(
        ax.get_xticklabels(),
        rotation=45,
        ha="right",
        fontsize=axis_ticks_size,
        fontweight="bold",
        color="#34495e",
    )
    ax.set_yticklabels(
        ax.get_yticklabels(),
        rotation=45,
        va="top",
        fontsize=axis_ticks_size,
        fontweight="bold",
        color="#34495e",
    )
    fig.tight_layout()
    return fig


def plot_vif(
    vif_data: pd.Series,
    threshold: float = 5.0,
    title: str = "VIF Diagnostics",
    x_label: str = "VIF / GVIF^2",
    y_label: str = None,
):
    """Render a VIF/GVIF bar chart from prepared values."""

    if vif_data is None or vif_data.empty:
        return None

    fig, ax = plt.subplots(figsize=(10, max(6, len(vif_data) * 0.35)))
    sns.barplot(x=vif_data.values, y=vif_data.index, color="#2563eb", ax=ax)
    ax.axvline(threshold, linestyle="--", color="#e11d48", linewidth=1.2)
    ax.set_title(title, fontsize=13, fontweight="bold", pad=20, loc="left")
    ax.set_xlabel(x_label, fontsize=11, fontweight="bold", color="#1e293b", labelpad=10)
    if y_label:
        ax.set_ylabel(y_label, fontsize=11, fontweight="bold", color="#1e293b")
    fig.tight_layout()

    return fig
