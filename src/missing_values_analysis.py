import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns


def plot_row_missingness(
    df,
    title="Missing Values per record",
    x_label="Amount of Missing Values (NAs)",
    y_label="Number of Records",
):
    """
    Computes the total number of missing values (NAs) per row (patient) directly
    from a raw dataframe, aggregates their frequency counts, and renders a
    professional distribution plot.
    """
    if df.empty:
        print("The dataframe has no rows. Returning None.")
        return None

    # Compute missing values per row efficiently
    row_na_count = df.isna().sum(axis=1)
    na_summary = row_na_count.value_counts().reset_index()
    na_summary.columns = ["row_na_count", "n_records"]
    na_summary = na_summary.sort_values("row_na_count")

    total_records = na_summary["n_records"].sum()
    na_summary["pct_label"] = (na_summary["n_records"] / total_records * 100).map(
        lambda x: f"{x:.1f}%"
    )

    # Plot
    plt.figure(figsize=(8, 6))
    bars = plt.bar(
        na_summary["row_na_count"],
        na_summary["n_records"],
        color="#2563eb",
        edgecolor="#1e3a8a",
        alpha=0.85,
        width=0.75,
        linewidth=0.4,
    )

    # Add the percentage text on top of the columns
    for bar, label in zip(bars, na_summary["pct_label"]):
        height = bar.get_height()
        plt.text(
            bar.get_x() + bar.get_width() / 2.0,
            height + (total_records * 0.015),
            label,
            ha="center",
            va="bottom",
            fontsize=9,
            fontweight="bold",
            color="#1e293b",
        )

    # Set x limits & integer ticks starting from 0
    max_x = na_summary["row_na_count"].max()
    plt.xticks(range(0, int(max_x) + 1))

    # Expand y limit to prevent text clipping at the top
    max_y = na_summary["n_records"].max()
    plt.ylim(0, max_y * 1.18)

    # Titles and styling
    if title:
        plt.title(title, fontsize=13, fontweight="bold", pad=20, loc="left")

    plt.xlabel(x_label, fontsize=11, fontweight="bold", color="#1e293b", labelpad=10)
    plt.ylabel(y_label, fontsize=11, fontweight="bold", color="#1e293b", labelpad=10)

    plt.yticks(fontweight="bold", color="#475569")
    plt.xticks(fontweight="bold", color="#475569")

    # Clean grid
    plt.grid(axis="y", color="#f1f5f9", linewidth=0.5)
    plt.gca().set_axisbelow(True)
    sns.despine(left=True, bottom=True)
    plt.tight_layout()

    return plt.gcf()


def plot_column_missingness(
    df,
    title="Missing Values per feature",
    x_label="Proportion of Missing Values (NAs)",
    y_label=None,
):
    """
    Calculates and plots the column-wise missingness distribution from a raw data frame.
    Saves the plot to a specified file path with the given dimensions and resolution.
    """
    # Calculate missing rate for each column
    missing_rate = df.isna().sum() / len(df)
    df_plot = pd.DataFrame(
        {"feature": missing_rate.index, "missing_rate": missing_rate.values}
    )

    # Filter out complete columns and sort
    df_plot = df_plot[df_plot["missing_rate"] > 0].sort_values("missing_rate")

    if df_plot.empty:
        print("No missing values found in the dataframe. Returning None.")
        return None

    # Plot
    plt.figure(figsize=(8, len(df_plot) * 0.35 + 2))
    bars = plt.barh(
        df_plot["feature"],
        df_plot["missing_rate"],
        color="#2563eb",
        edgecolor="#1e3a8a",
        alpha=0.85,
        height=0.75,
        linewidth=0.4,
    )

    # Threshold line at 25% missingness
    plt.axvline(x=0.25, color="#e11d48", linestyle="--", linewidth=1.2)
    plt.text(
        0.26,
        0,
        "25%",
        color="#e11d48",
        style="italic",
        fontweight="bold",
        fontsize=10,
        va="bottom",
    )

    # Label formatting as percentage with safety expansion to avoid clipping
    plt.xlim(0, max(df_plot["missing_rate"].max() * 1.15, 0.3))
    plt.gca().xaxis.set_major_formatter(plt.FuncFormatter(lambda y, _: f"{y*100:.0f}%"))

    # Titles and labels
    if title:
        plt.title(title, fontsize=13, fontweight="bold", pad=20, loc="left")

    plt.xlabel(x_label, fontsize=11, fontweight="bold", color="#1e293b", labelpad=10)
    if y_label:
        plt.ylabel(y_label, fontsize=11, fontweight="bold", color="#1e293b")

    plt.yticks(fontweight="bold", color="#475569")
    plt.xticks(fontweight="bold", color="#475569")

    # Clean grid
    plt.grid(axis="x", color="#f1f5f9", linewidth=0.5)
    plt.gca().set_axisbelow(True)
    sns.despine(left=True, bottom=True)
    plt.tight_layout()

    return plt.gcf()
