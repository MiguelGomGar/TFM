# %% Setup
print("Setting up paths and loading modules...")

# Set paths
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent
INPUT_FILE = PROJECT_ROOT / "data" / "raw" / "clinical_variables_review.xlsx"
OUTPUT_FILE = (
    PROJECT_ROOT / "results" / "data_collection" / "clinical_variables_review_plot.png"
)

# Load modules
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import seaborn as sns


# %% Main
def main():
    print(f"Loading data from {INPUT_FILE}...")
    # Assumes header in first row
    df = pd.read_excel(INPUT_FILE, usecols="A:C", nrows=27)
    df.columns = df.columns.str.strip()  # clean column names

    # Ensure expected columns exist
    expected = {"Variable", "Predimar", "Scores"}
    if not expected.issubset(set(df.columns)):
        raise ValueError(f"Expected columns {expected}, got {set(df.columns)}")

    print("Rearranging factor levels for plotting...")
    # Define category orders
    predimar_sort_levels = ["no", "disaggregated", "related", "yes"]
    predimar_original_levels = ["yes", "related", "disaggregated", "no"]

    # Create sorting categorical and original categorical for consistent plotting/legend
    df["Predimar_sort"] = pd.Categorical(
        df["Predimar"].astype(str), categories=predimar_sort_levels, ordered=True
    )
    df["Predimar"] = pd.Categorical(
        df["Predimar"].astype(str), categories=predimar_original_levels, ordered=True
    )

    # Sort by Predimar_sort then Scores (ascending)
    df = df.sort_values(["Predimar_sort", "Scores"]).reset_index(drop=True)

    # Freeze the Variable order to the current row order
    df["Variable"] = pd.Categorical(
        df["Variable"].astype(str), categories=df["Variable"].tolist(), ordered=True
    )

    # Color mapping
    color_map = {
        "yes": "#10b981",
        "related": "#f59e0b",
        "disaggregated": "#3b82f6",
        "no": "#ef4444",
    }
    colors = [color_map.get(str(x), "#999999") for x in df["Predimar"]]

    print("Generating the plot...")
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
    ax.set_xlim(
        0, max_score + max(1, int(max_score * 0.08))
    )  # leave room for text labels
    ax.set_title("Risk Score Factors Review", fontweight="bold", fontsize=14, pad=20)
    ax.xaxis.set_tick_params(labelcolor="#475569")
    ax.grid(axis="x", color="#e2e8f0", linewidth=0.5)
    ax.grid(axis="y", visible=False)
    ax.set_axisbelow(True)
    patches = [
        mpatches.Patch(color=color_map[k], label=k.capitalize())
        for k in predimar_original_levels
    ]
    ax.legend(
        handles=patches,
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

    # Ensure output directory exists
    OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)

    print(f"Saving the plot to {OUTPUT_FILE}...")
    plt.tight_layout()
    fig.savefig(OUTPUT_FILE, dpi=300)
    plt.close(fig)


if __name__ == "__main__":
    main()
