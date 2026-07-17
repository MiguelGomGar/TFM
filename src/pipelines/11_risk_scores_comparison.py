#%% Configuration
# Load packages
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd
from sklearn.metrics import (
    average_precision_score,
    precision_recall_curve,
    roc_auc_score,
    roc_curve,
)

# Set paths
project_root = Path(__file__).resolve().parents[2]
input_file = project_root / "data" / "clean" / "06clinical_data_selected.parquet"
output_dir = project_root / "results" / "risk_scores_validation"


#%% Main function
def main() -> None:
    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)

    # Load data
    print(f"Loading data from {input_file}...")
    df = pd.read_parquet(input_file)

    # Keep only the risk score columns and the target variable
    score_columns = df.filter(like="score_").columns.to_list()
    columns = score_columns + ["AF_recurrence"]
    df = df[columns].rename(columns=lambda column: column.removeprefix("score_"))

    # Prepare target variable
    target = df["AF_recurrence"].astype(str).str.strip().str.lower().map({"no": 0, "yes": 1})
    if target.isna().any():
        unexpected_values = df.loc[target.isna(), "AF_recurrence"].dropna().unique()
        raise ValueError(f"Unexpected AF_recurrence values: {unexpected_values}")

    # Evaluate each score
    print("Evaluating risk scores...")
    score_columns = [column for column in df.columns if column != "AF_recurrence"]
    evaluation_rows = []
    roc_plot_data = []
    pr_plot_data = []

    for column in score_columns:
        mask = df[column].notna() & target.notna()
        y_true = target.loc[mask].astype(int)
        y_score = df.loc[mask, column].astype(float)

        fpr, tpr, _ = roc_curve(y_true, y_score)
        precision, recall, _ = precision_recall_curve(y_true, y_score)
        roc_auc = roc_auc_score(y_true, y_score)
        average_precision = average_precision_score(y_true, y_score)

        evaluation_rows.append(
            {
                "score": column,
                "n_samples": int(mask.sum()),
                "roc_auc": roc_auc,
                "average_precision": average_precision,
            }
        )
        roc_plot_data.append((column, fpr, tpr, roc_auc))
        pr_plot_data.append((column, recall, precision, average_precision))

    evaluation_df = pd.DataFrame(evaluation_rows).sort_values("roc_auc", ascending=False)
    print(evaluation_df.to_string(index=False))
    evaluation_df.to_csv(output_dir / "risk_scores_metrics.csv", index=False)

    prevalence = target.mean()

    # ROC curve
    fig_roc, ax_roc = plt.subplots(figsize=(7, 6))
    for column, fpr, tpr, roc_auc in roc_plot_data:
        ax_roc.plot(fpr, tpr, linewidth=2, label=f"{column} (AUC = {roc_auc:.3f})")
    ax_roc.plot([0, 1], [0, 1], linestyle="--", color="grey", label="Random")
    ax_roc.set_title("ROC curves for risk scores")
    ax_roc.set_xlabel("False positive rate")
    ax_roc.set_ylabel("True positive rate")
    ax_roc.legend(loc="lower right", fontsize=9)
    ax_roc.grid(alpha=0.3)
    fig_roc.tight_layout()
    fig_roc.savefig(output_dir / "risk_scores_roc_curve.png", dpi=300, bbox_inches="tight")
    plt.close(fig_roc)

    # Precision-recall curve
    fig_pr, ax_pr = plt.subplots(figsize=(7, 6))
    for column, recall, precision, average_precision in pr_plot_data:
        ax_pr.plot(recall, precision, linewidth=2, label=f"{column} (AP = {average_precision:.3f})")
    ax_pr.axhline(prevalence, linestyle="--", color="grey", label=f"No-skill baseline = {prevalence:.3f}")
    ax_pr.set_title("Precision-recall curves for risk scores")
    ax_pr.set_xlabel("Recall")
    ax_pr.set_ylabel("Precision")
    ax_pr.legend(loc="lower right", fontsize=9)
    ax_pr.grid(alpha=0.3)
    fig_pr.tight_layout()
    fig_pr.savefig(output_dir / "risk_scores_precision_recall_curve.png", dpi=300, bbox_inches="tight")
    plt.close(fig_pr)

    print(f"Saved metrics to {output_dir / 'risk_scores_metrics.csv'}")
    print(f"Saved ROC curve to {output_dir / 'risk_scores_roc_curve.png'}")
    print(f"Saved precision-recall curve to {output_dir / 'risk_scores_precision_recall_curve.png'}")


if __name__ == "__main__":
    main()
