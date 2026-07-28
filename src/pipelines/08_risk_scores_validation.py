"""Validate the clinical risk scores with ROC and PR curves."""

from pathlib import Path

from src.models.data_preprocessing import encode_target_variable
from src.models.risk_scores_validation import evaluate_risk_scores
from src.data.data_cleaning import remove_prefix_from_columns
from src.utils.io import read_parquet, save_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.visualization.risk_scores_validation import plot_pr_curves, plot_roc_curves
from src.config import TARGET_VARIABLE
from src.utils.paths import (
    RISK_SCORES_DATA_PATH,
    RISK_SCORES_METRICS_PATH,
    RISK_SCORES_PR_CURVE_PATH,
    RISK_SCORES_ROC_CURVE_PATH,
    RISK_SCORES_VALIDATION_DIR,
)

INPUT_FILE = RISK_SCORES_DATA_PATH
OUTPUT_DIR = RISK_SCORES_VALIDATION_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    # Create output directory
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # Load data
    logger.info(f"Loading data from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)
    df = remove_prefix_from_columns(df, "score_")

    # Prepare target variable
    target = encode_target_variable(df, TARGET_VARIABLE)
    prevalence = target.mean()

    # Evaluate each score
    logger.info("Evaluating risk scores...")
    score_columns = [column for column in df.columns if column != TARGET_VARIABLE]

    evaluation_df, roc_plot_data, pr_plot_data = evaluate_risk_scores(
        df, target, score_columns
    )
    save_csv(evaluation_df, RISK_SCORES_METRICS_PATH)

    logger.info("Plotting ROC curves...")
    fig_roc = plot_roc_curves(roc_plot_data)
    save_figure(fig_roc, RISK_SCORES_ROC_CURVE_PATH)

    logger.info("Plotting Precision-Recall curves...")
    fig_pr = plot_pr_curves(pr_plot_data, prevalence)
    save_figure(fig_pr, RISK_SCORES_PR_CURVE_PATH)


if __name__ == "__main__":
    main()
