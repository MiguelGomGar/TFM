"""Validate the clinical risk scores against the observed recurrences.

Besides the summary metrics, the ROC and PR curve coordinates and the event
rate are saved so that src/pipelines/plots/08_risk_scores_plots.py can draw the
curves without re-evaluating the scores.
"""

from pathlib import Path

from src.models.data_preprocessing import encode_target_variable
from src.models.results_saving import save_curves_results, save_prevalence
from src.models.risk_scores_validation import evaluate_risk_scores
from src.data.data_cleaning import remove_prefix_from_columns
from src.utils.io import read_parquet, save_csv
from src.utils.logging_utils import setup_logger
from src.config import TARGET_VARIABLE
from src.utils.paths import (
    RISK_SCORES_DATA_PATH,
    RISK_SCORES_METRICS_PATH,
    RISK_SCORES_VALIDATION_DIR,
)

INPUT_FILE = RISK_SCORES_DATA_PATH
OUTPUT_DIR = RISK_SCORES_VALIDATION_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    """Score the published clinical risk scores on the observed recurrences."""
    # Create output directory
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # Load data
    logger.info(f"Loading data from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)
    df = remove_prefix_from_columns(df, "score_")

    # Prepare target variable
    target = encode_target_variable(df, TARGET_VARIABLE)

    # Evaluate each score
    logger.info("Evaluating risk scores...")
    score_columns = [column for column in df.columns if column != TARGET_VARIABLE]

    evaluation_df, roc_curves, pr_curves = evaluate_risk_scores(
        df, target, score_columns
    )
    save_csv(evaluation_df, RISK_SCORES_METRICS_PATH)

    # The curves are drawn elsewhere, so their coordinates and the no-skill
    # baseline they are compared against have to reach disk here.
    logger.info(f"Saving the ROC and PR curve coordinates to {OUTPUT_DIR}...")
    for curve_type, curves in (("roc", roc_curves), ("pr", pr_curves)):
        save_curves_results(
            [score for score, _, _, _ in curves],
            [x_values for _, x_values, _, _ in curves],
            [y_values for _, _, y_values, _ in curves],
            curve_type=curve_type,
            output_dir=OUTPUT_DIR,
        )

    save_prevalence(target, OUTPUT_DIR)


if __name__ == "__main__":
    main()
