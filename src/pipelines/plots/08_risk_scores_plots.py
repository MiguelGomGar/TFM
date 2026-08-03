"""Draw the risk score ROC and PR curves evaluated by pipeline 08."""

from pathlib import Path

from src.models.results_saving import load_prevalence
from src.utils.filenames import CURVES_FILE
from src.utils.io import read_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    RISK_SCORES_METRICS_PATH,
    RISK_SCORES_PR_CURVE_PATH,
    RISK_SCORES_ROC_CURVE_PATH,
    RISK_SCORES_VALIDATION_DIR,
)
from src.visualization.risk_scores_validation import plot_pr_curves, plot_roc_curves

INPUT_DIR = RISK_SCORES_VALIDATION_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    logger.info(f"Loading the risk score metrics from {RISK_SCORES_METRICS_PATH}...")
    metrics = read_csv(RISK_SCORES_METRICS_PATH).set_index("score")

    logger.info("Plotting ROC curves...")
    roc_curves = read_csv(INPUT_DIR / CURVES_FILE.format(curve="roc"))
    save_figure(
        plot_roc_curves(roc_curves, metrics["roc_auc"].to_dict()),
        RISK_SCORES_ROC_CURVE_PATH,
    )

    logger.info("Plotting Precision-Recall curves...")
    pr_curves = read_csv(INPUT_DIR / CURVES_FILE.format(curve="pr"))
    save_figure(
        plot_pr_curves(
            pr_curves,
            metrics["average_precision"].to_dict(),
            prevalence=load_prevalence(INPUT_DIR),
        ),
        RISK_SCORES_PR_CURVE_PATH,
    )


if __name__ == "__main__":
    main()
