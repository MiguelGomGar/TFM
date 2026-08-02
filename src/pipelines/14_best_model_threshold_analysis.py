"""Phase 3c: threshold sensitivity of the top three models (EN, SVM, MLP).

EN, SVM and MLP on the multimodal data are the three best-performing models
overall, ranked by test PR-AUC (see
``results/models/multimodal_data/models_metrics.csv``). Every modelling
phase elsewhere in the project reports hard metrics at the default 0.5
threshold only; this phase rescores each fitted pipeline at the
Youden-optimal cut-off instead (maximizes sensitivity + specificity - 1 on
the ROC curve).

The fitted pipelines are reloaded as-is from disk; nothing is retrained.
"""

from pathlib import Path

import pandas as pd

from src.config import BEST_MODELS, BEST_MODELS_COLORS, TARGET_VARIABLE
from src.models.best_model import analyze_model, build_threshold_metrics_table
from src.models.data_preprocessing import encode_target_variable
from src.utils.io import read_parquet, save_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    BEST_MODEL_DIR,
    CLEANED_MULTIMODAL_DATA_PATH,
    MULTIMODAL_MODELS_DIR,
)
from src.visualization.best_model import (
    plot_confusion_matrix,
    plot_threshold_metrics_comparison,
)

INPUT_FILE = CLEANED_MULTIMODAL_DATA_PATH
MODEL_SOURCE_DIR = MULTIMODAL_MODELS_DIR
OUTPUT_DIR = BEST_MODEL_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading the multimodal dataset from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)
    y = encode_target_variable(df, TARGET_VARIABLE)

    results = {
        abbreviation: analyze_model(abbreviation, MODEL_SOURCE_DIR, df, y, logger=logger)
        for abbreviation in BEST_MODELS
    }

    # ------------------------------------------------------------------
    # Save per-model results
    # ------------------------------------------------------------------
    all_threshold_rows = []
    for abbreviation, result in results.items():
        save_csv(result["metrics_table"], OUTPUT_DIR / f"threshold_metrics_{abbreviation}.csv")
        save_csv(result["cm_optimal"], OUTPUT_DIR / f"confusion_matrix_optimal_{abbreviation}.csv")

        ranking_metrics_df = pd.DataFrame(
            {
                "Metric": list(result["ranking_metrics"]),
                "Score": list(result["ranking_metrics"].values()),
            }
        )
        save_csv(ranking_metrics_df, OUTPUT_DIR / f"ranking_metrics_{abbreviation}.csv")

        all_threshold_rows.extend(result["threshold_rows"])

        figure = plot_confusion_matrix(
            result["cm_optimal"], title=f"{abbreviation} — {result['optimal_label']}"
        )
        save_figure(figure, OUTPUT_DIR / f"confusion_matrix_optimal_{abbreviation}.png")

    save_csv(pd.DataFrame(all_threshold_rows), OUTPUT_DIR / "threshold_info.csv")

    # ------------------------------------------------------------------
    # Cross-model comparison, optimal-threshold scenario only. Restricted to
    # the hard metrics the comparison was requested for (F1 is available per
    # model in threshold_metrics_{model}.csv but left out of this chart).
    # ------------------------------------------------------------------
    hard_metrics = ["Accuracy", "Precision", "Recall", "Specificity"]
    comparison_table = build_threshold_metrics_table(
        {
            abbreviation: {
                metric: results[abbreviation]["metrics_optimal"][metric] for metric in hard_metrics
            }
            for abbreviation in BEST_MODELS
        }
    )
    save_csv(comparison_table, OUTPUT_DIR / "threshold_metrics_comparison.csv")

    figure = plot_threshold_metrics_comparison(
        comparison_table,
        title="Hard metrics at the Youden-optimal threshold",
        palette=BEST_MODELS_COLORS,
    )
    save_figure(figure, OUTPUT_DIR / "threshold_metrics_comparison.png")

    logger.info(f"Top-model threshold analysis results saved to {OUTPUT_DIR}.")


if __name__ == "__main__":
    main()
