"""Phase 3c: threshold sensitivity of the top three models (EN, SVM, MLP).

EN, SVM and MLP on the multimodal data are the three best-performing models
overall, ranked by test PR-AUC (see
``results/models/multimodal_data/models_metrics.csv``). Every modelling
phase elsewhere in the project reports hard metrics at the default 0.5
threshold only; this phase rescores each fitted pipeline at the
Youden-optimal cut-off instead (maximizes sensitivity + specificity - 1 on
the ROC curve).

The fitted pipelines are reloaded as-is from disk; nothing is retrained. The
confusion matrices and the cross-model chart are drawn from the tables saved
here by src/pipelines/plots/14_best_model_plots.py.
"""

from pathlib import Path

from src.config import BEST_MODELS, TARGET_VARIABLE
from src.models.best_model import (
    analyze_model,
    build_model_comparison_table,
    build_ranking_metrics_table,
    build_threshold_info_table,
)
from src.models.data_preprocessing import encode_target_variable
from src.utils.filenames import (
    CONFUSION_MATRIX_FILE,
    RANKING_METRICS_FILE,
    THRESHOLD_COMPARISON_FILE,
    THRESHOLD_INFO_FILE,
    THRESHOLD_METRICS_FILE,
)
from src.utils.io import read_parquet, save_csv
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    BEST_MODEL_DIR,
    CLEANED_MULTIMODAL_DATA_PATH,
    MULTIMODAL_MODELS_DIR,
)

INPUT_FILE = CLEANED_MULTIMODAL_DATA_PATH
MODEL_SOURCE_DIR = MULTIMODAL_MODELS_DIR
OUTPUT_DIR = BEST_MODEL_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    """Save the threshold sensitivity of the three best models."""
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
    for abbreviation, result in results.items():
        save_csv(
            result["metrics_table"],
            OUTPUT_DIR / THRESHOLD_METRICS_FILE.format(model=abbreviation),
        )
        # Saved with the row index: it holds the observed-class labels the
        # confusion matrix figure uses for its axis ticks.
        save_csv(
            result["cm_optimal"],
            OUTPUT_DIR / CONFUSION_MATRIX_FILE.format(model=abbreviation),
            index=True,
        )

        save_csv(
            build_ranking_metrics_table(result["ranking_metrics"]),
            OUTPUT_DIR / RANKING_METRICS_FILE.format(model=abbreviation),
        )

    save_csv(
        build_threshold_info_table(results, BEST_MODELS),
        OUTPUT_DIR / THRESHOLD_INFO_FILE,
    )

    # Cross-model comparison, optimal-threshold scenario only.
    save_csv(
        build_model_comparison_table(results, BEST_MODELS),
        OUTPUT_DIR / THRESHOLD_COMPARISON_FILE,
    )

    logger.info(f"Top-model threshold analysis results saved to {OUTPUT_DIR}.")


if __name__ == "__main__":
    main()
