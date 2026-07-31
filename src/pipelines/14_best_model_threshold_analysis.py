"""Phase 3c: threshold sensitivity of the best-performing model (EN).

EN on the multimodal data is the best-performing model overall (see
``results/models/multimodal_data/models_metrics.csv``). Every modelling
phase elsewhere in the project reports hard metrics at the default 0.5
threshold only; this phase scores the same fitted EN pipeline under two
decision rules:

1. Default: probability > 0.5.
2. Optimal: the Youden-optimal cut-off (maximizes sensitivity + specificity
- 1 on the ROC curve).

The fitted pipeline is reloaded as-is from disk; nothing is retrained.
"""

from pathlib import Path

import pandas as pd
from sklearn.metrics import average_precision_score, roc_auc_score
from sklearn.model_selection import train_test_split

from src.config import BEST_MODEL, SEED, TARGET_VARIABLE, TEST_SIZE
from src.models.best_model import (
    apply_threshold,
    build_confusion_matrix_table,
    build_threshold_metrics_table,
    compute_youden_threshold,
    get_model_feature_columns,
    get_positive_class_probability,
    load_fitted_pipeline,
)
from src.models.data_preprocessing import encode_target_variable
from src.models.model_evaluation import compute_hard_metrics
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

    logger.info(f"Loading the fitted {BEST_MODEL} pipeline from {MODEL_SOURCE_DIR}...")
    model = load_fitted_pipeline(MODEL_SOURCE_DIR, BEST_MODEL)
    feature_columns = get_model_feature_columns(model)

    X = df[feature_columns]
    _, X_test, _, y_test = train_test_split(
        X, y, test_size=TEST_SIZE, random_state=SEED, shuffle=True, stratify=y
    )
    logger.info(
        f"Reconstructed the external validation set: {X_test.shape[0]} patients "
        f"({y_test.mean():.1%} recurrence)."
    )

    y_prob = get_positive_class_probability(model, X_test)

    # Threshold-independent ranking performance, kept only as context: it is
    # identical across the three scenarios below, since it does not depend
    # on where the cut-off is drawn.
    ranking_metrics = {
        "ROC-AUC": roc_auc_score(y_test, y_prob),
        "PR-AUC": average_precision_score(y_test, y_prob),
    }

    # ------------------------------------------------------------------
    # 1. Default threshold (0.5)
    # ------------------------------------------------------------------
    y_pred_default = model.predict(X_test)
    metrics_default = compute_hard_metrics(y_test, y_pred_default)
    cm_default = build_confusion_matrix_table(y_test, y_pred_default)
    logger.info(f"Default threshold (0.5): {metrics_default}")

    # ------------------------------------------------------------------
    # 2. Youden-optimal threshold
    # ------------------------------------------------------------------
    youden = compute_youden_threshold(y_test, y_prob)
    y_pred_optimal = apply_threshold(y_prob, youden["threshold"])
    metrics_optimal = compute_hard_metrics(y_test, y_pred_optimal)
    cm_optimal = build_confusion_matrix_table(y_test, y_pred_optimal)
    logger.info(
        f"Youden-optimal threshold: {youden['threshold']:.3f} "
        f"(J = {youden['youden_j']:.3f}); metrics: {metrics_optimal}"
    )

    # ------------------------------------------------------------------
    # Save results
    # ------------------------------------------------------------------
    default_label = "Default (p > 0.5)"
    optimal_label = f"Optimal (Youden, p > {youden['threshold']:.3f})"

    metrics_table = build_threshold_metrics_table(
        {
            default_label: metrics_default,
            optimal_label: metrics_optimal,
        }
    )
    save_csv(metrics_table, OUTPUT_DIR / "threshold_metrics.csv")

    save_csv(cm_default, OUTPUT_DIR / "confusion_matrix_default.csv")
    save_csv(cm_optimal, OUTPUT_DIR / "confusion_matrix_optimal.csv")

    threshold_info = pd.DataFrame(
        [
            {
                "Scenario": default_label,
                "Threshold": 0.5,
                "Method": "Fixed",
                "N_Test": int(X_test.shape[0]),
                "N_Scored": int(X_test.shape[0]),
            },
            {
                "Scenario": optimal_label,
                "Threshold": youden["threshold"],
                "Method": f"Youden's J = {youden['youden_j']:.3f} "
                f"(sensitivity = {metrics_optimal['Recall']:.3f}, "
                f"specificity = {metrics_optimal['Specificity']:.3f})",
                "N_Test": int(X_test.shape[0]),
                "N_Scored": int(X_test.shape[0]),
            },
        ]
    )
    save_csv(threshold_info, OUTPUT_DIR / "threshold_info.csv")

    ranking_metrics_df = pd.DataFrame(
        {"Metric": list(ranking_metrics), "Score": list(ranking_metrics.values())}
    )
    save_csv(ranking_metrics_df, OUTPUT_DIR / "ranking_metrics.csv")

    for scenario_label, matrix, file_stem in (
        (default_label, cm_default, "confusion_matrix_default"),
        (optimal_label, cm_optimal, "confusion_matrix_optimal"),
    ):
        figure = plot_confusion_matrix(matrix, title=scenario_label)
        save_figure(figure, OUTPUT_DIR / f"{file_stem}.png")

    figure = plot_threshold_metrics_comparison(metrics_table)
    save_figure(figure, OUTPUT_DIR / "threshold_metrics.png")

    logger.info(f"Best-model threshold analysis results saved to {OUTPUT_DIR}.")


if __name__ == "__main__":
    main()
