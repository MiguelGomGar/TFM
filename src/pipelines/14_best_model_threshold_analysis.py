"""Phase 3c: threshold sensitivity of the top three models (EN, SVM, MLP).

EN, SVM and MLP on the multimodal data are the three best-performing models
overall, ranked by test PR-AUC (see
``results/models/multimodal_data/models_metrics.csv``). Every modelling
phase elsewhere in the project reports hard metrics at the default 0.5
threshold only; this phase scores each fitted pipeline under two decision
rules:

1. Default: probability > 0.5 (or, for SVM, whose fitted pipeline has no
   probability calibration, the decision_function boundary at 0).
2. Optimal: the Youden-optimal cut-off (maximizes sensitivity + specificity
- 1 on the ROC curve).

The fitted pipelines are reloaded as-is from disk; nothing is retrained.
"""

from pathlib import Path

import pandas as pd
from sklearn.metrics import average_precision_score, roc_auc_score
from sklearn.model_selection import train_test_split

from src.config import BEST_MODELS, BEST_MODELS_COLORS, SEED, TARGET_VARIABLE, TEST_SIZE
from src.models.best_model import (
    apply_threshold,
    build_confusion_matrix_table,
    build_threshold_metrics_table,
    compute_youden_threshold,
    get_model_feature_columns,
    load_fitted_pipeline,
)
from src.models.data_preprocessing import encode_target_variable
from src.models.model_evaluation import compute_hard_metrics, get_decision_scores
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


# %% Per-model analysis
def _analyze_model(abbreviation: str, df: pd.DataFrame, y: pd.Series) -> dict:
    """Score one fitted pipeline under the default and Youden-optimal thresholds.

    Parameters
    ----------
    abbreviation : str
        Model abbreviation, matching the ``optimized_{abbreviation}.joblib``
        filename saved by the multimodal modelling phase.
    df : pd.DataFrame
        Cleaned multimodal dataset (predictors + target columns).
    y : pd.Series
        Encoded binary target, aligned to df's index.

    Returns
    -------
    dict
        Everything the caller needs to save this model's results and feed
        the cross-model comparison chart: 'ranking_metrics', 'metrics_table',
        'metrics_default', 'metrics_optimal', 'cm_default', 'cm_optimal',
        'threshold_rows' (list of dicts for the combined threshold_info
        table), 'default_label', 'optimal_label'.
    """
    logger.info(f"Loading the fitted {abbreviation} pipeline from {MODEL_SOURCE_DIR}...")
    model = load_fitted_pipeline(MODEL_SOURCE_DIR, abbreviation)
    feature_columns = get_model_feature_columns(model)

    X = df[feature_columns]
    _, X_test, _, y_test = train_test_split(
        X, y, test_size=TEST_SIZE, random_state=SEED, shuffle=True, stratify=y
    )
    logger.info(
        f"[{abbreviation}] Reconstructed the external validation set: "
        f"{X_test.shape[0]} patients ({y_test.mean():.1%} recurrence)."
    )

    y_score = get_decision_scores(model, X_test)
    # SVC is fitted without probability calibration, so its scores come from
    # decision_function (default cut-off 0) rather than predict_proba
    # (default cut-off 0.5); label and record whichever applies.
    is_probability = hasattr(model, "predict_proba")
    score_name = "p" if is_probability else "score"
    default_threshold = 0.5 if is_probability else 0.0

    # Threshold-independent ranking performance, kept only as context: it is
    # identical across the two scenarios below, since it does not depend on
    # where the cut-off is drawn.
    ranking_metrics = {
        "ROC-AUC": roc_auc_score(y_test, y_score),
        "PR-AUC": average_precision_score(y_test, y_score),
    }

    # ------------------------------------------------------------------
    # 1. Default threshold (0.5)
    # ------------------------------------------------------------------
    y_pred_default = model.predict(X_test)
    metrics_default = compute_hard_metrics(y_test, y_pred_default)
    cm_default = build_confusion_matrix_table(y_test, y_pred_default)
    logger.info(f"[{abbreviation}] Default threshold ({score_name} > {default_threshold}): {metrics_default}")

    # ------------------------------------------------------------------
    # 2. Youden-optimal threshold
    # ------------------------------------------------------------------
    youden = compute_youden_threshold(y_test, y_score)
    y_pred_optimal = apply_threshold(y_score, youden["threshold"])
    metrics_optimal = compute_hard_metrics(y_test, y_pred_optimal)
    cm_optimal = build_confusion_matrix_table(y_test, y_pred_optimal)
    logger.info(
        f"[{abbreviation}] Youden-optimal threshold: {youden['threshold']:.3f} "
        f"(J = {youden['youden_j']:.3f}); metrics: {metrics_optimal}"
    )

    default_label = f"Default ({score_name} > {default_threshold})"
    optimal_label = f"Optimal (Youden, {score_name} > {youden['threshold']:.3f})"

    metrics_table = build_threshold_metrics_table(
        {
            default_label: metrics_default,
            optimal_label: metrics_optimal,
        }
    )

    threshold_rows = [
        {
            "Model": abbreviation,
            "Scenario": default_label,
            "Threshold": default_threshold,
            "Method": "Fixed",
            "N_Test": int(X_test.shape[0]),
            "N_Scored": int(X_test.shape[0]),
        },
        {
            "Model": abbreviation,
            "Scenario": optimal_label,
            "Threshold": youden["threshold"],
            "Method": f"Youden's J = {youden['youden_j']:.3f} "
            f"(sensitivity = {metrics_optimal['Recall']:.3f}, "
            f"specificity = {metrics_optimal['Specificity']:.3f})",
            "N_Test": int(X_test.shape[0]),
            "N_Scored": int(X_test.shape[0]),
        },
    ]

    return {
        "ranking_metrics": ranking_metrics,
        "metrics_table": metrics_table,
        "metrics_default": metrics_default,
        "metrics_optimal": metrics_optimal,
        "cm_default": cm_default,
        "cm_optimal": cm_optimal,
        "threshold_rows": threshold_rows,
        "default_label": default_label,
        "optimal_label": optimal_label,
    }


# %% Main function
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading the multimodal dataset from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)
    y = encode_target_variable(df, TARGET_VARIABLE)

    results = {abbreviation: _analyze_model(abbreviation, df, y) for abbreviation in BEST_MODELS}

    # ------------------------------------------------------------------
    # Save per-model results
    # ------------------------------------------------------------------
    all_threshold_rows = []
    for abbreviation, result in results.items():
        save_csv(result["metrics_table"], OUTPUT_DIR / f"threshold_metrics_{abbreviation}.csv")
        save_csv(result["cm_default"], OUTPUT_DIR / f"confusion_matrix_default_{abbreviation}.csv")
        save_csv(result["cm_optimal"], OUTPUT_DIR / f"confusion_matrix_optimal_{abbreviation}.csv")

        ranking_metrics_df = pd.DataFrame(
            {
                "Metric": list(result["ranking_metrics"]),
                "Score": list(result["ranking_metrics"].values()),
            }
        )
        save_csv(ranking_metrics_df, OUTPUT_DIR / f"ranking_metrics_{abbreviation}.csv")

        all_threshold_rows.extend(result["threshold_rows"])

        for scenario_label, matrix, file_stem in (
            (result["default_label"], result["cm_default"], f"confusion_matrix_default_{abbreviation}"),
            (result["optimal_label"], result["cm_optimal"], f"confusion_matrix_optimal_{abbreviation}"),
        ):
            figure = plot_confusion_matrix(matrix, title=f"{abbreviation} — {scenario_label}")
            save_figure(figure, OUTPUT_DIR / f"{file_stem}.png")

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
