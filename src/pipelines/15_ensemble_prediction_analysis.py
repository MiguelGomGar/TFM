"""Phase 3b: agreement between the top multimodal models and their ensemble.

Elastic Net (EN), SVM and MLP are the three best-performing models on the
multimodal test set built by pipeline 12 (see
``results/models/multimodal_data/models_metrics.csv``): MLP and EN lead on
PR-AUC, and SVM is kept over the marginally better Random Forest because it
shows much less overfitting between the training and validation folds (see
``internal_validation_*.csv``). This phase checks whether the three models
actually agree with each other on individual patients, compares the
probabilities they assign before any binarization, and reports the simple
majority-voting ensemble that follows from combining their predictions.

The three fitted pipelines are reloaded as-is from disk; nothing is retrained
except one calibrated copy of the SVM (see
``src.models.ensemble.recalibrate_svm_probabilities``), needed only for the
probability comparison because the SVM is otherwise fitted with
``probability=False`` in every other phase.
"""

from pathlib import Path

from sklearn.model_selection import train_test_split

from src.config import ENSEMBLE_MODELS, SEED, TARGET_VARIABLE, TEST_SIZE
from src.models.data_preprocessing import encode_target_variable
from src.models.ensemble import (
    build_agreement_table,
    build_ensemble_curves,
    build_probability_distribution_table,
    get_model_feature_columns,
    get_positive_class_probability,
    load_fitted_pipeline,
    majority_vote,
    recalibrate_svm_probabilities,
)
from src.models.model_evaluation import get_decision_scores
from src.utils.io import read_parquet, save_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    CLEANED_MULTIMODAL_DATA_PATH,
    ENSEMBLE_ANALYSIS_DIR,
    MULTIMODAL_MODELS_DIR,
)
from src.utils.results_saving import save_curves_results
from src.visualization.ensemble import ensemble_curve_colors, plot_probability_violin
from src.visualization.model_evaluation import plot_model_pr_curves, plot_model_roc_curves

INPUT_FILE = CLEANED_MULTIMODAL_DATA_PATH
MODELS_DIR = MULTIMODAL_MODELS_DIR
OUTPUT_DIR = ENSEMBLE_ANALYSIS_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading the multimodal dataset from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)
    y = encode_target_variable(df, TARGET_VARIABLE)

    logger.info(f"Loading the fitted {ENSEMBLE_MODELS} pipelines from {MODELS_DIR}...")
    fitted_models = {
        abbreviation: load_fitted_pipeline(MODELS_DIR, abbreviation)
        for abbreviation in ENSEMBLE_MODELS
    }

    # Every model compared here was trained on the same multimodal feature
    # set (phase 3 does not re-filter it further; see pipeline 12), so any of
    # the fitted preprocessors defines the exact predictors and, together
    # with the shared seed and split settings below, the exact patient
    # partition used originally.
    reference_model = ENSEMBLE_MODELS[0]
    feature_columns = get_model_feature_columns(fitted_models[reference_model])
    for abbreviation, model in fitted_models.items():
        if get_model_feature_columns(model) != feature_columns:
            raise ValueError(
                f"The {abbreviation} pipeline was trained on a different "
                f"feature set than {reference_model}; the models are not "
                "directly comparable."
            )

    X = df[feature_columns]
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=TEST_SIZE, random_state=SEED, shuffle=True, stratify=y
    )
    logger.info(
        f"Reconstructed the external validation set: {X_test.shape[0]} patients "
        f"({y_test.mean():.1%} recurrence)."
    )

    # ------------------------------------------------------------------
    # 1. Probability distributions
    # ------------------------------------------------------------------
    logger.info(
        "Recalibrating the SVM with Platt scaling for the probability comparison..."
    )
    calibrated_svm = recalibrate_svm_probabilities(
        fitted_models["SVM"], X_train, y_train, seed=SEED
    )

    probabilities_by_model = {
        "EN": get_positive_class_probability(fitted_models["EN"], X_test),
        "SVM": get_positive_class_probability(calibrated_svm, X_test),
        "MLP": get_positive_class_probability(fitted_models["MLP"], X_test),
    }
    probabilities_df = build_probability_distribution_table(probabilities_by_model)
    save_csv(probabilities_df, OUTPUT_DIR / "probability_distributions.csv")

    figure = plot_probability_violin(probabilities_df)
    save_figure(figure, OUTPUT_DIR / "probability_distributions.png")
    logger.info("Saved the probability distribution table and violin plot.")

    # ------------------------------------------------------------------
    # 2. Majority-voting ensemble
    # ------------------------------------------------------------------
    logger.info("Building the majority-voting ensemble...")
    predictions_by_model = {
        abbreviation: model.predict(X_test) for abbreviation, model in fitted_models.items()
    }
    vote_share, ensemble_pred = majority_vote(predictions_by_model)

    agreement_table = build_agreement_table(predictions_by_model, y_test, ensemble_pred)
    save_csv(agreement_table, OUTPUT_DIR / "prediction_agreement.csv")
    unanimous_rate = agreement_table["Unanimous"].mean()
    logger.info(f"The three models agree on {unanimous_rate:.1%} of the test patients.")

    # Ranking scores only: each individual model's own decision score
    # (predict_proba, or decision_function for the non-recalibrated SVM,
    # exactly as used everywhere else in the project) plus the ensemble's
    # vote share. No threshold-dependent ('hard') metric is computed here.
    scores_by_model = {
        abbreviation: get_decision_scores(model, X_test)
        for abbreviation, model in fitted_models.items()
    }
    scores_by_model["Ensemble"] = vote_share

    curves_by_model = build_ensemble_curves(y_test, scores_by_model)
    colors = ensemble_curve_colors(ENSEMBLE_MODELS)

    roc_curves = save_curves_results(
        list(curves_by_model),
        [curves_by_model[label]["fpr"] for label in curves_by_model],
        [curves_by_model[label]["tpr"] for label in curves_by_model],
        curve_type="roc",
        output_dir=OUTPUT_DIR,
    )
    pr_curves = save_curves_results(
        list(curves_by_model),
        [curves_by_model[label]["recall"] for label in curves_by_model],
        [curves_by_model[label]["precision"] for label in curves_by_model],
        curve_type="pr",
        output_dir=OUTPUT_DIR,
    )

    figure = plot_model_roc_curves(
        roc_curves,
        {label: curves_by_model[label]["roc_auc"] for label in curves_by_model},
        title="ROC curves: individual models vs. ensemble",
        legend_loc="upper right",
        colors=colors,
    )
    save_figure(figure, OUTPUT_DIR / "curves_roc.png")

    figure = plot_model_pr_curves(
        pr_curves,
        {label: curves_by_model[label]["pr_auc"] for label in curves_by_model},
        prevalence=float(y_test.mean()),
        title="Precision-recall curves: individual models vs. ensemble",
        legend_loc="upper right",
        colors=colors,
    )
    save_figure(figure, OUTPUT_DIR / "curves_pr.png")
    logger.info("Saved the ROC and precision-recall curves (individual models vs. ensemble).")

    logger.info(f"Ensemble analysis results saved to {OUTPUT_DIR}.")


if __name__ == "__main__":
    main()
