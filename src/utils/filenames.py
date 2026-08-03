"""Basenames of the artifacts exchanged between the computing and plotting pipelines.

Since the refactor that split computation from figure generation, every result
file is written by a pipeline in ``src/pipelines/`` and read back by its mirror
in ``src/pipelines/plots/``. Hard-coding the names on both sides invites silent
drift, so the shared ones are declared here once. Templates use ``str.format``
placeholders; anything written and read by a single script stays inline there.
"""

# %% Helpers


def metric_slug(metric: str) -> str:
    """Turn a metric label into the filename fragment used across the project.

    Parameters
    ----------
    metric : str
        Metric label as it appears in the results tables, e.g. 'ROC-AUC'.

    Returns
    -------
    str
        Lowercase, underscore-separated fragment, e.g. 'roc_auc'.
    """
    return metric.lower().replace("-", "_")


# %% Exploratory data analysis (pipelines 03, 04, 06, 07)

MISSING_HEATMAP_FILE = "missing_values_heatmap_{stage}.csv"
MISSING_HEATMAP_FIGURE = "missing_values_heatmap_{stage}.png"
MISSING_PER_FEATURE_FILE = "missing_values_per_feature.csv"
MISSING_PER_FEATURE_FIGURE = "missing_values_per_feature.png"
MISSING_PER_RECORD_FILE = "missing_values_per_record_{stage}.csv"
MISSING_PER_RECORD_FIGURE = "missing_values_per_record_{stage}.png"
MISSING_STRATIFIED_FILE = "missing_values_stratified_{stage}.csv"
MISSING_STRATIFIED_FIGURE = "missing_values_stratified_{stage}.png"

CORRELATION_MATRIX_FILE = "correlation_matrix_{dtype}.csv"
CORRELATION_MATRIX_FIGURE = "correlation_matrix_{dtype}.png"
VIF_FILE = "vif_diagnostics.csv"
VIF_FIGURE = "vif_diagnostics.png"

DISTRIBUTION_FILE = "distribution_{feature}.csv"
DISTRIBUTION_FIGURE = "distribution_{feature}.png"
QQ_FILE = "distribution_{feature}_QQ.csv"
QQ_FIGURE = "distribution_{feature}_QQ.png"
STRATIFIED_DISTRIBUTION_FILE = "distribution_{feature}_stratified_by_{group}.csv"
STRATIFIED_DISTRIBUTION_FIGURE = "distribution_{feature}_stratified_by_{group}.png"

# %% Risk scores and modelling phases (pipelines 08 to 12)

PREVALENCE_FILE = "prevalence.csv"
CURVES_FILE = "curves_{curve}.csv"
CURVES_FIGURE = "curves_{curve}.png"

MODELS_METRICS_FILE = "models_metrics.csv"
BEST_PARAMS_FILE = "best_params.csv"
INTERNAL_VALIDATION_FILE = "internal_validation_{model}.csv"
INTERNAL_VALIDATION_FIGURE = "internal_validation_{metric}.png"
AUC_BY_MODEL_FILE = "auc_{curve}_by_model.csv"
AUC_BY_MODEL_FIGURE = "auc_{curve}_by_model.png"

# %% Modality comparison (pipeline 13)

MODALITY_DELTA_FILE = "modality_comparison.csv"
MODALITY_COMPARISON_FILE = "comparison_{metric}.csv"
MODALITY_COMPARISON_FIGURE = "comparison_{metric}.png"

# %% Threshold analysis of the best models (pipeline 14)

THRESHOLD_INFO_FILE = "threshold_info.csv"
THRESHOLD_METRICS_FILE = "threshold_metrics_{model}.csv"
RANKING_METRICS_FILE = "ranking_metrics_{model}.csv"
CONFUSION_MATRIX_FILE = "confusion_matrix_optimal_{model}.csv"
CONFUSION_MATRIX_FIGURE = "confusion_matrix_optimal_{model}.png"
THRESHOLD_COMPARISON_FILE = "threshold_metrics_comparison.csv"
THRESHOLD_COMPARISON_FIGURE = "threshold_metrics_comparison.png"

# %% SHAP explainability (pipeline 15)

SHAP_EXPLANATION_FILE = "shap_explanation_{model}.joblib"
SHAP_IMPORTANCE_FILE = "shap_importance_{model}.csv"
SHAP_IMPORTANCE_FIGURE = "shap_importance_{model}.png"
SHAP_VALUES_FILE = "shap_values_{model}.csv"
SHAP_SUMMARY_FIGURE = "shap_summary_{model}.png"
SHAP_WATERFALL_FIGURE = "shap_waterfall_{model}_{case}.png"
LOCAL_CASES_FILE = "local_cases_{model}.csv"
RANKING_COMPARISON_FILE = "ranking_comparison.csv"
RANKING_COMPARISON_FIGURE = "ranking_comparison.png"
RANK_CORRELATIONS_FILE = "rank_correlations.csv"
BLOCK_CONTRIBUTION_FILE = "block_contribution.csv"
SHAP_VALIDATION_FILE = "shap_validation.csv"
