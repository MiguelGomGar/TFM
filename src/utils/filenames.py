"""Basenames of the artifacts exchanged between the computing and plotting pipelines.

Since the refactor that split computation from figure generation, every result
file is written by a pipeline in ``src/pipelines/calculations/`` and read back by
its mirror in ``src/pipelines/plots/``. Hard-coding the names on both sides
invites silent drift, so the shared ones are declared here once. Templates use
``str.format`` placeholders; anything written and read by a single script stays
inline there.

Names are grouped into frozen dataclasses by the analysis stage that produces
them, and flattened into module-level constants at the bottom, which is the
interface the pipelines import.
"""

from dataclasses import dataclass

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


# %% Artifact name groups


@dataclass(frozen=True)
class MissingValuesArtifacts:
    """Missing value report of pipeline 03. Keyed by cleaning ``{stage}``."""

    heatmap_file: str = "missing_values_heatmap_{stage}.csv"
    heatmap_figure: str = "missing_values_heatmap_{stage}.png"
    per_feature_file: str = "missing_values_per_feature.csv"
    per_feature_figure: str = "missing_values_per_feature.png"
    per_record_file: str = "missing_values_per_record_{stage}.csv"
    per_record_figure: str = "missing_values_per_record_{stage}.png"
    stratified_file: str = "missing_values_stratified_{stage}.csv"
    stratified_figure: str = "missing_values_stratified_{stage}.png"


@dataclass(frozen=True)
class CollinearityArtifacts:
    """Collinearity diagnostics of pipeline 04. ``{dtype}`` is 'num' or 'cat'."""

    correlation_matrix_file: str = "correlation_matrix_{dtype}.csv"
    correlation_matrix_figure: str = "correlation_matrix_{dtype}.png"
    vif_file: str = "vif_diagnostics.csv"
    vif_figure: str = "vif_diagnostics.png"


@dataclass(frozen=True)
class DistributionArtifacts:
    """Per-feature distributions of pipelines 06 and 07.

    ``{feature}`` is the column name and ``{group}`` the stratifying variable.
    """

    distribution_file: str = "distribution_{feature}.csv"
    distribution_figure: str = "distribution_{feature}.png"
    qq_file: str = "distribution_{feature}_QQ.csv"
    qq_figure: str = "distribution_{feature}_QQ.png"
    stratified_file: str = "distribution_{feature}_stratified_by_{group}.csv"
    stratified_figure: str = "distribution_{feature}_stratified_by_{group}.png"


@dataclass(frozen=True)
class ModellingArtifacts:
    """Outputs of the risk score validation and modelling phases (08 to 12).

    ``{model}`` is a model abbreviation from ``MODEL_ORDER``, ``{curve}`` is
    'roc' or 'pr', and ``{metric}`` is a slug from ``metric_slug``.
    """

    prevalence_file: str = "prevalence.csv"
    curves_file: str = "curves_{curve}.csv"
    curves_figure: str = "curves_{curve}.png"
    models_metrics_file: str = "models_metrics.csv"
    best_params_file: str = "best_params.csv"
    internal_validation_file: str = "internal_validation_{model}.csv"
    internal_validation_figure: str = "internal_validation_{metric}.png"
    auc_by_model_file: str = "auc_{curve}_by_model.csv"
    auc_by_model_figure: str = "auc_{curve}_by_model.png"
    feature_selection_file: str = "feature_selection_{model}.csv"
    feature_selection_figure: str = "feature_selection_{model}.png"


@dataclass(frozen=True)
class ComparisonArtifacts:
    """Modality comparison tables and figures of pipeline 13."""

    delta_file: str = "modality_comparison.csv"
    comparison_file: str = "comparison_{metric}.csv"
    comparison_figure: str = "comparison_{metric}.png"


@dataclass(frozen=True)
class ThresholdArtifacts:
    """Threshold sensitivity analysis of the best models (pipeline 14)."""

    info_file: str = "threshold_info.csv"
    metrics_file: str = "threshold_metrics_{model}.csv"
    ranking_metrics_file: str = "ranking_metrics_{model}.csv"
    confusion_matrix_file: str = "confusion_matrix_optimal_{model}.csv"
    confusion_matrix_figure: str = "confusion_matrix_optimal_{model}.png"
    comparison_file: str = "threshold_metrics_comparison.csv"
    comparison_figure: str = "threshold_metrics_comparison.png"


MISSING_VALUES = MissingValuesArtifacts()
COLLINEARITY = CollinearityArtifacts()
DISTRIBUTIONS = DistributionArtifacts()
MODELLING = ModellingArtifacts()
COMPARISON = ComparisonArtifacts()
THRESHOLD = ThresholdArtifacts()

# %% Module-level bindings

MISSING_HEATMAP_FILE = MISSING_VALUES.heatmap_file
MISSING_HEATMAP_FIGURE = MISSING_VALUES.heatmap_figure
MISSING_PER_FEATURE_FILE = MISSING_VALUES.per_feature_file
MISSING_PER_FEATURE_FIGURE = MISSING_VALUES.per_feature_figure
MISSING_PER_RECORD_FILE = MISSING_VALUES.per_record_file
MISSING_PER_RECORD_FIGURE = MISSING_VALUES.per_record_figure
MISSING_STRATIFIED_FILE = MISSING_VALUES.stratified_file
MISSING_STRATIFIED_FIGURE = MISSING_VALUES.stratified_figure

CORRELATION_MATRIX_FILE = COLLINEARITY.correlation_matrix_file
CORRELATION_MATRIX_FIGURE = COLLINEARITY.correlation_matrix_figure
VIF_FILE = COLLINEARITY.vif_file
VIF_FIGURE = COLLINEARITY.vif_figure

DISTRIBUTION_FILE = DISTRIBUTIONS.distribution_file
DISTRIBUTION_FIGURE = DISTRIBUTIONS.distribution_figure
QQ_FILE = DISTRIBUTIONS.qq_file
QQ_FIGURE = DISTRIBUTIONS.qq_figure
STRATIFIED_DISTRIBUTION_FILE = DISTRIBUTIONS.stratified_file
STRATIFIED_DISTRIBUTION_FIGURE = DISTRIBUTIONS.stratified_figure

PREVALENCE_FILE = MODELLING.prevalence_file
CURVES_FILE = MODELLING.curves_file
CURVES_FIGURE = MODELLING.curves_figure
MODELS_METRICS_FILE = MODELLING.models_metrics_file
BEST_PARAMS_FILE = MODELLING.best_params_file
INTERNAL_VALIDATION_FILE = MODELLING.internal_validation_file
INTERNAL_VALIDATION_FIGURE = MODELLING.internal_validation_figure
AUC_BY_MODEL_FILE = MODELLING.auc_by_model_file
AUC_BY_MODEL_FIGURE = MODELLING.auc_by_model_figure
FEATURE_SELECTION_FILE = MODELLING.feature_selection_file
FEATURE_SELECTION_FIGURE = MODELLING.feature_selection_figure

MODALITY_DELTA_FILE = COMPARISON.delta_file
MODALITY_COMPARISON_FILE = COMPARISON.comparison_file
MODALITY_COMPARISON_FIGURE = COMPARISON.comparison_figure

THRESHOLD_INFO_FILE = THRESHOLD.info_file
THRESHOLD_METRICS_FILE = THRESHOLD.metrics_file
RANKING_METRICS_FILE = THRESHOLD.ranking_metrics_file
CONFUSION_MATRIX_FILE = THRESHOLD.confusion_matrix_file
CONFUSION_MATRIX_FIGURE = THRESHOLD.confusion_matrix_figure
THRESHOLD_COMPARISON_FILE = THRESHOLD.comparison_file
THRESHOLD_COMPARISON_FIGURE = THRESHOLD.comparison_figure
