"""Draw the figures of every modelling phase trained by pipelines 09 to 12.

Five families of charts are redrawn per phase, all from the tables the phase
left on disk: the Elastic Net feature selection coefficients, the
train-vs-validation overfitting bars, the ROC and PR curves of the external
validation, and the AUC bar charts against the risk score baseline. Nothing is
refitted, so a change of palette or title costs seconds.
"""

from pathlib import Path

from src.config import (
    FEATURE_SELECTION_MODEL,
    FEATURE_SELECTION_TOP_N,
    MODELLING_PHASES,
    SCORING_METRICS,
)
from src.models.model_evaluation import (
    build_internal_validation_table,
    load_curve_areas,
)
from src.models.results_saving import load_internal_validation, load_prevalence
from src.utils.filenames import (
    AUC_BY_MODEL_FIGURE,
    AUC_BY_MODEL_FILE,
    CURVES_FIGURE,
    CURVES_FILE,
    FEATURE_SELECTION_FIGURE,
    FEATURE_SELECTION_FILE,
    INTERNAL_VALIDATION_FIGURE,
    MODELS_METRICS_FILE,
    metric_slug,
)
from src.utils.io import read_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.visualization.model_evaluation import (
    plot_feature_selection_coefficients,
    plot_internal_validation,
    plot_metric_by_model,
    plot_model_pr_curves,
    plot_model_roc_curves,
)

# Bar chart metric, curve file fragment and the AUC it is labelled with.
AUC_METRICS = (("ROC-AUC", "roc"), ("PR-AUC", "pr"))

logger = setup_logger(Path(__file__).stem)


def plot_phase(input_dir, phase_title: str, baseline: dict) -> None:
    """Redraw every figure of one modelling phase from its saved tables.

    Parameters
    ----------
    input_dir : str or Path
        Output directory of the modelling phase, e.g. MULTIMODAL_MODELS_DIR.
    phase_title : str
        Suffix appended to the figure titles, e.g. 'multimodal data'.
    baseline : dict
        Metric label to reference value drawn on the AUC bar charts.

    Returns
    -------
    None
    """
    input_dir = Path(input_dir)
    title_suffix = f" ({phase_title})" if phase_title else ""

    logger.info(f"Plotting the feature selection coefficients of {input_dir.name}...")
    coefficients = read_csv(
        input_dir / FEATURE_SELECTION_FILE.format(model=FEATURE_SELECTION_MODEL)
    )
    save_figure(
        plot_feature_selection_coefficients(
            coefficients,
            title=f"Feature selection coefficients{title_suffix}",
            top_n=FEATURE_SELECTION_TOP_N,
        ),
        input_dir / FEATURE_SELECTION_FIGURE.format(model=FEATURE_SELECTION_MODEL),
    )

    logger.info(f"Plotting the overfitting analysis of {input_dir.name}...")
    internal_validation = build_internal_validation_table(
        load_internal_validation(input_dir, logger=logger)
    )
    for metric in SCORING_METRICS:
        save_figure(
            plot_internal_validation(
                internal_validation,
                metric=metric,
                title=f"Overfitting analysis: {metric}{title_suffix}",
            ),
            input_dir / INTERNAL_VALIDATION_FIGURE.format(metric=metric_slug(metric)),
        )

    metrics_df = read_csv(input_dir / MODELS_METRICS_FILE)

    logger.info(f"Plotting the external validation curves of {input_dir.name}...")
    save_figure(
        plot_model_roc_curves(
            read_csv(input_dir / CURVES_FILE.format(curve="roc")),
            load_curve_areas(metrics_df, "ROC-AUC"),
            title=f"ROC curves{title_suffix}",
        ),
        input_dir / CURVES_FIGURE.format(curve="roc"),
    )
    save_figure(
        plot_model_pr_curves(
            read_csv(input_dir / CURVES_FILE.format(curve="pr")),
            load_curve_areas(metrics_df, "PR-AUC"),
            prevalence=load_prevalence(input_dir),
            title=f"Precision-recall curves{title_suffix}",
        ),
        input_dir / CURVES_FIGURE.format(curve="pr"),
    )

    logger.info(f"Plotting the AUC bar charts of {input_dir.name}...")
    for metric, curve in AUC_METRICS:
        auc_table = read_csv(input_dir / AUC_BY_MODEL_FILE.format(curve=curve))
        save_figure(
            plot_metric_by_model(
                auc_table,
                metric=metric,
                baseline=baseline.get(metric),
                title=f"{metric} by model{title_suffix}",
            ),
            input_dir / AUC_BY_MODEL_FIGURE.format(curve=curve),
        )


# %% Main function
def main() -> None:
    for phase_spec in MODELLING_PHASES:
        plot_phase(**phase_spec)


if __name__ == "__main__":
    main()
