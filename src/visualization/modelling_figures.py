"""Redraw a whole modelling phase or arm comparison from its saved tables.

The functions in ``src.visualization.model_evaluation`` are pure builders: they
take a DataFrame and return a figure. The two routines here sit one level above,
composing those builders with the I/O needed to turn a results directory into a
complete set of figures on disk. Nothing is refitted, so a change of palette or
title costs seconds rather than a full retrain.

Both take the same specification dicts the computing pipelines use
(``MODELLING_PHASES`` and ``MODALITY_COMPARISONS`` in ``src.config``), so the
tables and the figures can never disagree about which arms exist or in which
order they are read.
"""

import logging
from pathlib import Path

from src.config import (
    FEATURE_SELECTION_MODEL,
    FEATURE_SELECTION_TOP_N,
    SCORING_METRICS,
)
from src.models.model_evaluation import build_internal_validation_table, load_curve_areas
from src.models.modality_comparison import modality_order
from src.models.results_saving import load_internal_validation, load_prevalence
from src.utils.filenames import (
    AUC_BY_MODEL_FIGURE,
    AUC_BY_MODEL_FILE,
    CURVES_FIGURE,
    CURVES_FILE,
    FEATURE_SELECTION_FIGURE,
    FEATURE_SELECTION_FILE,
    INTERNAL_VALIDATION_FIGURE,
    MODALITY_COMPARISON_FIGURE,
    MODALITY_COMPARISON_FILE,
    MODELS_METRICS_FILE,
    metric_slug,
)
from src.utils.io import read_csv, save_figure
from src.visualization.model_evaluation import (
    plot_feature_selection_coefficients,
    plot_internal_validation,
    plot_metric_by_model,
    plot_modality_comparison,
    plot_model_pr_curves,
    plot_model_roc_curves,
)

# Bar chart metric, curve file fragment and the AUC it is labelled with.
AUC_METRICS = (("ROC-AUC", "roc"), ("PR-AUC", "pr"))

_logger = logging.getLogger(__name__)


def plot_modelling_phase(
    input_dir: str | Path,
    phase_title: str,
    baseline: dict[str, float],
    logger: logging.Logger | None = None,
) -> None:
    """Redraw every figure of one modelling phase from its saved tables.

    Five families of charts are produced: the Elastic Net feature selection
    coefficients, the train-vs-validation overfitting bars (one per metric), the
    ROC and precision-recall curves of the external validation, and the AUC bar
    charts against the risk score baseline.

    Parameters
    ----------
    input_dir : str or Path
        Output directory of the modelling phase, e.g. MULTIMODAL_MODELS_DIR.
        Figures are written back into it, next to the tables they come from.
    phase_title : str
        Suffix appended to the figure titles, e.g. 'multimodal data'.
    baseline : dict of str to float
        Metric label to reference value drawn on the AUC bar charts.
    logger : logging.Logger, optional
        Logger for the progress messages. Defaults to this module's logger.

    Returns
    -------
    None
    """
    log = logger or _logger
    input_dir = Path(input_dir)
    title_suffix = f" ({phase_title})" if phase_title else ""

    log.info(f"Plotting the feature selection coefficients of {input_dir.name}...")
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

    log.info(f"Plotting the overfitting analysis of {input_dir.name}...")
    internal_validation = build_internal_validation_table(
        load_internal_validation(input_dir, logger=log)
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

    log.info(f"Plotting the external validation curves of {input_dir.name}...")
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

    log.info(f"Plotting the AUC bar charts of {input_dir.name}...")
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


def plot_arm_comparison(
    baseline_label: str,
    comparison_label: str,
    output_dir: str | Path,
    extra_arms: list[tuple[str, Path]] | None = None,
    logger: logging.Logger | None = None,
    **_unused: object,
) -> None:
    """Redraw one modelling-arm comparison, metric by metric.

    Takes the same specification dict as the table builder of pipeline 13, so
    both read the arms in the same order; the metrics-file entries of that dict
    are irrelevant here and absorbed by ``_unused``.

    Parameters
    ----------
    baseline_label : str
        Name of the reference arm.
    comparison_label : str
        Name of the arm being compared.
    output_dir : str or Path
        Directory holding the comparison tables, where the figures are saved.
    extra_arms : list of (str, Path), optional
        Additional arms shown between the two, as (label, metrics_file) pairs.
    logger : logging.Logger, optional
        Logger for the progress messages. Defaults to this module's logger.

    Returns
    -------
    None
    """
    log = logger or _logger
    output_dir = Path(output_dir)
    order = modality_order(baseline_label, comparison_label, extra_arms)
    comparison_title = " vs ".join(order)

    log.info(f"Plotting the {comparison_title} comparison...")
    for metric in SCORING_METRICS:
        file_path = output_dir / MODALITY_COMPARISON_FILE.format(
            metric=metric_slug(metric)
        )
        if not file_path.exists():
            log.warning(f"Missing {file_path}; skipping {metric}.")
            continue
        save_figure(
            plot_modality_comparison(
                read_csv(file_path),
                metric=metric,
                modality_order=order,
                title=f"{comparison_title}: {metric}",
            ),
            output_dir / MODALITY_COMPARISON_FIGURE.format(metric=metric_slug(metric)),
        )

    log.info(f"Figures saved to {output_dir}.")
