"""Build the comparisons between modelling arms, metric by metric.

No model is refitted here: every comparison is rebuilt from the metrics tables
already saved by each modelling phase, so it can be regenerated cheaply. The
corresponding figures are drawn from these tables by
``src/pipelines/plots/13_modality_comparison_plots.py``.
"""

from collections.abc import Sequence
from logging import Logger
from pathlib import Path

from src.config import SCORING_METRICS
from src.models.model_evaluation import (
    build_modality_comparison_table,
    build_modality_delta_table,
)
from src.utils.filenames import (
    MODALITY_COMPARISON_FILE,
    MODALITY_DELTA_FILE,
    metric_slug,
)
from src.utils.io import read_csv, save_csv


def modality_order(
    baseline_label: str,
    comparison_label: str,
    extra_arms: Sequence[tuple[str, Path]] | None = None,
) -> tuple[str, ...]:
    """List the arms of one comparison in reporting order.

    Shared by the table builder and the plotting pipeline so both lay the arms
    out identically.

    Parameters
    ----------
    baseline_label : str
        Name of the reference arm.
    comparison_label : str
        Name of the arm being compared.
    extra_arms : list of (str, Path), optional
        Additional arms shown between the two, as (label, metrics_file) pairs.

    Returns
    -------
    tuple of str
        Arm labels, baseline first and comparison last.
    """
    extra_arms = extra_arms or []
    return (baseline_label, *(label for label, _ in extra_arms), comparison_label)


def build_modality_comparison(
    baseline_label: str,
    baseline_metrics_file: str | Path,
    comparison_label: str,
    comparison_metrics_file: str | Path,
    output_dir: str | Path,
    extra_arms: Sequence[tuple[str, Path]] | None = None,
    logger: Logger | None = None,
) -> None:
    """Build and save one modelling-arm comparison, metric by metric.

    Parameters
    ----------
    baseline_label : str
        Name of the reference arm, used as its 'Modality' value.
    baseline_metrics_file : str or Path
        Path to the reference arm's consolidated models_metrics.csv.
    comparison_label : str
        Name of the arm being compared, used as its 'Modality' value.
    comparison_metrics_file : str or Path
        Path to the comparison arm's consolidated models_metrics.csv.
    output_dir : str or Path
        Directory where the comparison tables are saved.
    extra_arms : list of (str, Path), optional
        Additional modelling arms to include between the baseline and the
        comparison arm (e.g. a pure proteomic arm alongside clinical and
        multimodal), each as a (label, metrics_file) pair. The saved delta
        table still contrasts baseline vs comparison only.
    logger : logging.Logger, optional
        Logger used to report progress.

    Returns
    -------
    None
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    extra_arms = extra_arms or []

    if logger is not None:
        logger.info(f"Loading {baseline_label} metrics from {baseline_metrics_file}...")
    baseline_metrics = read_csv(baseline_metrics_file)

    if logger is not None:
        logger.info(f"Loading {comparison_label} metrics from {comparison_metrics_file}...")
    comparison_metrics = read_csv(comparison_metrics_file)

    extra_metrics = []
    for extra_label, extra_metrics_file in extra_arms:
        if logger is not None:
            logger.info(f"Loading {extra_label} metrics from {extra_metrics_file}...")
        extra_metrics.append((extra_label, read_csv(extra_metrics_file)))

    comparison_title = " vs ".join(
        modality_order(baseline_label, comparison_label, extra_metrics)
    )

    if logger is not None:
        logger.info(f"Building the {comparison_title} tables...")
    comparison = build_modality_comparison_table(
        baseline_metrics,
        comparison_metrics,
        baseline_label=baseline_label,
        comparison_label=comparison_label,
        extra_arms=extra_metrics,
    )
    deltas = build_modality_delta_table(
        comparison, baseline_label=baseline_label, comparison_label=comparison_label
    )
    save_csv(deltas, output_dir / MODALITY_DELTA_FILE)

    for metric in SCORING_METRICS:
        metric_comparison = comparison[comparison["Metric"] == metric]
        if metric_comparison.empty:
            if logger is not None:
                logger.warning(f"No results found for metric {metric}; skipping.")
            continue
        save_csv(
            metric_comparison,
            output_dir / MODALITY_COMPARISON_FILE.format(metric=metric_slug(metric)),
        )

    if logger is not None:
        logger.info(f"Results saved to {output_dir}.")
