"""Build and plot comparisons between modelling arms, model by model."""

from pathlib import Path

from src.config import MODEL_ORDER
from src.models.model_evaluation import (
    build_modality_comparison_table,
    build_modality_delta_table,
)
from src.models.model_zoo import get_display_name
from src.utils.io import read_csv, save_csv, save_figure
from src.visualization.model_evaluation import plot_modality_comparison


def run_modality_comparison(
    baseline_label: str,
    baseline_metrics_file: Path,
    comparison_label: str,
    comparison_metrics_file: Path,
    output_dir: Path,
    extra_arms: list[tuple[str, Path]] | None = None,
    logger=None,
) -> None:
    """Build and plot one modelling-arm comparison, model by model.

    No model is refitted here: the comparison is rebuilt from the metrics
    tables already saved by each modelling phase, so the figures can be
    regenerated cheaply.

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
        Directory where the comparison tables and figures are saved.
    extra_arms : list of (str, Path), optional
        Additional modelling arms to include between the baseline and the
        comparison arm (e.g. a pure proteomic arm alongside clinical and
        multimodal), each as a (label, metrics_file) pair. The saved delta
        table and its plotted values still contrast baseline vs comparison
        only.
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

    modality_order = (
        baseline_label,
        *(label for label, _ in extra_metrics),
        comparison_label,
    )
    comparison_title = " vs ".join(modality_order)

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
    save_csv(deltas, output_dir / "modality_comparison.csv")

    if logger is not None:
        logger.info(f"Plotting the {comparison_title} comparison...")
    for abbreviation in MODEL_ORDER:
        model_comparison = comparison[comparison["Model"] == abbreviation]
        if model_comparison.empty:
            if logger is not None:
                logger.warning(f"No results found for model {abbreviation}; skipping.")
            continue

        filename = f"comparison_{abbreviation.lower()}"
        save_csv(model_comparison, output_dir / f"{filename}.csv")
        model_name = get_display_name(abbreviation)
        figure = plot_modality_comparison(
            model_comparison,
            model_name=model_name,
            modality_order=modality_order,
            title=f"{comparison_title}: {model_name}",
        )
        save_figure(figure, output_dir / f"{filename}.png")

    if logger is not None:
        logger.info(f"Results saved to {output_dir}.")
