"""Draw the modelling-arm comparisons built by pipeline 13."""

from pathlib import Path

from src.config import MODALITY_COMPARISONS, SCORING_METRICS
from src.models.modality_comparison import modality_order
from src.utils.filenames import (
    MODALITY_COMPARISON_FIGURE,
    MODALITY_COMPARISON_FILE,
    metric_slug,
)
from src.utils.io import read_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.visualization.model_evaluation import plot_modality_comparison

logger = setup_logger(Path(__file__).stem)


def plot_comparison(
    baseline_label: str,
    comparison_label: str,
    output_dir,
    extra_arms=None,
    **_unused,
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

    Returns
    -------
    None
    """
    output_dir = Path(output_dir)
    order = modality_order(baseline_label, comparison_label, extra_arms)
    comparison_title = " vs ".join(order)

    logger.info(f"Plotting the {comparison_title} comparison...")
    for metric in SCORING_METRICS:
        file_path = output_dir / MODALITY_COMPARISON_FILE.format(
            metric=metric_slug(metric)
        )
        if not file_path.exists():
            logger.warning(f"Missing {file_path}; skipping {metric}.")
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

    logger.info(f"Figures saved to {output_dir}.")


# %% Main function
def main() -> None:
    for comparison_spec in MODALITY_COMPARISONS:
        plot_comparison(**comparison_spec)


if __name__ == "__main__":
    main()
