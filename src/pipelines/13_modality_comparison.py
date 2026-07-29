"""Compare modelling arms trained on the same partition, model by model.

Two comparisons are run:
  - Clinical (matched) vs multimodal: the two arms of phase 3, trained on the
    same matched subcohort. Isolates the contribution of the proteomic panel.
  - Clinical vs clinical filtered: phases 1 and 1b, trained on the full
    clinical cohort. Isolates the effect of the Elastic Net feature filtering.

No model is refitted here: each comparison is rebuilt from the metrics
tables, so the figures can be regenerated cheaply.
"""

from pathlib import Path

from src.config import MODEL_ORDER
from src.models.model_evaluation import (
    build_modality_comparison_table,
    build_modality_delta_table,
)
from src.models.model_zoo import get_display_name
from src.utils.io import read_csv, save_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    CLINICAL_FILTERING_COMPARISON_DIR,
    CLINICAL_MODELS_DIR,
    CLINICAL_MODELS_FILTERED_DIR,
    CLINICAL_MODELS_MATCHED_DIR,
    MODALITY_COMPARISON_DIR,
    MULTIMODAL_MODELS_DIR,
)
from src.visualization.model_evaluation import plot_modality_comparison

# Each entry compares two modelling arms trained on the same cohort and split.
COMPARISONS = [
    {
        "baseline_label": "Clinical",
        "baseline_metrics_file": CLINICAL_MODELS_MATCHED_DIR / "models_metrics.csv",
        "comparison_label": "Multimodal",
        "comparison_metrics_file": MULTIMODAL_MODELS_DIR / "models_metrics.csv",
        "output_dir": MODALITY_COMPARISON_DIR,
    },
    {
        "baseline_label": "Clinical",
        "baseline_metrics_file": CLINICAL_MODELS_DIR / "models_metrics.csv",
        "comparison_label": "Clinical filtered",
        "comparison_metrics_file": CLINICAL_MODELS_FILTERED_DIR / "models_metrics.csv",
        "output_dir": CLINICAL_FILTERING_COMPARISON_DIR,
    },
]

logger = setup_logger(Path(__file__).stem)


def run_comparison(
    baseline_label: str,
    baseline_metrics_file: Path,
    comparison_label: str,
    comparison_metrics_file: Path,
    output_dir: Path,
) -> None:
    """Build and plot one modelling-arm comparison, model by model."""
    output_dir.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading {baseline_label} metrics from {baseline_metrics_file}...")
    baseline_metrics = read_csv(baseline_metrics_file)

    logger.info(
        f"Loading {comparison_label} metrics from {comparison_metrics_file}..."
    )
    comparison_metrics = read_csv(comparison_metrics_file)

    logger.info(f"Building the {baseline_label} vs {comparison_label} tables...")
    comparison = build_modality_comparison_table(
        baseline_metrics,
        comparison_metrics,
        baseline_label=baseline_label,
        comparison_label=comparison_label,
    )
    deltas = build_modality_delta_table(
        comparison, baseline_label=baseline_label, comparison_label=comparison_label
    )
    save_csv(deltas, output_dir / "modality_comparison.csv")

    logger.info(f"Plotting the {baseline_label} vs {comparison_label} comparison...")
    for abbreviation in MODEL_ORDER:
        model_comparison = comparison[comparison["Model"] == abbreviation]
        if model_comparison.empty:
            logger.warning(f"No results found for model {abbreviation}; skipping.")
            continue

        filename = f"comparison_{abbreviation.lower()}"
        save_csv(model_comparison, output_dir / f"{filename}.csv")
        figure = plot_modality_comparison(
            model_comparison,
            model_name=get_display_name(abbreviation),
            modality_order=(baseline_label, comparison_label),
        )
        save_figure(figure, output_dir / f"{filename}.png")

    logger.info(f"Results saved to {output_dir}.")


# %% Main function
def main() -> None:
    for comparison_spec in COMPARISONS:
        run_comparison(**comparison_spec)


if __name__ == "__main__":
    main()
