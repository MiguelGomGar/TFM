"""Compare the multi-modal models against the clinical-only ones.

Both sets of metrics come from the two arms of phase 3, trained on the same
matched subcohort and the same partition, so the difference isolates the
contribution of the proteomic panel. No model is refitted here: the comparison
is rebuilt from the metrics tables, so the figures can be regenerated cheaply.
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
    CLINICAL_MODELS_MATCHED_DIR,
    MODALITY_COMPARISON_DIR,
    MULTIMODAL_MODELS_DIR,
)
from src.visualization.model_evaluation import plot_modality_comparison

CLINICAL_METRICS_FILE = CLINICAL_MODELS_MATCHED_DIR / "models_metrics.csv"
MULTIMODAL_METRICS_FILE = MULTIMODAL_MODELS_DIR / "models_metrics.csv"
OUTPUT_DIR = MODALITY_COMPARISON_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading clinical metrics from {CLINICAL_METRICS_FILE}...")
    clinical_metrics = read_csv(CLINICAL_METRICS_FILE)

    logger.info(f"Loading multimodal metrics from {MULTIMODAL_METRICS_FILE}...")
    multimodal_metrics = read_csv(MULTIMODAL_METRICS_FILE)

    logger.info("Building the comparison tables...")
    comparison = build_modality_comparison_table(clinical_metrics, multimodal_metrics)
    deltas = build_modality_delta_table(comparison)
    save_csv(deltas, OUTPUT_DIR / "modality_comparison.csv")

    logger.info("Plotting the comparison for each model...")
    for abbreviation in MODEL_ORDER:
        model_comparison = comparison[comparison["Model"] == abbreviation]
        if model_comparison.empty:
            logger.warning(f"No results found for model {abbreviation}; skipping.")
            continue

        filename = f"comparison_{abbreviation.lower()}"
        save_csv(model_comparison, OUTPUT_DIR / f"{filename}.csv")
        figure = plot_modality_comparison(
            model_comparison, model_name=get_display_name(abbreviation)
        )
        save_figure(figure, OUTPUT_DIR / f"{filename}.png")

    improved = deltas[deltas["Delta"] > 0]
    logger.info(
        f"The multimodal arm improved on {len(improved)} of {len(deltas)} "
        "model/metric combinations."
    )
    logger.info(f"Results saved to {OUTPUT_DIR}.")


if __name__ == "__main__":
    main()
