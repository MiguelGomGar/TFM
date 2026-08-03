"""Draw the SHAP figures computed by pipeline 15.

The beeswarm and waterfall charts need the whole explanation object, which
pipeline 15 serialized; the bar and ranking charts are rebuilt from its tables.
"""

from pathlib import Path

from src.config import EXPLAINABILITY_MODELS
from src.models.model_explainability import load_explanation
from src.utils.dataframe_utils import split_feature_blocks
from src.utils.filenames import (
    LOCAL_CASES_FILE,
    RANKING_COMPARISON_FIGURE,
    RANKING_COMPARISON_FILE,
    SHAP_IMPORTANCE_FIGURE,
    SHAP_IMPORTANCE_FILE,
    SHAP_SUMMARY_FIGURE,
    SHAP_WATERFALL_FIGURE,
)
from src.utils.io import read_csv, save_figure, slugify
from src.utils.logging_utils import setup_logger
from src.utils.paths import EXPLAINABILITY_DIR
from src.visualization.model_explainability import (
    plot_ranking_comparison,
    plot_shap_bar,
    plot_shap_summary,
    plot_shap_waterfall,
)

INPUT_DIR = EXPLAINABILITY_DIR
OUTPUT_DIR = EXPLAINABILITY_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    for abbreviation in EXPLAINABILITY_MODELS:
        logger.info(f"Loading the SHAP results of {abbreviation}...")
        importance = read_csv(
            INPUT_DIR / SHAP_IMPORTANCE_FILE.format(model=abbreviation)
        )
        local_cases = read_csv(INPUT_DIR / LOCAL_CASES_FILE.format(model=abbreviation))
        explanation = load_explanation(INPUT_DIR, abbreviation)

        # Cheap to recompute from the feature names, so it is not persisted.
        blocks = split_feature_blocks(importance["Feature"])

        save_figure(
            plot_shap_bar(
                importance, blocks, title=f"{abbreviation} — global SHAP importance"
            ),
            OUTPUT_DIR / SHAP_IMPORTANCE_FIGURE.format(model=abbreviation),
        )

        save_figure(
            plot_shap_summary(explanation, title=f"{abbreviation} — SHAP summary"),
            OUTPUT_DIR / SHAP_SUMMARY_FIGURE.format(model=abbreviation),
        )

        for _, case in local_cases.iterrows():
            if case["Case"] == "Borderline":
                continue
            outcome = "recurrence" if case["True_Label"] == 1 else "no recurrence"
            save_figure(
                plot_shap_waterfall(
                    explanation,
                    case_index=int(case["Test_Index"]),
                    title=(
                        f"{abbreviation} — {case['Case']} "
                        f"(patient #{int(case['Test_Index'])}, actual: {outcome})"
                    ),
                ),
                OUTPUT_DIR
                / SHAP_WATERFALL_FIGURE.format(
                    model=abbreviation, case=slugify(case["Case"])
                ),
            )

    logger.info("Plotting the cross-model ranking comparison...")
    ranking_comparison = read_csv(INPUT_DIR / RANKING_COMPARISON_FILE)
    save_figure(
        plot_ranking_comparison(ranking_comparison),
        OUTPUT_DIR / RANKING_COMPARISON_FIGURE,
    )


if __name__ == "__main__":
    main()
