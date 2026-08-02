"""Phase 3d: SHAP explainability of the top three models (EN, SVM, MLP).

Every earlier phase reports how well the models discriminate, never why they
predict what they predict. This phase derives Shapley additive explanations
from the fitted multimodal pipelines and answers three questions:

1. Which predictors drive each model, and do the three best models agree on
   the ranking, or do they reach a similar AUC by different routes?
2. Why was an individual patient classified the way they were? Four archetypes
   are explained with waterfall plots, including the confident false negative,
   which is the clinically costliest error.
3. How is the attribution split between the clinical variables and the
   proteins? This is the attribution-based counterpart to the AUC-based
   modality comparison of phase 13, addressing the secondary objective of
   whether proteomic information adds predictive value.

The analysis is restricted to the multimodal arm, the only one where the block
comparison is possible at all. The fitted pipelines are reloaded as-is from
disk; nothing is retrained.

Note on scope: the 35 predictors explained here already survived the Elastic
Net selection of phases 1b and 2, so these figures describe importance *within
the final model*, not evidence of incremental value over a clinical-only model
— that evidence comes from the modality comparison.
"""

from pathlib import Path

import pandas as pd

from src.config import EXPLAINABILITY_MODELS, TARGET_VARIABLE
from src.models.data_preprocessing import encode_target_variable
from src.models.model_explainability import (
    build_ranking_comparison_table,
    compute_rank_correlations,
    explain_model,
)
from src.utils.io import read_parquet, save_csv, save_figure, slugify
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    BEST_MODEL_DIR,
    CLEANED_MULTIMODAL_DATA_PATH,
    EXPLAINABILITY_DIR,
    MULTIMODAL_MODELS_DIR,
)
from src.visualization.model_explainability import (
    plot_ranking_comparison,
    plot_shap_bar,
    plot_shap_summary,
    plot_shap_waterfall,
)

INPUT_FILE = CLEANED_MULTIMODAL_DATA_PATH
MODEL_SOURCE_DIR = MULTIMODAL_MODELS_DIR
THRESHOLD_SOURCE_FILE = BEST_MODEL_DIR / "threshold_info.csv"
OUTPUT_DIR = EXPLAINABILITY_DIR

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading the multimodal dataset from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)
    y = encode_target_variable(df, TARGET_VARIABLE)

    results = {
        abbreviation: explain_model(
            abbreviation, MODEL_SOURCE_DIR, THRESHOLD_SOURCE_FILE, df, y, logger=logger
        )
        for abbreviation in EXPLAINABILITY_MODELS
    }

    # ------------------------------------------------------------------
    # Save per-model results
    # ------------------------------------------------------------------
    for abbreviation, result in results.items():
        save_csv(result["importance"], OUTPUT_DIR / f"shap_importance_{abbreviation}.csv")
        save_csv(result["shap_values_table"], OUTPUT_DIR / f"shap_values_{abbreviation}.csv")
        save_csv(result["local_cases"], OUTPUT_DIR / f"local_cases_{abbreviation}.csv")

        figure = plot_shap_bar(
            result["importance"],
            result["blocks"],
            title=f"{abbreviation} — global SHAP importance",
        )
        save_figure(figure, OUTPUT_DIR / f"shap_importance_{abbreviation}.png")

        figure = plot_shap_summary(
            result["explanation"], title=f"{abbreviation} — SHAP summary"
        )
        save_figure(figure, OUTPUT_DIR / f"shap_summary_{abbreviation}.png")

        for _, case in result["local_cases"].iterrows():
            if case["Case"] == "Borderline":
                continue
            figure = plot_shap_waterfall(
                result["explanation"],
                case_index=int(case["Test_Index"]),
                title=(
                    f"{abbreviation} — {case['Case']} (patient #{int(case['Test_Index'])}, "
                    f"actual: {'recurrence' if case['True_Label'] == 1 else 'no recurrence'})"
                ),
            )
            save_figure(
                figure,
                OUTPUT_DIR / f"shap_waterfall_{abbreviation}_{slugify(case['Case'])}.png",
            )

    # ------------------------------------------------------------------
    # Cross-model comparisons
    # ------------------------------------------------------------------
    importance_by_model = {
        abbreviation: result["importance"] for abbreviation, result in results.items()
    }

    ranking_comparison = build_ranking_comparison_table(importance_by_model)
    save_csv(ranking_comparison, OUTPUT_DIR / "ranking_comparison.csv")
    save_figure(plot_ranking_comparison(ranking_comparison), OUTPUT_DIR / "ranking_comparison.png")

    # Reported as a table only: three pairwise coefficients do not warrant a
    # figure of their own.
    rank_correlations = compute_rank_correlations(importance_by_model)
    save_csv(rank_correlations, OUTPUT_DIR / "rank_correlations.csv")
    for _, row in rank_correlations.iterrows():
        logger.info(
            f"Ranking agreement {row['Model_A']} vs {row['Model_B']}: "
            f"rho = {row['Spearman_Rho']:.3f} (p = {row['P_Value']:.3g})."
        )

    # ------------------------------------------------------------------
    # Clinical vs proteomic block contribution: the secondary-objective output
    # ------------------------------------------------------------------
    block_contribution = pd.concat(
        [result["block_contribution"] for result in results.values()], ignore_index=True
    )
    save_csv(block_contribution, OUTPUT_DIR / "block_contribution.csv")

    validation = pd.concat(
        [result["validation"] for result in results.values()], ignore_index=True
    )
    save_csv(validation, OUTPUT_DIR / "shap_validation.csv")
    if not validation["Passed"].all():
        logger.error("Additivity check FAILED for at least one model; see shap_validation.csv.")

    logger.info(f"SHAP explainability results saved to {OUTPUT_DIR}.")


if __name__ == "__main__":
    main()
