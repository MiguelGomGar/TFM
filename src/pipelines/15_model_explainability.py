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
disk; nothing is retrained. The SHAP figures are drawn from the tables and the
serialized explanations saved here by
src/pipelines/plots/15_explainability_plots.py.

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
    save_explanation,
)
from src.utils.filenames import (
    BLOCK_CONTRIBUTION_FILE,
    LOCAL_CASES_FILE,
    RANKING_COMPARISON_FILE,
    RANK_CORRELATIONS_FILE,
    SHAP_IMPORTANCE_FILE,
    SHAP_VALIDATION_FILE,
    SHAP_VALUES_FILE,
)
from src.utils.io import read_parquet, save_csv
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    BEST_MODEL_DIR,
    CLEANED_MULTIMODAL_DATA_PATH,
    EXPLAINABILITY_DIR,
    MULTIMODAL_MODELS_DIR,
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
        save_csv(
            result["importance"],
            OUTPUT_DIR / SHAP_IMPORTANCE_FILE.format(model=abbreviation),
        )
        save_csv(
            result["shap_values_table"],
            OUTPUT_DIR / SHAP_VALUES_FILE.format(model=abbreviation),
        )
        save_csv(
            result["local_cases"], OUTPUT_DIR / LOCAL_CASES_FILE.format(model=abbreviation)
        )

        # The beeswarm and waterfall figures need the explanation object itself,
        # which no table can reconstruct, so it is serialized for the plotting
        # pipeline instead of being recomputed there.
        save_explanation(result["explanation"], OUTPUT_DIR, abbreviation)

    # ------------------------------------------------------------------
    # Cross-model comparisons
    # ------------------------------------------------------------------
    importance_by_model = {
        abbreviation: result["importance"] for abbreviation, result in results.items()
    }

    ranking_comparison = build_ranking_comparison_table(importance_by_model)
    save_csv(ranking_comparison, OUTPUT_DIR / RANKING_COMPARISON_FILE)

    # Reported as a table only: three pairwise coefficients do not warrant a
    # figure of their own.
    rank_correlations = compute_rank_correlations(importance_by_model)
    save_csv(rank_correlations, OUTPUT_DIR / RANK_CORRELATIONS_FILE)
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
    save_csv(block_contribution, OUTPUT_DIR / BLOCK_CONTRIBUTION_FILE)

    validation = pd.concat(
        [result["validation"] for result in results.values()], ignore_index=True
    )
    save_csv(validation, OUTPUT_DIR / SHAP_VALIDATION_FILE)
    if not validation["Passed"].all():
        logger.error("Additivity check FAILED for at least one model; see shap_validation.csv.")

    logger.info(f"SHAP explainability results saved to {OUTPUT_DIR}.")


if __name__ == "__main__":
    main()
