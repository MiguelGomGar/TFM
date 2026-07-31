"""Phase 3d: SHAP explainability of the top three models (EN, SVM, MLP).

Every earlier phase reports how well the models discriminate, never why they
predict what they predict. This phase derives Shapley additive explanations
from the fitted multimodal pipelines and answers three questions:

1. Which predictors drive each model, and do the three best models agree on
   the ranking, or do they reach a similar AUC by different routes?
2. Why was an individual patient classified the way they were? Four archetypes
   plus a borderline case are explained with waterfall plots, including the
   confident false negative, which is the clinically costliest error.
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

import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split

from src.config import (
    EXPLAINABILITY_MODELS,
    SEED,
    SHAP_BACKGROUND_SIZE,
    TARGET_VARIABLE,
    TEST_SIZE,
)
from src.models.best_model import (
    compute_youden_threshold,
    get_model_feature_columns,
    load_fitted_pipeline,
)
from src.models.data_preprocessing import encode_target_variable
from src.models.model_evaluation import get_decision_scores
from src.models.model_explainability import (
    build_background_set,
    build_block_contribution_table,
    build_explainer,
    build_global_importance_table,
    build_ranking_comparison_table,
    build_shap_values_table,
    check_shap_additivity,
    check_shap_dimensions,
    compute_rank_correlations,
    compute_shap_values,
    get_explained_output,
    get_transformed_matrices,
    select_local_cases,
)
from src.utils.dataframe_utils import split_feature_blocks
from src.utils.io import read_csv, read_parquet, save_csv, save_figure
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    BEST_MODEL_DIR,
    CLEANED_MULTIMODAL_DATA_PATH,
    EXPLAINABILITY_DIR,
    MULTIMODAL_MODELS_DIR,
)
from src.visualization.model_explainability import (
    plot_block_contribution,
    plot_rank_correlation_heatmap,
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


# %% Helpers
def _load_youden_threshold(abbreviation: str, y_test, y_score) -> float:
    """Reuse the Youden cut-off already chosen by the threshold analysis.

    Reading it back keeps the archetypes below consistent with the confusion
    matrices of pipeline 14: a patient labeled a false negative here is the
    same patient counted as a false negative there. Recomputed on the fly if
    that phase has not been run yet.

    Parameters
    ----------
    abbreviation : str
        Model abbreviation.
    y_test : array-like
        Binary external validation target.
    y_score : array-like
        Continuous decision score from model_evaluation.get_decision_scores.

    Returns
    -------
    float
        The Youden-optimal decision threshold.
    """
    if THRESHOLD_SOURCE_FILE.exists():
        thresholds = read_csv(THRESHOLD_SOURCE_FILE)
        optimal = thresholds[
            (thresholds["Model"] == abbreviation)
            & (thresholds["Scenario"].str.startswith("Optimal"))
        ]
        if not optimal.empty:
            return float(optimal["Threshold"].iloc[0])

    logger.warning(
        f"[{abbreviation}] No stored threshold in {THRESHOLD_SOURCE_FILE}; recomputing it."
    )
    return compute_youden_threshold(y_test, y_score)["threshold"]


def _case_slug(case_name: str) -> str:
    """Turn a local-case label into a filename-safe slug.

    Parameters
    ----------
    case_name : str
        Case label, e.g. 'Confident FN'.

    Returns
    -------
    str
        Lowercase, underscore-separated slug, e.g. 'confident_fn'.
    """
    return case_name.lower().replace(" ", "_")


# %% Per-model analysis
def _explain_model(abbreviation: str, df: pd.DataFrame, y: pd.Series) -> dict:
    """Run the full SHAP analysis for one fitted pipeline.

    Parameters
    ----------
    abbreviation : str
        Model abbreviation, matching the ``optimized_{abbreviation}.joblib``
        filename saved by the multimodal modelling phase.
    df : pd.DataFrame
        Cleaned multimodal dataset (predictors + target columns).
    y : pd.Series
        Encoded binary target, aligned to df's index.

    Returns
    -------
    dict
        Keys 'explanation', 'importance', 'blocks', 'block_contribution',
        'shap_values_table', 'local_cases' and 'validation'.
    """
    logger.info(f"Loading the fitted {abbreviation} pipeline from {MODEL_SOURCE_DIR}...")
    model = load_fitted_pipeline(MODEL_SOURCE_DIR, abbreviation)
    feature_columns = get_model_feature_columns(model)

    # Same SEED, TEST_SIZE and stratification as the multimodal modelling phase,
    # so the reconstructed partition matches it record by record. Unlike the
    # threshold analysis, the training half is needed here: it is the reference
    # distribution the SHAP contributions are measured against.
    X = df[feature_columns]
    X_train, X_test, _, y_test = train_test_split(
        X, y, test_size=TEST_SIZE, random_state=SEED, shuffle=True, stratify=y
    )
    logger.info(
        f"[{abbreviation}] Reconstructed the partition: {X_train.shape[0]} training and "
        f"{X_test.shape[0]} external validation patients ({y_test.mean():.1%} recurrence)."
    )

    # ------------------------------------------------------------------
    # 1. SHAP values on the transformed matrix
    # ------------------------------------------------------------------
    X_train_transformed, X_test_transformed, feature_names = get_transformed_matrices(
        model, X_train, X_test
    )
    background = build_background_set(
        X_train_transformed, size=SHAP_BACKGROUND_SIZE, seed=SEED, logger=logger
    )
    explainer, kind = build_explainer(model, abbreviation, background, logger=logger)
    explanation = compute_shap_values(explainer, X_test_transformed, feature_names, logger=logger)

    # ------------------------------------------------------------------
    # 2. Quality control, before anything is derived from the values
    # ------------------------------------------------------------------
    check_shap_dimensions(explanation, X_test_transformed.shape[0], feature_names)
    output_function, output_label = get_explained_output(model, abbreviation)
    validation = check_shap_additivity(
        explanation, output_function(X_test_transformed), abbreviation, kind
    )
    logger.info(
        f"[{abbreviation}] Additivity on {output_label}: max error "
        f"{validation['Max_Abs_Error'].iloc[0]:.2e} vs tolerance "
        f"{validation['Tolerance'].iloc[0]:.0e} -> "
        f"{'PASSED' if validation['Passed'].iloc[0] else 'FAILED'}; "
        f"{validation['Zero_Fraction'].iloc[0]:.1%} of the values are exactly zero."
    )
    if not validation["Passed"].iloc[0]:
        logger.error(
            f"[{abbreviation}] SHAP values do not reconstruct the model output; "
            f"treat this model's explanations as unreliable."
        )
    if validation["Zero_Fraction"].iloc[0] > 0.05:
        logger.warning(
            f"[{abbreviation}] An unexpectedly large share of the SHAP values is exactly "
            f"zero, which suggests the explainer truncated the attributions; the "
            f"importance ranking and the block shares would be biased."
        )

    # ------------------------------------------------------------------
    # 3. Global importance and block contribution
    # ------------------------------------------------------------------
    importance = build_global_importance_table(explanation, abbreviation)
    blocks = split_feature_blocks(feature_names)
    if not blocks["Clinical"] or not blocks["Proteomic"]:
        raise ValueError(
            f"[{abbreviation}] Expected both feature blocks to be non-empty, got "
            f"{len(blocks['Clinical'])} clinical and {len(blocks['Proteomic'])} proteomic."
        )
    logger.info(
        f"[{abbreviation}] Feature blocks: {len(blocks['Clinical'])} clinical, "
        f"{len(blocks['Proteomic'])} proteomic (total {len(feature_names)})."
    )
    block_contribution = build_block_contribution_table(explanation, abbreviation, blocks)
    for _, row in block_contribution.iterrows():
        logger.info(
            f"[{abbreviation}] {row['Block']}: {row['Share']:.1%} of total |SHAP| across "
            f"{int(row['N_Features'])} predictors "
            f"({row['Mean_Abs_SHAP_Per_Feature']:.4f} per predictor)."
        )

    # ------------------------------------------------------------------
    # 4. Local cases, selected in the threshold analysis' score space
    # ------------------------------------------------------------------
    y_score = get_decision_scores(model, X_test)
    threshold = _load_youden_threshold(abbreviation, y_test, y_score)
    local_cases = select_local_cases(y_test, y_score, threshold)
    logger.info(
        f"[{abbreviation}] Selected {len(local_cases)} local cases at threshold "
        f"{threshold:.4f}: {', '.join(local_cases['Case'])}."
    )

    return {
        "explanation": explanation,
        "importance": importance,
        "blocks": blocks,
        "block_contribution": block_contribution,
        "shap_values_table": build_shap_values_table(explanation, y_test),
        "local_cases": local_cases,
        "validation": validation,
    }


# %% Main function
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading the multimodal dataset from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)
    y = encode_target_variable(df, TARGET_VARIABLE)

    results = {
        abbreviation: _explain_model(abbreviation, df, y)
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
                OUTPUT_DIR / f"shap_waterfall_{abbreviation}_{_case_slug(case['Case'])}.png",
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

    rank_correlations = compute_rank_correlations(importance_by_model)
    save_csv(rank_correlations, OUTPUT_DIR / "rank_correlations.csv")
    save_figure(
        plot_rank_correlation_heatmap(rank_correlations, list(results)),
        OUTPUT_DIR / "rank_correlations.png",
    )
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
    save_figure(
        plot_block_contribution(
            block_contribution, title="Clinical vs proteomic SHAP contribution"
        ),
        OUTPUT_DIR / "block_contribution.png",
    )

    validation = pd.concat(
        [result["validation"] for result in results.values()], ignore_index=True
    )
    save_csv(validation, OUTPUT_DIR / "shap_validation.csv")
    if not validation["Passed"].all():
        logger.error("Additivity check FAILED for at least one model; see shap_validation.csv.")

    logger.info(f"SHAP explainability results saved to {OUTPUT_DIR}.")


if __name__ == "__main__":
    main()
