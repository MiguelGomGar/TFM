#!/usr/bin/env python3
"""Run the calculation pipelines (src/pipelines/calculations/) in order.

Every table, model and dataset the project reports is produced here. The
figures are drawn separately by run_plots.py, which only reads what this script
leaves in results/.
"""

from src.utils.orchestration import Pipeline, parse_args, run_orchestrator

# Ordered analysis steps: each one consumes what the previous one wrote.
PIPELINES = [
    Pipeline(
        "src.pipelines.calculations.01_clinical_variables_review",
        "Clinical variables review",
    ),
    Pipeline(
        "src.pipelines.calculations.02_data_collection",
        "Clinical data collection",
    ),
    Pipeline(
        "src.pipelines.calculations.03_missing_values_reporting",
        "Missing values analysis",
    ),
    Pipeline(
        "src.pipelines.calculations.04_collinearity_reporting",
        "Collinearity analysis",
    ),
    Pipeline(
        "src.pipelines.calculations.05_clinical_data_cleaning",
        "Clinical data cleaning",
    ),
    Pipeline(
        "src.pipelines.calculations.06_clinical_data_statistical_reporting",
        "Clinical statistical report",
    ),
    Pipeline(
        "src.pipelines.calculations.07_proteomic_data_statistical_reporting",
        "Proteomic statistical report",
    ),
    Pipeline(
        "src.pipelines.calculations.08_risk_scores_validation",
        "Risk scores validation",
    ),
]

# Modelling steps. These are orders of magnitude slower than the ones above (a
# randomized hyperparameter search over eight models and five phases), so they
# are kept apart and can be left out with --skip-modelling.
MODELLING_PIPELINES = [
    Pipeline(
        "src.pipelines.calculations.09_clinical_modelling",
        "Clinical modelling (phase 1)",
    ),
    Pipeline(
        "src.pipelines.calculations.10_clinical_filtered_modelling",
        "Filtered clinical modelling (phase 1b)",
    ),
    Pipeline(
        "src.pipelines.calculations.11_proteomic_modelling",
        "Proteomic modelling (phase 2)",
    ),
    Pipeline(
        "src.pipelines.calculations.12_multimodal_modelling",
        "Multimodal modelling (phase 3)",
    ),
    Pipeline(
        "src.pipelines.calculations.13_modality_comparison",
        "Clinical vs multimodal comparison",
    ),
    Pipeline(
        "src.pipelines.calculations.14_best_model_threshold_analysis",
        "Threshold sensitivity of the three best models",
    ),
    Pipeline(
        "src.pipelines.calculations.15_publication_tables",
        "Publication-ready result tables",
    ),
]


def main(argv: list[str] | None = None) -> None:
    """Run the calculation pipelines, or only the fast ones if asked.

    Parameters
    ----------
    argv : list of str, optional
        Command-line arguments. Defaults to sys.argv.

    Returns
    -------
    None
    """
    args = parse_args(
        description=__doc__,
        skip_flag="--skip-modelling",
        skip_help="Skip the modelling phases (09 to 15), which are the slow ones.",
        argv=argv,
    )
    pipelines = list(PIPELINES)
    if not args.skip_modelling:
        pipelines += MODELLING_PIPELINES

    run_orchestrator(
        pipelines,
        log_name="run_calculations",
        title="Calculation pipeline orchestrator",
    )


if __name__ == "__main__":
    main()
