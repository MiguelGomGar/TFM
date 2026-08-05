#!/usr/bin/env python3
"""Run the figure pipelines (src/pipelines/plots/) in order.

These only read the tables the calculation pipelines left in results/, so every
figure in the project can be redrawn in minutes without recomputing or
refitting anything.
"""

from src.utils.orchestration import Pipeline, parse_args, run_orchestrator

# Ordered figure steps, numbered after the calculation pipeline they draw.
PLOT_PIPELINES = [
    Pipeline(
        "src.pipelines.plots.01_clinical_variables_review_plots",
        "Figure: clinical variables review",
    ),
    Pipeline(
        "src.pipelines.plots.03_missing_values_plots",
        "Figures: missing values",
    ),
    Pipeline(
        "src.pipelines.plots.04_collinearity_plots",
        "Figures: collinearity",
    ),
    Pipeline(
        "src.pipelines.plots.06_clinical_statistical_plots",
        "Figures: clinical distributions",
    ),
    Pipeline(
        "src.pipelines.plots.07_proteomic_statistical_plots",
        "Figures: proteomic distributions",
    ),
    Pipeline(
        "src.pipelines.plots.08_risk_scores_plots",
        "Figures: risk score curves",
    ),
]

# Figures of the modelling phases, whose input tables would not exist if the
# modelling was skipped when calculating. Kept apart so they can be left out
# with --skip-modelling-plots.
MODELLING_PLOT_PIPELINES = [
    Pipeline(
        "src.pipelines.plots.09_modelling_phases_plots",
        "Figures: modelling phases (09 to 12)",
    ),
    Pipeline(
        "src.pipelines.plots.13_modality_comparison_plots",
        "Figures: modality comparison",
    ),
    Pipeline(
        "src.pipelines.plots.14_best_model_plots",
        "Figures: threshold analysis",
    ),
]


def main(argv: list[str] | None = None) -> None:
    """Run the figure pipelines, or only the non-modelling ones if asked.

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
        skip_flag="--skip-modelling-plots",
        skip_help="Skip the figures of the modelling phases (09 to 14).",
        argv=argv,
    )
    pipelines = list(PLOT_PIPELINES)
    if not args.skip_modelling_plots:
        pipelines += MODELLING_PLOT_PIPELINES

    run_orchestrator(
        pipelines,
        log_name="run_plots",
        title="Figure pipeline orchestrator",
    )


if __name__ == "__main__":
    main()
