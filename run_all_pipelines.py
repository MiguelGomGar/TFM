#!/usr/bin/env python3
"""Orchestrator: runs all pipelines sequentially with unified logging."""

import importlib
import sys
import time
from datetime import datetime

from src.utils.paths import LOGS_DIR
from src.utils.logging_utils import setup_logger

ORCHESTRATOR_LOG_NAME = "orchestrator"

# Ordered list of pipelines: (module, description)
PIPELINES = [
    ("src.pipelines.01_clinical_variables_review", "Clinical variables review"),
    ("src.pipelines.02_data_collection", "Clinical data collection"),
    ("src.pipelines.03_missing_values_reporting", "Missing values analysis"),
    ("src.pipelines.04_collinearity_reporting", "Collinearity"),
    ("src.pipelines.05_clinical_data_cleaning", "Clinical data cleaning"),
    (
        "src.pipelines.06_clinical_data_statistical_reporting",
        "Clinical statistical reporting",
    ),
    (
        "src.pipelines.07_proteomic_data_statistical_reporting",
        "Proteomic statistical reporting",
    ),
    ("src.pipelines.08_risk_scores_validation", "Risk scores validation"),
]

# Modelling pipelines. They are orders of magnitude slower than the ones
# above (random hyperparameter search over eight models and four phases),
# so they are kept in a separate list: to skip them, simply don't
# concatenate it to PIPELINES in main().
MODELLING_PIPELINES = [
    ("src.pipelines.09_clinical_modelling", "Clinical modelling (phase 1)"),
    (
        "src.pipelines.10_clinical_filtered_modelling",
        "Filtered clinical modelling (phase 1b)",
    ),
    ("src.pipelines.11_proteomic_modelling", "Proteomic modelling (phase 2)"),
    ("src.pipelines.12_multimodal_modelling", "Multimodal modelling (phase 3)"),
    ("src.pipelines.13_modality_comparison", "Clinical vs multimodal comparison"),
    ("src.pipelines.14_publication_tables", "Publication result tables"),
    (
        "src.pipelines.15_ensemble_prediction_analysis",
        "Ensemble analysis of the best multimodal models",
    ),
    (
        "src.pipelines.16_best_model_threshold_analysis",
        "Threshold sensitivity analysis of the best model (MLP)",
    ),
]


def run_pipeline(module_name: str, description: str, logger) -> float:
    """Import and run the pipeline's main(), returning duration in seconds."""
    logger.info("=" * 72)
    logger.info(f"STARTING: {description}  ({module_name})")
    logger.info("=" * 72)

    t0 = time.perf_counter()
    try:
        mod = importlib.import_module(module_name)
        mod.main()
        elapsed = time.perf_counter() - t0
        logger.info(f"✓ FINISHED: {description}  ({elapsed:.1f} s)")
    except Exception:
        elapsed = time.perf_counter() - t0
        logger.exception(
            f"✗ ERROR in {description} after {elapsed:.1f} s — aborting run."
        )
        raise
    return elapsed


def main() -> None:
    overall_start = time.perf_counter()

    pipelines = PIPELINES + MODELLING_PIPELINES

    # ── Orchestrator logger (console + single file) ──────────────────────
    log_dir = LOGS_DIR / "orchestrator"
    log_dir.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    log_file = log_dir / f"run_all_{timestamp}.log"

    logger = setup_logger(ORCHESTRATOR_LOG_NAME, log_dir=log_dir)

    # Also redirect stdout/stderr to the log file (in addition to the
    # console) to keep a complete record.
    sys.stdout.flush()
    sys.stderr.flush()

    logger.info("Pipeline orchestrator")
    logger.info(f"Start:           {datetime.now().isoformat()}")
    logger.info(f"Log file:        {log_file}")
    logger.info(f"Python:          {sys.version}")
    logger.info(f"Pipelines to run: {len(pipelines)}")
    logger.info("")

    results: list[tuple[str, str, float, str]] = []

    for module_name, description in pipelines:
        try:
            elapsed = run_pipeline(module_name, description, logger)
            results.append((module_name, description, elapsed, "OK"))
        except Exception:
            results.append((module_name, description, 0.0, "ERROR"))
            break  # Stop the run on the first error

    # ── Final summary ─────────────────────────────────────────────────────────
    overall_elapsed = time.perf_counter() - overall_start
    logger.info("")
    logger.info("Pipeline run summary")
    logger.info(f"{'Pipeline':<45} {'Status':<8} {'Time':>8}")
    logger.info("-" * 63)
    for module_name, description, elapsed, status in results:
        label = f"{description} ({module_name.split('.')[-1]})"
        time_str = f"{elapsed:.1f} s" if status == "OK" else "—"
        logger.info(f"{label:<45} {status:<8} {time_str:>8}")
    logger.info("-" * 63)
    logger.info(f"{'TOTAL':<45} {'':<8} {overall_elapsed:.1f} s")

    ok_count = sum(1 for _, _, _, s in results if s == "OK")
    logger.info(f"\nPipelines completed: {ok_count} / {len(pipelines)}")

    log_path_msg = f"\nFull log saved to: {log_file}"
    if ok_count == len(pipelines):
        logger.info(
            f"✅ All pipelines ran successfully.{log_path_msg}"
        )
    else:
        failed = [d for _, d, _, s in results if s != "OK"]
        logger.error(f"❌ The following pipelines failed: {failed}.{log_path_msg}")
        sys.exit(1)


if __name__ == "__main__":
    main()
