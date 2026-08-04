#!/usr/bin/env python3
"""Orquestador: ejecuta los pipelines de figuras (src/pipelines/plots/) secuencialmente con logging unificado.

Solo leen los resultados que dejaron en results/ los pipelines de cálculo, así
que este orquestador regenera todas las figuras del TFM en segundos, sin
recalcular ni reentrenar nada.
"""

import argparse
import importlib
import sys
import time
from datetime import datetime

from src.utils.paths import LOGS_DIR
from src.utils.logging_utils import setup_logger

ORCHESTRATOR_LOG_NAME = "orchestrator"

# Lista ordenada de pipelines de figuras: (módulo, descripción)
PLOT_PIPELINES = [
    (
        "src.pipelines.plots.01_clinical_variables_review_plots",
        "Figura: revisión de variables clínicas",
    ),
    ("src.pipelines.plots.03_missing_values_plots", "Figuras: valores faltantes"),
    ("src.pipelines.plots.04_collinearity_plots", "Figuras: colinealidad"),
    (
        "src.pipelines.plots.06_clinical_statistical_plots",
        "Figuras: distribuciones clínicas",
    ),
    (
        "src.pipelines.plots.07_proteomic_statistical_plots",
        "Figuras: distribuciones proteómicas",
    ),
    ("src.pipelines.plots.08_risk_scores_plots", "Figuras: curvas de los risk scores"),
]

# Figuras de las fases de modelado, cuyos datos de entrada no existirían si se
# omitió el modelado al calcular. Se mantienen aparte para poder excluirlas con
# --skip-modelling-plots.
MODELLING_PLOT_PIPELINES = [
    (
        "src.pipelines.plots.09_modelling_phases_plots",
        "Figuras: fases de modelado (09 a 12)",
    ),
    (
        "src.pipelines.plots.13_modality_comparison_plots",
        "Figuras: comparación entre modalidades",
    ),
    ("src.pipelines.plots.14_best_model_plots", "Figuras: análisis del umbral"),
    ("src.pipelines.plots.15_explainability_plots", "Figuras: explicabilidad SHAP"),
]


def parse_args(argv=None) -> argparse.Namespace:
    """Interpreta los argumentos de línea de comandos del orquestador."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--skip-modelling-plots",
        action="store_true",
        help="Omite las figuras de las fases de modelado (09 a 15).",
    )
    return parser.parse_args(argv)


def select_pipelines(args: argparse.Namespace) -> list[tuple[str, str]]:
    """Construye la lista ordenada de pipelines de figuras a ejecutar."""
    pipelines = list(PLOT_PIPELINES)
    if not args.skip_modelling_plots:
        pipelines += MODELLING_PLOT_PIPELINES
    return pipelines


def run_pipeline(module_name: str, description: str, logger) -> float:
    """Importa y ejecuta main() del pipeline, retornando duración en segundos."""
    logger.info("=" * 72)
    logger.info(f"INICIANDO: {description}  ({module_name})")
    logger.info("=" * 72)

    t0 = time.perf_counter()
    try:
        mod = importlib.import_module(module_name)
        mod.main()
        elapsed = time.perf_counter() - t0
        logger.info(f"✓ FINALIZADO: {description}  ({elapsed:.1f} s)")
    except Exception:
        elapsed = time.perf_counter() - t0
        logger.exception(
            f"✗ ERROR en {description} tras {elapsed:.1f} s — abortando ejecución."
        )
        raise
    return elapsed


def main(argv=None) -> None:
    overall_start = time.perf_counter()

    pipelines = select_pipelines(parse_args(argv))

    # ── Logger del orquestador (consola + archivo único) ──────────────────────
    log_dir = LOGS_DIR / "orchestrator"
    log_dir.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    log_file = log_dir / f"run_plots_{timestamp}.log"

    logger = setup_logger(ORCHESTRATOR_LOG_NAME, log_dir=log_dir)

    sys.stdout.flush()
    sys.stderr.flush()

    logger.info(f"Orquestador de pipelines de figuras")
    logger.info(f"Inicio:          {datetime.now().isoformat()}")
    logger.info(f"Log file:        {log_file}")
    logger.info(f"Python:          {sys.version}")
    logger.info(f"Pipelines a ejecutar: {len(pipelines)}")
    logger.info("")

    results: list[tuple[str, str, float, str]] = []

    for module_name, description in pipelines:
        try:
            elapsed = run_pipeline(module_name, description, logger)
            results.append((module_name, description, elapsed, "OK"))
        except Exception:
            results.append((module_name, description, 0.0, "ERROR"))
            break  # Detenemos la ejecución en el primer error

    # ── Resumen final ─────────────────────────────────────────────────────────
    overall_elapsed = time.perf_counter() - overall_start
    logger.info("")
    logger.info(f"Resumen de ejecución de pipelines")
    logger.info(f"{'Pipeline':<45} {'Estado':<8} {'Tiempo':>8}")
    logger.info("-" * 63)
    for module_name, description, elapsed, status in results:
        label = f"{description} ({module_name.split('.')[-1]})"
        time_str = f"{elapsed:.1f} s" if status == "OK" else "—"
        logger.info(f"{label:<45} {status:<8} {time_str:>8}")
    logger.info("-" * 63)
    logger.info(f"{'TOTAL':<45} {'':<8} {overall_elapsed:.1f} s")

    ok_count = sum(1 for _, _, _, s in results if s == "OK")
    logger.info(f"\nPipelines completados: {ok_count} / {len(pipelines)}")

    log_path_msg = f"\nLog completo guardado en: {log_file}"
    if ok_count == len(pipelines):
        logger.info(
            f"✅ Todos los pipelines se ejecutaron correctamente.{log_path_msg}"
        )
    else:
        failed = [d for _, d, _, s in results if s != "OK"]
        logger.error(f"❌ Los siguientes pipelines fallaron: {failed}.{log_path_msg}")
        sys.exit(1)


if __name__ == "__main__":
    main()
