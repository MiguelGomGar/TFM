#!/usr/bin/env python3
"""Orquestador: ejecuta los pipelines de cálculo (src/pipelines/calculations/) secuencialmente con logging unificado."""

import argparse
import importlib
import sys
import time
from datetime import datetime

from src.utils.paths import LOGS_DIR
from src.utils.logging_utils import setup_logger

ORCHESTRATOR_LOG_NAME = "orchestrator"

# Lista ordenada de pipelines de cálculo: (módulo, descripción)
PIPELINES = [
    (
        "src.pipelines.calculations.01_clinical_variables_review",
        "Revisión de variables clínicas",
    ),
    ("src.pipelines.calculations.02_data_collection", "Recolección de datos clínicos"),
    (
        "src.pipelines.calculations.03_missing_values_reporting",
        "Análisis de valores faltantes",
    ),
    ("src.pipelines.calculations.04_collinearity_reporting", "Colinealidad"),
    ("src.pipelines.calculations.05_clinical_data_cleaning", "Limpieza de datos clínicos"),
    (
        "src.pipelines.calculations.06_clinical_data_statistical_reporting",
        "Reporte estadístico clínico",
    ),
    (
        "src.pipelines.calculations.07_proteomic_data_statistical_reporting",
        "Reporte estadístico proteómico",
    ),
    (
        "src.pipelines.calculations.08_risk_scores_validation",
        "Validación de risk scores",
    ),
]

# Pipelines de modelado. Son órdenes de magnitud más lentos que los anteriores
# (búsqueda aleatoria de hiperparámetros sobre ocho modelos y cuatro fases), así
# que se mantienen en una lista aparte y se pueden omitir con --skip-modelling.
MODELLING_PIPELINES = [
    ("src.pipelines.calculations.09_clinical_modelling", "Modelado clínico (fase 1)"),
    (
        "src.pipelines.calculations.10_clinical_filtered_modelling",
        "Modelado clínico filtrado (fase 1b)",
    ),
    ("src.pipelines.calculations.11_proteomic_modelling", "Modelado proteómico (fase 2)"),
    ("src.pipelines.calculations.12_multimodal_modelling", "Modelado multimodal (fase 3)"),
    (
        "src.pipelines.calculations.13_modality_comparison",
        "Comparación clínico vs multimodal",
    ),
    (
        "src.pipelines.calculations._publication_tables",
        "Tablas de resultados para publicación",
    ),
    (
        "src.pipelines.calculations.14_best_model_threshold_analysis",
        "Análisis de sensibilidad al umbral de los tres mejores modelos",
    ),
    (
        "src.pipelines.calculations.15_model_explainability",
        "Explicabilidad SHAP de los tres mejores modelos",
    ),
]


def parse_args(argv=None) -> argparse.Namespace:
    """Interpreta los argumentos de línea de comandos del orquestador."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--skip-modelling",
        action="store_true",
        help="Omite las fases de modelado (09 a 15), que son las lentas.",
    )
    return parser.parse_args(argv)


def select_pipelines(args: argparse.Namespace) -> list[tuple[str, str]]:
    """Construye la lista ordenada de pipelines de cálculo a ejecutar."""
    pipelines = list(PIPELINES)
    if not args.skip_modelling:
        pipelines += MODELLING_PIPELINES
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
    log_file = log_dir / f"run_calculations_{timestamp}.log"

    logger = setup_logger(ORCHESTRATOR_LOG_NAME, log_dir=log_dir)

    sys.stdout.flush()
    sys.stderr.flush()

    logger.info(f"Orquestador de pipelines de cálculo")
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
