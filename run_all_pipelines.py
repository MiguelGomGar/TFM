#!/usr/bin/env python3
"""Orchestrator: ejecuta todos los pipelines secuencialmente con logging unificado."""

import argparse
import importlib
import sys
import time
from datetime import datetime
from pathlib import Path

from src.utils.paths import LOGS_DIR
from src.utils.logging_utils import setup_logger

ORCHESTRATOR_LOG_NAME = "orchestrator"

# Lista ordenada de pipelines: (módulo, descripción)
PIPELINES = [
    ("src.pipelines.01_clinical_variables_review", "Revisión de variables clínicas"),
    ("src.pipelines.02_data_collection", "Recolección de datos clínicos"),
    ("src.pipelines.03_missing_values_reporting", "Análisis de valores faltantes"),
    ("src.pipelines.04_collinearity_reporting", "Colinealidad"),
    ("src.pipelines.05_clinical_data_cleaning", "Limpieza de datos clínicos"),
    (
        "src.pipelines.06_clinical_data_statistical_reporting",
        "Reporte estadístico clínico",
    ),
    (
        "src.pipelines.07_proteomic_data_statistical_reporting",
        "Reporte estadístico proteómico",
    ),
    ("src.pipelines.08_risk_scores_validation", "Validación de risk scores"),
]

# Pipelines de modelado. Son órdenes de magnitud más lentos que los anteriores
# (búsqueda aleatoria de hiperparámetros sobre ocho modelos y cuatro fases), así
# que se mantienen en una lista aparte y se pueden omitir con --skip-modelling.
MODELLING_PIPELINES = [
    ("src.pipelines.09_clinical_modelling", "Modelado clínico (fase 1)"),
    (
        "src.pipelines.10_clinical_filtered_modelling",
        "Modelado clínico filtrado (fase 1b)",
    ),
    ("src.pipelines.11_proteomic_modelling", "Modelado proteómico (fase 2)"),
    ("src.pipelines.12_multimodal_modelling", "Modelado multimodal (fase 3)"),
    ("src.pipelines.13_modality_comparison", "Comparación clínico vs multimodal"),
    ("src.pipelines._publication_tables", "Tablas de resultados para publicación"),
    (
        "src.pipelines.14_best_model_threshold_analysis",
        "Análisis de sensibilidad al umbral de los tres mejores modelos",
    ),
    (
        "src.pipelines.15_model_explainability",
        "Explicabilidad SHAP de los tres mejores modelos",
    ),
]

# Pipelines de figuras. Solo leen los resultados que dejaron en results/ los
# pipelines anteriores, así que --only-plots regenera todas las figuras del TFM
# en segundos, sin recalcular ni reentrenar nada. Se dividen en dos listas para
# que --skip-modelling omita también las figuras de las fases de modelado, cuyos
# datos de entrada no existirían.
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
        "--skip-modelling",
        action="store_true",
        help="Omite las fases de modelado (09 a 15), que son las lentas.",
    )
    parser.add_argument(
        "--skip-plots",
        action="store_true",
        help="Calcula y guarda los resultados, sin generar ninguna figura.",
    )
    parser.add_argument(
        "--only-plots",
        action="store_true",
        help="Regenera solo las figuras, a partir de los resultados ya guardados.",
    )
    args = parser.parse_args(argv)
    if args.only_plots and (args.skip_plots or args.skip_modelling):
        parser.error("--only-plots no se puede combinar con --skip-plots ni --skip-modelling.")
    return args


def select_pipelines(args: argparse.Namespace) -> list[tuple[str, str]]:
    """Construye la lista ordenada de pipelines a ejecutar.

    Primero todo el cálculo y después todas las figuras, de modo que un fallo al
    dibujar no impida que los resultados queden guardados.
    """
    with_modelling = not args.skip_modelling
    with_plots = not args.skip_plots

    if args.only_plots:
        return PLOT_PIPELINES + MODELLING_PLOT_PIPELINES

    pipelines = list(PIPELINES)
    if with_modelling:
        pipelines += MODELLING_PIPELINES
    if with_plots:
        pipelines += PLOT_PIPELINES
        if with_modelling:
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
        logger.info(f"✓ FINALIZADO: {description}  ({elapsed:.1f} s)")
    except Exception:
        elapsed = time.perf_counter() - t0
        logger.exception(
            f"✗ ERROR en {description} tras {elapsed:.1f} s — abortando ejecución."
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
    log_file = log_dir / f"run_all_{timestamp}.log"

    logger = setup_logger(ORCHESTRATOR_LOG_NAME, log_dir=log_dir)

    # También redirigimos stdout/stderr al archivo de log (además de consola)
    # para tener un registro completo.
    sys.stdout.flush()
    sys.stderr.flush()

    logger.info(f"Orquestador de pipelines")
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
    logger.info(f"{'TOTAL':<45} {'':<8} {overall_elapsed:.1f} s")

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
