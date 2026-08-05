"""Sequential runner shared by the calculation and figure orchestrators.

Pipeline modules are named after the analysis step they perform and are prefixed
with their position (``01_``, ``02_``...), which makes them invalid Python
identifiers: they cannot be imported with a plain ``import`` statement, only
through ``importlib``. That indirection means a mistyped module name is not a
syntax error but a runtime failure, so the runner reports one explicitly instead
of failing halfway through a long night of modelling.

Execution stops at the first failure. The steps form a chain in which each one
consumes what the previous one wrote, so continuing past an error would only
produce results derived from stale inputs.
"""

import argparse
import importlib
import sys
import time
from collections.abc import Sequence
from dataclasses import dataclass
from datetime import datetime
from logging import FileHandler, Logger
from pathlib import Path

from src.utils.logging_utils import setup_logger
from src.utils.paths import LOGS_DIR


@dataclass(frozen=True)
class Pipeline:
    """One executable analysis step.

    Attributes
    ----------
    module : str
        Importable module path, e.g. 'src.pipelines.calculations.02_data_collection'.
    description : str
        Human-readable label used in the logs and the summary table.
    """

    module: str
    description: str


@dataclass(frozen=True)
class PipelineResult:
    """Outcome of a single pipeline run.

    Attributes
    ----------
    pipeline : Pipeline
        The step this result belongs to.
    elapsed : float
        Wall-clock seconds spent in the step, 0.0 if it failed.
    succeeded : bool
        Whether main() returned without raising.
    """

    pipeline: Pipeline
    elapsed: float
    succeeded: bool


def parse_args(
    description: str, skip_flag: str, skip_help: str, argv: Sequence[str] | None = None
) -> argparse.Namespace:
    """Parse the command line of an orchestrator.

    Parameters
    ----------
    description : str
        Help text shown by --help.
    skip_flag : str
        Long option that skips the slow modelling steps, e.g. '--skip-modelling'.
    skip_help : str
        Help text for that option.
    argv : sequence of str, optional
        Arguments to parse. Defaults to sys.argv.

    Returns
    -------
    argparse.Namespace
        Parsed arguments, with the skip flag stored under its dest name.
    """
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument(skip_flag, action="store_true", help=skip_help)
    return parser.parse_args(argv)


def run_pipeline(pipeline: Pipeline, logger: Logger) -> float:
    """Import a pipeline module and run its main().

    Parameters
    ----------
    pipeline : Pipeline
        Step to execute.
    logger : logging.Logger
        Logger receiving the progress and error messages.

    Returns
    -------
    float
        Wall-clock seconds the step took.

    Raises
    ------
    Exception
        Whatever main() raised, after logging the traceback.
    """
    logger.info("=" * 72)
    logger.info(f"STARTING: {pipeline.description}  ({pipeline.module})")
    logger.info("=" * 72)

    started = time.perf_counter()
    try:
        importlib.import_module(pipeline.module).main()
    except Exception:
        logger.exception(
            f"✗ FAILED: {pipeline.description} after "
            f"{time.perf_counter() - started:.1f} s — aborting the run."
        )
        raise

    elapsed = time.perf_counter() - started
    logger.info(f"✓ DONE: {pipeline.description}  ({elapsed:.1f} s)")
    return elapsed


def _log_summary(
    results: Sequence[PipelineResult],
    total_elapsed: float,
    expected: int,
    log_file: Path,
    logger: Logger,
) -> bool:
    """Log the closing summary table and report whether the run succeeded.

    Parameters
    ----------
    results : sequence of PipelineResult
        Outcomes in execution order. Shorter than ``expected`` when the run
        stopped early.
    total_elapsed : float
        Wall-clock seconds of the whole run.
    expected : int
        Number of steps that were scheduled.
    log_file : Path
        File the run was logged to, quoted in the closing message.
    logger : logging.Logger
        Logger receiving the summary.

    Returns
    -------
    bool
        True when every scheduled step succeeded.
    """
    logger.info("")
    logger.info("Run summary")
    logger.info(f"{'Pipeline':<45} {'Status':<8} {'Time':>8}")
    logger.info("-" * 63)
    for result in results:
        label = f"{result.pipeline.description} ({result.pipeline.module.split('.')[-1]})"
        status = "OK" if result.succeeded else "ERROR"
        elapsed = f"{result.elapsed:.1f} s" if result.succeeded else "—"
        logger.info(f"{label:<45} {status:<8} {elapsed:>8}")
    logger.info("-" * 63)
    logger.info(f"{'TOTAL':<45} {'':<8} {total_elapsed:.1f} s")

    succeeded = sum(1 for result in results if result.succeeded)
    logger.info(f"\nPipelines completed: {succeeded} / {expected}")

    if succeeded == expected:
        logger.info(f"✅ Every pipeline ran successfully.\nFull log saved to: {log_file}")
        return True

    failed = [r.pipeline.description for r in results if not r.succeeded]
    logger.error(f"❌ These pipelines failed: {failed}.\nFull log saved to: {log_file}")
    return False


def run_orchestrator(pipelines: Sequence[Pipeline], log_name: str, title: str) -> None:
    """Run every pipeline in order, logging progress and a closing summary.

    Exits the process with status 1 if any step fails, so a failed run is
    visible to whatever launched it.

    Parameters
    ----------
    pipelines : sequence of Pipeline
        Steps to execute, in order.
    log_name : str
        Basename of the log file, e.g. 'run_calculations'. Also the logger name.
    title : str
        Headline written at the top of the log.

    Returns
    -------
    None
    """
    started = time.perf_counter()

    log_dir = LOGS_DIR / "orchestrator"
    logger = setup_logger(log_name, log_dir=log_dir)
    # setup_logger timestamps the filename itself, so read the path back off the
    # handler rather than recomputing it: the two clocks would not always agree,
    # and the previous version reported a file that was never created.
    log_file = next(
        (
            Path(handler.baseFilename)
            for handler in logger.handlers
            if isinstance(handler, FileHandler)
        ),
        log_dir,
    )

    sys.stdout.flush()
    sys.stderr.flush()

    logger.info(title)
    logger.info(f"Started:            {datetime.now().isoformat()}")
    logger.info(f"Log file:           {log_file}")
    logger.info(f"Python:             {sys.version}")
    logger.info(f"Pipelines to run:   {len(pipelines)}")
    logger.info("")

    results: list[PipelineResult] = []
    for pipeline in pipelines:
        try:
            elapsed = run_pipeline(pipeline, logger)
        except Exception:
            results.append(PipelineResult(pipeline, 0.0, succeeded=False))
            break
        results.append(PipelineResult(pipeline, elapsed, succeeded=True))

    ok = _log_summary(
        results, time.perf_counter() - started, len(pipelines), log_file, logger
    )
    if not ok:
        sys.exit(1)
