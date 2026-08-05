"""Logging setup helpers for the TFM project."""

import atexit
import logging
import sys
from datetime import datetime
from pathlib import Path
from types import TracebackType

from src.utils.paths import LOGS_DIR


def setup_logger(name: str, log_dir: Path | None = None) -> logging.Logger:
    """Configure a logger that writes to both console and file.

    Messages are printed as-is, with no timestamp or header. A timestamp is
    only logged once at the start and once at the end of the run; if the run
    crashes with an uncaught exception, the error is logged instead.

    Parameters
    ----------
    name : str
        Logger name (typically __name__ of the calling module).
    log_dir : Path or None, default None
        Directory where log files are written. If None, uses LOGS_DIR.

    Returns
    -------
    logging.Logger
        Configured logger with file and stream handlers at INFO level.
    """
    target_dir = Path(log_dir) if log_dir is not None else LOGS_DIR
    target_dir.mkdir(parents=True, exist_ok=True)

    logger = logging.getLogger(name)
    for handler in list(logger.handlers):
        logger.removeHandler(handler)
        handler.close()

    logger.setLevel(logging.INFO)
    logger.propagate = False

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    log_file = target_dir / f"{name}_{timestamp}.log"

    formatter = logging.Formatter("%(message)s")

    file_handler = logging.FileHandler(log_file, encoding="utf-8")
    file_handler.setFormatter(formatter)

    stream_handler = logging.StreamHandler()
    stream_handler.setFormatter(formatter)

    logger.addHandler(file_handler)
    logger.addHandler(stream_handler)

    logger.info(f"[{datetime.now():%Y-%m-%d %H:%M:%S}] Started: {name}")

    def log_uncaught_exception(
        exc_type: type[BaseException],
        exc_value: BaseException,
        exc_traceback: TracebackType | None,
    ) -> None:
        """Record a crash in the log file instead of only on the console.

        A Ctrl-C is deliberately left to the default hook: it is the user
        stopping the run, not a failure worth a traceback in the log.
        """
        if issubclass(exc_type, KeyboardInterrupt):
            sys.__excepthook__(exc_type, exc_value, exc_traceback)
            return
        logger.error("Run failed", exc_info=(exc_type, exc_value, exc_traceback))

    def log_finished() -> None:
        """Stamp the end of the run, however the interpreter exits."""
        logger.info(f"[{datetime.now():%Y-%m-%d %H:%M:%S}] Finished: {name}")

    sys.excepthook = log_uncaught_exception
    atexit.register(log_finished)

    return logger
