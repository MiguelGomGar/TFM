"""Logging setup helpers for the TFM project."""

import logging
from datetime import datetime
from pathlib import Path

from src.utils.paths import LOGS_DIR


def setup_logger(name: str, log_dir: Path | None = None) -> logging.Logger:
    """Configure a logger that writes to both console and file.

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

    formatter = logging.Formatter("%(asctime)s - %(levelname)s - %(message)s")

    file_handler = logging.FileHandler(log_file, encoding="utf-8")
    file_handler.setLevel(logging.INFO)
    file_handler.setFormatter(formatter)

    stream_handler = logging.StreamHandler()
    stream_handler.setLevel(logging.INFO)
    stream_handler.setFormatter(formatter)

    logger.addHandler(file_handler)
    logger.addHandler(stream_handler)

    return logger
