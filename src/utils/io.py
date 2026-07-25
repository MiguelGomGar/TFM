"""Input/output helpers for the TFM project."""

import logging
from pathlib import Path
from typing import Any
import joblib
import pandas as pd
import gc
from matplotlib import pyplot as plt
from matplotlib.figure import Figure

logger = logging.getLogger(__name__)


def _as_path(file_path: str | Path) -> Path:
    return Path(file_path).expanduser()


def _prepare_output_path(file_path: str | Path, default_suffix: str) -> Path:
    path = _as_path(file_path)
    if path.suffix == "":
        path = path.with_suffix(default_suffix)
    path.parent.mkdir(parents=True, exist_ok=True)
    return path


def read_excel(file_path: str | Path, **kwargs: Any) -> pd.DataFrame:
    """Read an Excel file into a DataFrame."""

    path = _as_path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"Excel file not found: {path}")

    try:
        dataframe = pd.read_excel(path, **kwargs)
    except Exception as exc:  # pragma: no cover - engine/backend errors vary
        raise RuntimeError(f"Unable to read Excel file: {path}") from exc

    return dataframe


def read_dta(file_path: str | Path, **kwargs: Any) -> pd.DataFrame:
    """Read a Stata .dta file into a DataFrame."""

    path = _as_path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"DTA file not found: {path}")

    try:
        dataframe = pd.read_stata(path, **kwargs)
    except Exception as exc:  # pragma: no cover - parser/backend errors vary
        raise RuntimeError(f"Unable to read DTA file: {path}") from exc

    return dataframe


def read_parquet(file_path: str | Path, **kwargs: Any) -> pd.DataFrame:
    """Read a Parquet file into a DataFrame."""

    path = _as_path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"Parquet file not found: {path}")

    try:
        dataframe = pd.read_parquet(path, **kwargs)
    except Exception as exc:  # pragma: no cover - pandas backend errors vary
        raise RuntimeError(f"Unable to read Parquet file: {path}") from exc

    return dataframe


def save_parquet(dataframe: pd.DataFrame, file_path: str | Path, **kwargs: Any) -> Path:
    """Save a DataFrame to Parquet and return the output path."""

    path = _prepare_output_path(file_path, ".parquet")
    try:
        dataframe.to_parquet(path, index=False, **kwargs)
    except Exception as exc:  # pragma: no cover - pandas backend errors vary
        raise RuntimeError(f"Unable to write Parquet file: {path}") from exc

    return path


def read_csv(file_path: str | Path, **kwargs: Any) -> pd.DataFrame:
    """Read a CSV file into a DataFrame."""

    path = _as_path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"CSV file not found: {path}")

    try:
        dataframe = pd.read_csv(path, **kwargs)
    except Exception as exc:  # pragma: no cover - parser errors vary
        raise RuntimeError(f"Unable to read CSV file: {path}") from exc

    return dataframe


def save_csv(dataframe: pd.DataFrame, file_path: str | Path, **kwargs: Any) -> Path:
    """Save a DataFrame to CSV and return the output path."""

    path = _prepare_output_path(file_path, ".csv")
    try:
        dataframe.to_csv(path, index=False, **kwargs)
    except Exception as exc:  # pragma: no cover - parser errors vary
        raise RuntimeError(f"Unable to write CSV file: {path}") from exc

    return path


def load_joblib(file_path: str | Path) -> Any:
    """Load a serialized object from a joblib file."""

    path = _as_path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"Joblib file not found: {path}")

    try:
        obj = joblib.load(path)
    except Exception as exc:  # pragma: no cover - object-specific errors vary
        raise RuntimeError(f"Unable to load joblib file: {path}") from exc

    return obj


def save_joblib(obj: Any, file_path: str | Path) -> Path:
    """Serialize an object with joblib and return the output path."""

    path = _prepare_output_path(file_path, ".joblib")
    try:
        joblib.dump(obj, path)
    except Exception as exc:  # pragma: no cover - object-specific errors vary
        raise RuntimeError(f"Unable to write joblib file: {path}") from exc

    return path


def save_figure(
    figure: Figure,
    file_path: str | Path,
    dpi: int = 300,
    bbox_inches: str = "tight",
    close_fig: bool = True,
) -> Path:
    """Save a matplotlib Figure and return the output path."""

    path = _prepare_output_path(file_path, ".png")
    try:
        figure.savefig(path, dpi=dpi, bbox_inches=bbox_inches)
        if close_fig:
            figure.clf()
            plt.close(figure)
            plt.close("all")
            gc.collect()
    except Exception as exc:  # pragma: no cover - backend errors vary
        raise RuntimeError(f"Unable to save figure: {path}") from exc

    return path
