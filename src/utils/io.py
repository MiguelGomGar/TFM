"""Input/output helpers for the TFM project."""

import logging
from pathlib import Path
from typing import Any

import pandas as pd
import gc
import matplotlib

# Force a non-interactive backend before pyplot is imported anywhere: the reporting
# pipelines render thousands of figures in a loop, and GUI backends retain a figure
# manager plus native resources per figure, exhausting memory mid-run.
matplotlib.use("Agg", force=True)

from matplotlib import pyplot as plt
from matplotlib.figure import Figure

logger = logging.getLogger(__name__)


def _as_path(file_path: str | Path) -> Path:
    """Convert a string or Path to an expanded Path object.

    Parameters
    ----------
    file_path : str or Path
        File path as a string or Path object.

    Returns
    -------
    Path
        Expanded Path object with user home directory (~) resolved.
    """
    return Path(file_path).expanduser()


def _prepare_output_path(file_path: str | Path, default_suffix: str) -> Path:
    """Prepare an output file path and create its parent directory if needed.

    Parameters
    ----------
    file_path : str or Path
        Target file path. If it has no suffix, `default_suffix` is appended.
    default_suffix : str
        File extension to append if the path has no suffix (e.g., ".png").

    Returns
    -------
    Path
        Prepared Path object with parent directory created and suffix ensured.
    """
    path = _as_path(file_path)
    if path.suffix == "":
        path = path.with_suffix(default_suffix)
    path.parent.mkdir(parents=True, exist_ok=True)
    return path


def read_excel(file_path: str | Path, **kwargs: Any) -> pd.DataFrame:
    """Read an Excel file into a DataFrame.

    Parameters
    ----------
    file_path : str or Path
        Path to the Excel file.
    **kwargs : dict
        Additional keyword arguments passed to pandas.read_excel().

    Returns
    -------
    pd.DataFrame
        DataFrame loaded from the Excel file.

    Raises
    ------
    FileNotFoundError
        If the file does not exist.
    RuntimeError
        If the file cannot be read due to a backend error.
    """
    path = _as_path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"Excel file not found: {path}")

    try:
        dataframe = pd.read_excel(path, **kwargs)
    except Exception as exc:  # pragma: no cover - engine/backend errors vary
        raise RuntimeError(f"Unable to read Excel file: {path}") from exc

    return dataframe


def read_dta(file_path: str | Path, **kwargs: Any) -> pd.DataFrame:
    """Read a Stata .dta file into a DataFrame.

    Parameters
    ----------
    file_path : str or Path
        Path to the Stata .dta file.
    **kwargs : dict
        Additional keyword arguments passed to pandas.read_stata().

    Returns
    -------
    pd.DataFrame
        DataFrame loaded from the Stata file.

    Raises
    ------
    FileNotFoundError
        If the file does not exist.
    RuntimeError
        If the file cannot be read due to a backend error.
    """
    path = _as_path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"DTA file not found: {path}")

    try:
        dataframe = pd.read_stata(path, **kwargs)
    except Exception as exc:  # pragma: no cover - parser/backend errors vary
        raise RuntimeError(f"Unable to read DTA file: {path}") from exc

    return dataframe


def read_parquet(file_path: str | Path, **kwargs: Any) -> pd.DataFrame:
    """Read a Parquet file into a DataFrame.

    Parameters
    ----------
    file_path : str or Path
        Path to the Parquet file.
    **kwargs : dict
        Additional keyword arguments passed to pandas.read_parquet().

    Returns
    -------
    pd.DataFrame
        DataFrame loaded from the Parquet file.

    Raises
    ------
    FileNotFoundError
        If the file does not exist.
    RuntimeError
        If the file cannot be read due to a backend error.
    """
    path = _as_path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"Parquet file not found: {path}")

    try:
        dataframe = pd.read_parquet(path, **kwargs)
    except Exception as exc:  # pragma: no cover - pandas backend errors vary
        raise RuntimeError(f"Unable to read Parquet file: {path}") from exc

    return dataframe


def save_parquet(dataframe: pd.DataFrame, file_path: str | Path, **kwargs: Any) -> Path:
    """Save a DataFrame to Parquet format and return the output path.

    Parameters
    ----------
    dataframe : pd.DataFrame
        DataFrame to save.
    file_path : str or Path
        Output file path. If no suffix, ".parquet" is appended.
    **kwargs : dict
        Additional keyword arguments passed to pandas.to_parquet().

    Returns
    -------
    Path
        Path where the file was saved.

    Raises
    ------
    RuntimeError
        If the file cannot be written due to a backend error.
    """
    path = _prepare_output_path(file_path, ".parquet")
    try:
        dataframe.to_parquet(path, index=False, **kwargs)
    except Exception as exc:  # pragma: no cover - pandas backend errors vary
        raise RuntimeError(f"Unable to write Parquet file: {path}") from exc

    return path


def read_csv(file_path: str | Path, **kwargs: Any) -> pd.DataFrame:
    """Read a CSV file into a DataFrame.

    Parameters
    ----------
    file_path : str or Path
        Path to the CSV file.
    **kwargs : dict
        Additional keyword arguments passed to pandas.read_csv().

    Returns
    -------
    pd.DataFrame
        DataFrame loaded from the CSV file.

    Raises
    ------
    FileNotFoundError
        If the file does not exist.
    RuntimeError
        If the file cannot be read due to a backend error.
    """
    path = _as_path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"CSV file not found: {path}")

    try:
        dataframe = pd.read_csv(path, **kwargs)
    except Exception as exc:  # pragma: no cover - parser errors vary
        raise RuntimeError(f"Unable to read CSV file: {path}") from exc

    return dataframe


def save_csv(
    dataframe: pd.DataFrame, file_path: str | Path, index: bool = False, **kwargs: Any
) -> Path:
    """Save a DataFrame to CSV format and return the output path.

    Parameters
    ----------
    dataframe : pd.DataFrame
        DataFrame to save.
    file_path : str or Path
        Output file path. If no suffix, ".csv" is appended.
    index : bool, default False
        Whether to save the DataFrame index as a column.
    **kwargs : dict
        Additional keyword arguments passed to pandas.to_csv().

    Returns
    -------
    Path
        Path where the file was saved.

    Raises
    ------
    RuntimeError
        If the file cannot be written due to a backend error.
    """
    path = _prepare_output_path(file_path, ".csv")
    try:
        dataframe.to_csv(path, index=index, **kwargs)
    except Exception as exc:  # pragma: no cover - parser errors vary
        raise RuntimeError(f"Unable to write CSV file: {path}") from exc

    return path


def save_figure(
    figure: Figure,
    file_path: str | Path,
    dpi: int = 300,
    bbox_inches: str = "tight",
    close_fig: bool = True,
) -> Path:
    """Save a matplotlib Figure to disk and return the output path.

    Parameters
    ----------
    figure : matplotlib.figure.Figure
        Matplotlib figure object to save.
    file_path : str or Path
        Output file path. If no suffix, ".png" is appended.
    dpi : int, default 300
        Dots per inch (resolution) for the saved figure.
    bbox_inches : str, default "tight"
        Bounding box setting; "tight" removes excess whitespace.
    close_fig : bool, default True
        If True, close the figure after saving to free memory.

    Returns
    -------
    Path
        Path where the figure was saved.

    Raises
    ------
    RuntimeError
        If the figure cannot be saved due to a backend error.
    """
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
