"""Helpers for persisting model evaluation artifacts."""

import re
from collections.abc import Sequence
from logging import Logger
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.pipeline import Pipeline

from src.config import MODEL_ORDER
from src.models.model_evaluation import (
    build_comparison_table,
    build_hyperparameters_table,
    build_performance_table,
)
from src.utils.excel_styling import save_excel_workbook
from src.utils.filenames import (
    BEST_PARAMS_FILE,
    CURVES_FILE,
    INTERNAL_VALIDATION_FILE,
    MODELS_METRICS_FILE,
    PREVALENCE_FILE,
)
from src.utils.io import read_csv, save_csv, save_joblib


def _clean_feature_names(feature_list: Sequence[str]) -> list[str]:
    """Remove pipeline prefixes from feature names.

    Strips everything before and including the double underscore ('__')
    from each feature name (e.g., 'num__age' -> 'age').

    Parameters
    ----------
    feature_list : list of str
        Feature names, potentially with pipeline prefixes.

    Returns
    -------
    list of str
        Cleaned feature names with prefixes removed.
    """
    return [name.split("__")[-1] for name in feature_list]


def get_relevant_features(
    regularized_model_pipeline: Pipeline,
) -> tuple[list[str], list[str]]:
    """Extract relevant (non-zero) and irrelevant (zero) features from a regularized model.

    Separates features based on their coefficients from a fitted regularized
    linear model (e.g., Elastic Net), sorting relevant features by absolute
    coefficient magnitude in descending order.

    Parameters
    ----------
    regularized_model_pipeline : sklearn.pipeline.Pipeline
        Fitted pipeline containing a 'preprocessor' step and 'clf' (regularized
        linear model) step with a coef_ attribute.

    Returns
    -------
    relevant_cols : list of str
        Feature names with non-zero coefficients, sorted by absolute coefficient
        magnitude (largest first).
    irrelevant_cols : list of str
        Feature names with zero coefficients (dropped by regularization).

    Raises
    ------
    ValueError
        If the pipeline does not contain a 'preprocessor' step or if the
        preprocessor lacks a get_feature_names_out() method.
    """
    # Get the feature names from the model's preprocessing step (if available)
    if (
        hasattr(regularized_model_pipeline, "named_steps")
        and "preprocessor" in regularized_model_pipeline.named_steps
    ):
        preprocessor = regularized_model_pipeline.named_steps["preprocessor"]
        feature_names = preprocessor.get_feature_names_out()
    else:
        raise ValueError(
            "The provided model does not contain a 'preprocessor' step with feature names."
        )

    # Get the coefficients from the fitted model
    coefficients = regularized_model_pipeline.named_steps["clf"].coef_[0]

    # Create a DataFrame of features and their corresponding coefficients
    df_coefficients = pd.DataFrame(
        {"Feature": feature_names, "Coefficient": np.abs(coefficients.astype(float))}
    )

    # Separate out features with zero coefficients from features with non-zero
    # coefficients
    df_relevant = df_coefficients[coefficients != 0].copy()
    df_irrelevant = df_coefficients[coefficients == 0].copy()

    # Extract the list of irrelevant feature names
    irrelevant_cols = df_irrelevant["Feature"].tolist()
    irrelevant_cols = _clean_feature_names(irrelevant_cols)

    # Sort by absolute coefficient magnitude in descending order
    df_relevant = df_relevant.sort_values(by="Coefficient", ascending=False)

    # Extract the list of relevant feature names
    relevant_cols = df_relevant["Feature"].tolist()
    relevant_cols = _clean_feature_names(relevant_cols)

    return relevant_cols, irrelevant_cols


def save_model(
    fitted_pipeline: Pipeline, output_dir: Path, identifier: str | None = None
) -> None:
    """Save a fitted scikit-learn pipeline to disk as a joblib file.

    Persists the entire trained pipeline (including preprocessing and classifier),
    allowing it to be loaded and used for prediction on new data.

    Parameters
    ----------
    fitted_pipeline : sklearn.pipeline.Pipeline
        A fully trained pipeline object containing a 'preprocessor' and 'clf'
        (classifier) step.
    output_dir : Path
        Directory where the joblib file will be saved.
    identifier : str, optional
        Optional string to identify the model in the filename. If provided,
        filename will be 'optimized_{identifier}.joblib'; otherwise,
        'optimized_{model_class_name}.joblib'.

    Returns
    -------
    None
    """
    # ===========================================================================
    # 1. Save the entire fitted pipeline
    # ===========================================================================

    # Extract the classifier class name dynamically for a precise filename
    model_class_name = type(fitted_pipeline["clf"]).__name__

    # Save the model object
    if identifier is not None:
        file_path = output_dir / f"optimized_{identifier}.joblib"
    else:
        file_path = output_dir / f"optimized_{model_class_name}.joblib"

    # Save the model object (contains preprocessing states, weights, and
    # params)
    save_joblib(fitted_pipeline, file_path)


def save_metrics_results(
    models_dict: dict[str, pd.DataFrame], output_dir: str | Path | None = None
) -> pd.DataFrame:
    """Consolidate cross-validation metrics from multiple models into a master DataFrame.

    Combines long-format metric DataFrames from multiple models into a single
    unified DataFrame with a 'Model' column, and optionally saves to CSV.

    Parameters
    ----------
    models_dict : dict
        Dictionary where keys are model names (str) and values are pandas
        DataFrames with columns ['Metric', 'Dataset', 'Score', 'Fold'].
    output_dir : str or Path, optional
        Directory where the output CSV file will be saved. If None, no file
        is written (only returns the DataFrame).

    Returns
    -------
    pd.DataFrame
        Consolidated DataFrame in long format with columns:
        ['Model', 'Metric', 'Dataset', 'Score']. Suitable for grouped
        plotting and comparison of model performance.
    """
    processed_dfs = []

    # 1. Iterate over each model's DataFrame to add the 'Model' column and
    # remove 'Fold'
    for model_name, df_model in models_dict.items():
        # Make a copy to avoid modifying the original DataFrame
        df_copy = df_model.copy()

        # Add the 'Model' column with the current model's name
        df_copy["Model"] = model_name

        # Delete the 'Fold' column
        df_copy = df_copy.drop(columns=["Fold"], errors="ignore")

        # Append the processed DataFrame to the list
        processed_dfs.append(df_copy)

    # 2. Concatenate all processed DataFrames
    df_master = pd.concat(processed_dfs, ignore_index=True)

    # Reorder columns
    desired_order = ["Model", "Metric", "Dataset", "Score"]
    df_master = df_master[[col for col in desired_order if col in df_master.columns]]

    # 3. Save the master DataFrame to a CSV file if an output directory is
    # provided
    if output_dir is not None:
        output_path = Path(output_dir) / MODELS_METRICS_FILE
        df_master.to_csv(output_path, index=False)

    return df_master


def save_curves_results(
    model_names: Sequence[str],
    x_list: Sequence[np.ndarray],
    y_list: Sequence[np.ndarray],
    curve_type: str = "roc",
    output_dir: str | Path | None = None,
    filename: str | None = None,
) -> pd.DataFrame:
    """Build and save evaluation curve coordinates (ROC or PR) for multiple models.

    Consolidates X and Y coordinates for ROC or Precision-Recall curves from
    multiple models into a single long-format DataFrame, and optionally saves
    to CSV.

    Parameters
    ----------
    model_names : list of str
        Model identifiers (one per curve).
    x_list : list of array-like
        X-axis values for each model. For ROC: False Positive Rate; for PR: Recall.
    y_list : list of array-like
        Y-axis values for each model. For ROC: True Positive Rate; for PR: Precision.
    curve_type : str, default 'roc'
        Type of curve: 'roc' or 'pr' (Precision-Recall).
    output_dir : str or Path, optional
        Directory where the CSV will be saved. If None, no file is written.
    filename : str, optional
        Output CSV filename. If None, defaults to 'curves_roc.csv' or
        'curves_pr.csv' based on curve_type.

    Returns
    -------
    pd.DataFrame
        Long-format DataFrame with columns [x_label, y_label, 'Model'].
        For ROC: ['False Positive Rate', 'True Positive Rate', 'Model'].
        For PR: ['Recall', 'Precision', 'Model'].

    Raises
    ------
    ValueError
        If curve_type is not 'roc' or 'pr'.
    """
    # Validate the curve type input parameter
    if curve_type.lower() not in ["roc", "pr"]:
        raise ValueError("curve_type must be strictly 'roc' or 'pr'")

    # 1. Dynamically set column labels and file prefixes based on the curve
    # category
    if curve_type.lower() == "roc":
        x_label = "False Positive Rate"
        y_label = "True Positive Rate"
    else:
        x_label = "Recall"
        y_label = "Precision"

    # Use the provided user filename or fall back to the dynamic default
    if filename is None:
        filename = CURVES_FILE.format(curve=curve_type.lower())

    # 2. Iterate over models to build individual DataFrames
    individual_dfs = []
    for model, x_vals, y_vals in zip(model_names, x_list, y_list):
        df_temp = pd.DataFrame({x_label: x_vals, y_label: y_vals, "Model": model})
        individual_dfs.append(df_temp)

    # 3. Concatenate all individual records into a single master long-format
    # DataFrame
    df_curve = pd.concat(individual_dfs, ignore_index=True)

    # 4. Serialize and save the DataFrame to disk if a path is provided
    if output_dir is not None:
        path = Path(output_dir)
        file_path = path / f"{filename}"
        df_curve.to_csv(file_path, index=False)

    return df_curve


def save_prevalence(
    y_test: pd.Series | np.ndarray, output_dir: str | Path
) -> pd.DataFrame:
    """Persist the positive-class rate of an evaluation set.

    The precision-recall figures need the prevalence to draw the no-skill
    baseline. It is a property of the split rather than of any model, so it is
    saved next to the metrics instead of being recomputed by whoever plots.

    Parameters
    ----------
    y_test : pd.Series or array-like
        Binary target of the external validation set.
    output_dir : str or Path
        Directory where prevalence.csv is written.

    Returns
    -------
    pd.DataFrame
        Single-row table with columns ['N', 'Positives', 'Prevalence'].
    """
    y_test = pd.Series(y_test)
    table = pd.DataFrame(
        [
            {
                "N": int(y_test.shape[0]),
                "Positives": int(y_test.sum()),
                "Prevalence": float(y_test.mean()),
            }
        ]
    )
    save_csv(table, Path(output_dir) / PREVALENCE_FILE)
    return table


def load_prevalence(input_dir: str | Path) -> float:
    """Read back the prevalence saved by ``save_prevalence``.

    Parameters
    ----------
    input_dir : str or Path
        Directory containing prevalence.csv.

    Returns
    -------
    float
        Positive-class rate of the evaluation set.
    """
    return float(read_csv(Path(input_dir) / PREVALENCE_FILE)["Prevalence"].iloc[0])


def load_internal_validation(
    input_dir: str | Path, logger: Logger | None = None
) -> dict[str, pd.DataFrame]:
    """Load every internal_validation_{model}.csv file found in input_dir.

    Parameters
    ----------
    input_dir : str or Path
        Directory produced by a modelling phase, containing one
        internal_validation_{abbreviation}.csv file per model.
    logger : logging.Logger, optional
        Logger used to report models that could not be found.

    Returns
    -------
    dict
        Mapping of model abbreviation to its internal-validation DataFrame,
        restricted to the models listed in MODEL_ORDER that were actually
        found on disk.
    """
    input_dir = Path(input_dir)
    internal_validation = {}
    for abbreviation in MODEL_ORDER:
        file_path = input_dir / INTERNAL_VALIDATION_FILE.format(
            model=abbreviation.lower()
        )
        if not file_path.exists():
            if logger is not None:
                logger.warning(f"Missing {file_path}; skipping {abbreviation}.")
            continue
        internal_validation[abbreviation] = read_csv(file_path)
    return internal_validation


def build_performance_tables(
    phases: Sequence[dict],
    metrics: Sequence[str],
    output_path: str | Path,
    decimals: int = 3,
    logger: Logger | None = None,
) -> None:
    """Build one performance workbook with one sheet per modelling phase.

    Each sheet is a single table covering every requested metric at once
    (row index 'Model', column MultiIndex ['Metric', 'Partition']), read
    straight from the phase's models_metrics.csv (see
    model_evaluation.build_performance_table).

    Parameters
    ----------
    phases : list of dict
        Modelling phases to summarize, each with keys 'label' and 'input_dir'
        (see config.PERFORMANCE_PHASES).
    metrics : list of str
        Metrics to report, e.g. ['ROC-AUC', 'PR-AUC'] (see
        config.PUBLICATION_METRICS).
    output_path : str or Path
        Path of the .xlsx workbook to write.
    decimals : int, default 3
        Number of decimals used to format the scores.
    logger : logging.Logger, optional
        Logger used to report progress and missing input files.
    """
    sheets = {}
    for phase in phases:
        metrics_file = Path(phase["input_dir"]) / MODELS_METRICS_FILE
        if not metrics_file.exists():
            if logger is not None:
                logger.warning(
                    f"Missing {metrics_file}; skipping the {phase['label']} "
                    "performance table."
                )
            continue

        if logger is not None:
            logger.info(f"Building the {phase['label']} performance table...")
        models_metrics = read_csv(metrics_file)
        sheets[phase["label"]] = build_performance_table(
            models_metrics, metrics, decimals=decimals
        )

    if not sheets:
        if logger is not None:
            logger.warning("No performance tables were built; skipping the workbook.")
        return

    save_excel_workbook(sheets, output_path, n_header_rows=3, n_index_cols=1)


def build_comparison_tables(
    phases: Sequence[dict],
    metrics: Sequence[str],
    output_path: str | Path,
    decimals: int = 3,
    logger: Logger | None = None,
) -> None:
    """Build one comparison workbook with one sheet per modality comparison.

    Each sheet is a single table covering every requested metric at once
    (row index 'Model', column MultiIndex ['Metric', {baseline, comparison,
    Delta}]), reshaped from the delta table saved by pipeline 13 (see
    model_evaluation.build_comparison_table).

    Parameters
    ----------
    phases : list of dict
        Comparisons to reshape, each with keys 'label', 'delta_file',
        'baseline_label' and 'comparison_label' (see config.COMPARISON_PHASES).
    metrics : list of str
        Metrics to report, e.g. ['ROC-AUC', 'PR-AUC'] (see
        config.PUBLICATION_METRICS).
    output_path : str or Path
        Path of the .xlsx workbook to write.
    decimals : int, default 3
        Number of decimals used to format the scores and the delta.
    logger : logging.Logger, optional
        Logger used to report progress and missing delta files.
    """
    sheets = {}
    for phase in phases:
        if not phase["delta_file"].exists():
            if logger is not None:
                logger.warning(
                    f"Missing {phase['delta_file']}; run pipeline 13 first. Skipping "
                    f"{phase['label']}."
                )
            continue

        if logger is not None:
            logger.info(f"Building the {phase['label']} comparison table...")
        delta_table = read_csv(phase["delta_file"])
        sheets[phase["label"]] = build_comparison_table(
            delta_table,
            metrics=metrics,
            baseline_label=phase["baseline_label"],
            comparison_label=phase["comparison_label"],
            decimals=decimals,
        )

    if not sheets:
        if logger is not None:
            logger.warning("No comparison tables were built; skipping the workbook.")
        return

    save_excel_workbook(sheets, output_path, n_header_rows=3, n_index_cols=1)


_MEAN_SD_SUFFIX = ", mean (SD)"
_MEAN_SD_VALUE = re.compile(r"^(-?\d+\.?\d*)\s*\((-?\d+\.?\d*)\)$")


def _format_mean_sd_value(value: object) -> object:
    """Turn a TableOne 'mean (SD)' cell into 'mean ± SD'.

    Parameters
    ----------
    value : object
        Cell value; only strings matching the '<mean> (<SD>)' pattern are
        reformatted, everything else (NaN, already-formatted strings) passes
        through unchanged.

    Returns
    -------
    object
        The reformatted string, or the original value if it does not match.
    """
    if not isinstance(value, str):
        return value
    match = _MEAN_SD_VALUE.match(value.strip())
    if not match:
        return value
    mean, sd = match.groups()
    return f"{mean} ± {sd}"


def _humanize_table1(table1: pd.DataFrame) -> pd.DataFrame:
    """Reformat a raw TableOne export for manuscript-ready presentation.

    Underscores in variable and group names are TableOne's stand-in for
    spaces (it reads them straight off the dataframe's column names), not a
    meaningful separator, so they are replaced throughout. Numeric variables
    reported as 'mean (SD)' are also switched to 'mean ± SD', which reads
    better in a table than the parenthesized form; non-normal variables
    reported as median [Q1,Q3] are left untouched since that reflects their
    skewed distribution.

    Parameters
    ----------
    table1 : pd.DataFrame
        Table as read from table1.csv, with a ['Variable', 'Level'] row
        MultiIndex and a 2-level column MultiIndex.

    Returns
    -------
    pd.DataFrame
        Same shape, with underscores stripped from labels and 'mean (SD)'
        rows switched to 'mean ± SD'.
    """
    table1 = table1.copy()

    variables = table1.index.get_level_values("Variable")
    mean_sd_mask = variables.str.endswith(_MEAN_SD_SUFFIX)
    for column in table1.columns:
        table1.loc[mean_sd_mask, column] = table1.loc[mean_sd_mask, column].map(
            _format_mean_sd_value
        )

    new_variables = variables.str.replace(
        _MEAN_SD_SUFFIX, ", mean ± SD", regex=False
    ).str.replace("_", " ", regex=False)
    levels = table1.index.get_level_values("Level")
    table1.index = pd.MultiIndex.from_arrays(
        [new_variables, levels], names=["Variable", "Level"]
    )

    table1.columns = table1.columns.set_levels(
        table1.columns.levels[0].str.replace("_", " ", regex=False), level=0
    )

    return table1


def build_table1_workbook(
    table1_path: str | Path,
    output_path: str | Path,
    logger: Logger | None = None,
) -> None:
    """Build a single-sheet workbook with the clinical Table 1.

    Reformats the raw TableOne export (pipeline 06) into a merged-cell,
    styled worksheet matching the rest of the publication tables, instead of
    the flat, repeated-label CSV (see _humanize_table1 for the label/value
    clean-up applied).

    Parameters
    ----------
    table1_path : str or Path
        Path to table1.csv (see config.TABLE1_PATH), as saved by
        data.statistical_analysis.create_table1.
    output_path : str or Path
        Path of the .xlsx workbook to write.
    logger : logging.Logger, optional
        Logger used to report progress and a missing input file.
    """
    table1_path = Path(table1_path)
    if not table1_path.exists():
        if logger is not None:
            logger.warning(f"Missing {table1_path}; skipping the Table 1 workbook.")
        return

    if logger is not None:
        logger.info("Building the Table 1 workbook...")

    table1 = read_csv(table1_path, header=[0, 1], index_col=[0, 1])
    table1.index = table1.index.set_names(["Variable", "Level"])
    table1 = _humanize_table1(table1)

    save_excel_workbook(
        {"Table 1": table1}, output_path, n_header_rows=3, n_index_cols=2
    )


def build_hyperparameters_tables(
    phases: Sequence[dict], output_path: str | Path, logger: Logger | None = None
) -> None:
    """Build a single hyperparameters workbook comparing every modelling phase.

    Every phase's tuned hyperparameters are merged into one table, indexed by
    a ['Model', 'Parameter'] MultiIndex, with one 'Value' sub-column per
    phase side by side (see model_evaluation.build_hyperparameters_table),
    instead of one near-identical table per phase.

    Parameters
    ----------
    phases : list of dict
        Modelling phases to summarize, each with keys 'label' and 'input_dir'
        (see config.PERFORMANCE_PHASES).
    output_path : str or Path
        Path of the .xlsx workbook to write.
    logger : logging.Logger, optional
        Logger used to report progress and missing best_params files.
    """
    phase_best_params = {}
    for phase in phases:
        best_params_file = Path(phase["input_dir"]) / BEST_PARAMS_FILE
        if not best_params_file.exists():
            if logger is not None:
                logger.warning(
                    f"Missing {best_params_file}; skipping the {phase['label']} "
                    "hyperparameters column."
                )
            continue

        if logger is not None:
            logger.info(f"Loading the {phase['label']} hyperparameters...")
        # keep_default_na=False: values such as the string "None" (a real,
        # meaningful hyperparameter setting, e.g. class_weight=None) must not
        # be silently parsed into a missing value.
        phase_best_params[phase["label"]] = read_csv(
            best_params_file, keep_default_na=False
        )

    if not phase_best_params:
        if logger is not None:
            logger.warning("No hyperparameters were found; skipping the workbook.")
        return

    table = build_hyperparameters_table(phase_best_params)
    save_excel_workbook(
        {"Hyperparameters": table}, output_path, n_header_rows=3, n_index_cols=2
    )
