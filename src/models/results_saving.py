"""Helpers for persisting model evaluation artifacts."""

from pathlib import Path

import joblib
import pandas as pd

from src.config import MODEL_ORDER
from src.models.model_evaluation import (
    build_comparison_table,
    build_model_hyperparameter_tables,
    build_performance_table,
)
from src.utils.io import read_csv, save_csv


def _clean_feature_names(feature_list):
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


def get_relevant_features(regularized_model_pipeline):
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
        {"Feature": feature_names, "Coefficient": coefficients.astype(float).abs()}
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


def save_model(fitted_pipeline, output_dir, identifier=None):
    """Save a fitted scikit-learn pipeline to disk as a joblib file.

    Persists the entire trained pipeline (including preprocessing and classifier),
    allowing it to be loaded and used for prediction on new data.

    Parameters
    ----------
    fitted_pipeline : sklearn.pipeline.Pipeline
        A fully trained pipeline object containing a 'preprocessor' and 'clf'
        (classifier) step.
    output_dir : str or Path
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
    joblib.dump(fitted_pipeline, file_path)


def save_metrics_results(models_dict, output_dir=None):
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
        output_path = Path(output_dir) / "models_metrics.csv"
        df_master.to_csv(output_path, index=False)

    return df_master


def save_curves_results(
    model_names, x_list, y_list, curve_type="roc", output_dir=None, filename=None
):
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
        default_prefix = "curves_roc"
    else:
        x_label = "Recall"
        y_label = "Precision"
        default_prefix = "curves_pr"

    # Use the provided user filename or fall back to the dynamic default
    filename = filename if filename is not None else f"{default_prefix}.csv"

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


def load_internal_validation(input_dir: Path, logger=None) -> dict:
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
        file_path = input_dir / f"internal_validation_{abbreviation.lower()}.csv"
        if not file_path.exists():
            if logger is not None:
                logger.warning(f"Missing {file_path}; skipping {abbreviation}.")
            continue
        internal_validation[abbreviation] = read_csv(file_path)
    return internal_validation


def build_performance_tables(
    phases, metrics, output_dir: Path, decimals: int = 3, logger=None
) -> None:
    """Build one wide performance table per modelling phase and per metric.

    Parameters
    ----------
    phases : list of dict
        Modelling phases to summarize, each with keys 'label', 'input_dir'
        and 'output_prefix' (see config.PERFORMANCE_PHASES).
    metrics : dict
        Mapping of metric name (e.g. 'ROC-AUC') to the filename slug used to
        build each table's filename (see config.PUBLICATION_METRICS).
    output_dir : str or Path
        Directory where each phase's performance tables are saved.
    decimals : int, default 3
        Number of decimals used to format the "mean (SD)" strings.
    logger : logging.Logger, optional
        Logger used to report progress and missing input directories.
    """
    for phase in phases:
        if logger is not None:
            logger.info(f"Building the {phase['label']} performance tables...")
        internal_validation = load_internal_validation(
            phase["input_dir"], logger=logger
        )
        if not internal_validation:
            if logger is not None:
                logger.warning(
                    f"No internal-validation files found for {phase['label']} in "
                    f"{phase['input_dir']}; skipping."
                )
            continue

        for metric, slug in metrics.items():
            table = build_performance_table(
                internal_validation, metric, decimals=decimals
            )
            filename = f"{phase['output_prefix']}_{slug}.csv"
            save_csv(table, Path(output_dir) / filename)


def build_comparison_tables(
    phases, metrics, output_dir: Path, decimals: int = 3, logger=None
) -> None:
    """Build one wide incremental-value table per comparison and per metric.

    Parameters
    ----------
    phases : list of dict
        Comparisons to reshape, each with keys 'label', 'delta_file',
        'baseline_label', 'comparison_label' and 'output_prefix' (see
        config.COMPARISON_PHASES).
    metrics : dict
        Mapping of metric name (e.g. 'ROC-AUC') to the filename slug used to
        build each table's filename (see config.PUBLICATION_METRICS).
    output_dir : str or Path
        Directory where each comparison's tables are saved.
    decimals : int, default 3
        Number of decimals used to format the scores and the delta.
    logger : logging.Logger, optional
        Logger used to report progress and missing delta files.
    """
    for phase in phases:
        if not phase["delta_file"].exists():
            if logger is not None:
                logger.warning(
                    f"Missing {phase['delta_file']}; run pipeline 13 first. Skipping "
                    f"{phase['label']}."
                )
            continue

        if logger is not None:
            logger.info(f"Building the {phase['label']} comparison tables...")
        delta_table = read_csv(phase["delta_file"])

        for metric, slug in metrics.items():
            table = build_comparison_table(
                delta_table,
                metric=metric,
                baseline_label=phase["baseline_label"],
                comparison_label=phase["comparison_label"],
                decimals=decimals,
            )
            filename = f"{phase['output_prefix']}_{slug}.csv"
            save_csv(table, Path(output_dir) / filename)


def build_hyperparameters_tables(phases, output_dir: Path, logger=None) -> None:
    """Build one Parameter/Value hyperparameters table per model per phase.

    Each file is named '{phase}_{model}.csv' (e.g. 'clinical_EN.csv',
    'multimodal_SVM.csv'), with two columns ['Parameter', 'Value'] and every
    scikit-learn pipeline prefix stripped from the parameter names.

    Parameters
    ----------
    phases : list of dict
        Modelling phases to summarize, each with keys 'label', 'input_dir'
        and 'prefix' (see config.PERFORMANCE_PHASES).
    output_dir : str or Path
        Directory where the hyperparameters tables are saved.
    logger : logging.Logger, optional
        Logger used to report progress and missing best_params files.
    """
    for phase in phases:
        best_params_file = Path(phase["input_dir"]) / "best_params.csv"
        if not best_params_file.exists():
            if logger is not None:
                logger.warning(
                    f"Missing {best_params_file}; skipping the {phase['label']} "
                    "hyperparameters tables."
                )
            continue

        if logger is not None:
            logger.info(f"Building the {phase['label']} hyperparameters tables...")
        # keep_default_na=False: values such as the string "None" (a real,
        # meaningful hyperparameter setting, e.g. class_weight=None) must not
        # be silently parsed into a missing value.
        best_params = read_csv(best_params_file, keep_default_na=False)
        tables_by_model = build_model_hyperparameter_tables(best_params)

        for model, table in tables_by_model.items():
            filename = f"{phase['prefix']}_{model}.csv"
            save_csv(table, Path(output_dir) / filename)
