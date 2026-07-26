"""Helpers for persisting model evaluation artifacts."""

from pathlib import Path

import joblib
import pandas as pd


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
        {"Feature": feature_names, "Coefficient": coefficients}
    )

    # Separate out features with zero coefficients from features with non-zero
    # coefficients
    df_relevant = df_coefficients[df_coefficients["Coefficient"] != 0]
    df_irrelevant = df_coefficients[df_coefficients["Coefficient"] == 0]

    # Extract the list of irrelevant feature names
    irrelevant_cols = df_irrelevant["Feature"].tolist()
    irrelevant_cols = _clean_feature_names(irrelevant_cols)

    # Sort by absolute value of coefficients in descending order
    df_relevant["Abs_Coefficient"] = df_relevant["Coefficient"].abs()
    df_relevant = df_relevant.sort_values(by="Abs_Coefficient", ascending=False).drop(
        columns="Abs_Coefficient"
    )

    # Extract the list of relevant feature names
    relevant_cols = df_relevant["Feature"].tolist()
    relevant_cols = _clean_feature_names(relevant_cols)

    return relevant_cols, irrelevant_cols


def save_feature_selection_results(
    relevant_cols, irrelevant_cols, output_dir, identifier=None
):
    """Save feature selection results (relevant and irrelevant features) to joblib.

    Persists the lists of selected and rejected features from a regularized
    model's feature selection step as a joblib-serialized dictionary.

    Parameters
    ----------
    relevant_cols : list of str
        Feature names with non-zero coefficients (selected features).
    irrelevant_cols : list of str
        Feature names with zero coefficients (rejected features).
    output_dir : str or Path
        Directory where the joblib file will be saved.
    identifier : str, optional
        Optional string to uniquely identify the saved file. If provided,
        filename will be 'feature_selection_{identifier}.joblib'; otherwise,
        'feature_selection.joblib'.

    Returns
    -------
    Path
        Path to the saved joblib file.
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    payload = {
        "relevant_features": relevant_cols,
        "irrelevant_features": irrelevant_cols,
    }

    if identifier is not None:
        file_path = output_dir / f"feature_selection_{identifier}.joblib"
    else:
        file_path = output_dir / "feature_selection.joblib"

    joblib.dump(payload, file_path)
    return file_path


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
