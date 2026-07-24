import pandas as pd
from pathlib import Path
import joblib


def _clean_feature_names(feature_list):
    """
    Cleans a list of feature names by removing any prefixes before the double
    underscore '__'.

    Parameters:
    ----------
    - feature_list : list of str
        A list of feature names with prefixes (e.g., 'num__age').

    Returns:
    -------
    - list of str
        A list of cleaned feature names.

    """

    return [name.split("__")[-1] for name in feature_list]


def get_relevant_features(regularized_model_pipeline):
    """
    Extracts and returns a DataFrame of non-zero coefficients from a regularized
    linear model.

    Parameters:
    ----------
    - regularized_model_pipeline : sklearn.pipeline.Pipeline
        A fitted pipeline containing a regularized linear model.

    Returns:
    -------
    - relevant_cols : list
        A list of feature names corresponding to non-zero coefficients in the
        model.
    - irrelevant_cols : list
        A list of feature names corresponding to zero coefficients in the model.
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
    """
    Saves the relevant and irrelevant feature lists derived from the Elastic Net
    regularization step as a joblib artifact.

    Parameters:
    ----------
    - relevant_cols : list
        List of feature names with non-zero coefficients.
    - irrelevant_cols : list
        List of feature names with zero coefficients.
    - output_dir : str or pathlib.Path
        Directory where the feature selection artifact will be stored.
    - identifier : str, optional
        Optional identifier used to build the filename.

    Returns:
    -------
    - pathlib.Path
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
    """
    Saves the entire fitted pipeline as a binary file (.joblib), displays its
    hyperparameters as a formatted pandas DataFrame (excluding default/None
    values), and returns that DataFrame.

    Parameters:
    ----------
    - fitted_pipeline : sklearn.pipeline.Pipeline
        The fully trained pipeline object to be serialized and saved.
    - output_dir : str or pathlib.Path
        The directory path where the binary file will be stored.
    - identifier : str, optional
        An optional string to uniquely identify the saved model file (e.g.,
        "RF").

    Returns:
    -------
    - None
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
    """
    Unifies multiple long-format model result DataFrames into a single master
    DataFrame, adds a 'Model' column, removes the 'Fold' column, and saves it as
    a CSV.

    Parameters:
    ----------
    - models_dict : dict
        A dictionary where keys are model names (str) and values are pandas
        DataFrames
        containing ['Metric', 'Dataset', 'Score', 'Fold'] columns.
    - output_dir : str or pathlib.Path
        The directory where the output CSV file will be saved.

    Returns:
    -------
    - df_master : pandas.DataFrame
        The consolidated master DataFrame ready for global comparison plots.
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
    """
    Builds a unified long-format DataFrame containing evaluation curve
    coordinates (ROC or PR)
    for multiple models and saves it to a CSV file.

    Parameters:
    ----------
    - model_names : list of str
        List containing the names of the models.
    - x_list : list of arrays/lists
        List containing the X-axis values for each model (FPR for ROC, Recall
        for PR).
    - y_list : list of arrays/lists
        List containing the Y-axis values for each model (TPR for ROC, Precision
        for PR).
    - curve_type : str, default='roc'
        The type of evaluation curve data to build. Options are 'roc' or 'pr'.
    - output_dir : str or pathlib.Path, optional
        The directory path where the CSV file will be stored.
    - filename : str, optional
        The name of the output CSV file. If None, it dynamically defaults
        to 'roc_curves' or 'pr_curves' based on curve_type.

    Returns:
    -------
    - df_curve : pandas.DataFrame
        Unified DataFrame in long format containing the curve coordinates.
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
