import pandas as pd
from pathlib import Path
import joblib

def save_model(fitted_pipeline, output_dir, filename_prefix="best_model"):
    """
    Saves the entire fitted pipeline as a binary file (.joblib), displays its 
    hyperparameters as a formatted pandas DataFrame (excluding default/None values), 
    and returns that DataFrame.

    Parameters:
    ----------
    - fitted_pipeline : sklearn.pipeline.Pipeline
        The fully trained pipeline object to be serialized and saved.
    - output_dir : str or pathlib.Path
        The directory path where the binary file will be stored.
    - filename_prefix : str, default="best_model"
        The prefix used for the output file name.

    Returns:
    -------
    - df_display : pandas.DataFrame
        A DataFrame containing only the explicit optimal hyperparameters for notebook visualization.
    """
    #===============================================================================
    # 1. Save the entire fitted pipeline
    #===============================================================================
    # Extract the classifier class name dynamically for a precise filename
    model_class_name = type(fitted_pipeline['clf']).__name__
    file_path = output_dir / f"{filename_prefix}_{model_class_name}.joblib"
    
    # Save the model object (contains preprocessing states, weights, and params)
    joblib.dump(fitted_pipeline, file_path)
    
    #===============================================================================
    # 2. Extract and display only the explicitly set optimal hyperparameters
    #===============================================================================
    # Extract hyperparameters from the specific estimator step ('clf')
    fitted_model_params = fitted_pipeline['clf'].get_params()
    
    # Convert parameters to a DataFrame for clean notebook rendering
    df_display = pd.DataFrame(
        list(fitted_model_params.items()), 
        columns=["Hyperparameter", "Optimal Value"]
    )
    
    # Filter out parameters that are None
    df_display = df_display.dropna(subset=["Optimal Value"])
    
    # Reset the index to make it sequential after removing rows
    df_display = df_display.reset_index(drop=True)
    df_display.index = df_display.index + 1

    return df_display

import pandas as pd
from pathlib import Path

def join_and_save_results(models_dict, output_dir):
    """
    Unifies multiple long-format model result DataFrames into a single master 
    DataFrame, adds a 'Model' column, removes the 'Fold' column, and saves it as a CSV.

    Parameters:
    ----------
    - models_dict : dict
        A dictionary where keys are model names (str) and values are pandas DataFrames
        containing ['Metric', 'Dataset', 'Score', 'Fold'] columns.
    - output_dir : str or pathlib.Path
        The directory where the output CSV file will be saved.

    Returns:
    -------
    - df_master : pandas.DataFrame
        The consolidated master DataFrame ready for global comparison plots.
    """
    processed_dfs = []

    # 1. Iterate over each model's DataFrame to add the 'Model' column and remove 'Fold'
    for model_name, df_model in models_dict.items():        
        # Make a copy to avoid modifying the original DataFrame
        df_copy = df_model.copy()
        
        # Add the 'Model' column with the current model's name
        df_copy['Model'] = model_name
        
        # Delete the 'Fold' column
        df_copy = df_copy.drop(columns=['Fold'], errors='ignore')
        
        # Append the processed DataFrame to the list
        processed_dfs.append(df_copy)
        
    # 2. Concatenate all processed DataFrames
    df_master = pd.concat(processed_dfs, ignore_index=True)
    
    # Reorder columns
    desired_order = ['Model', 'Metric', 'Dataset', 'Score']
    df_master = df_master[[col for col in desired_order if col in df_master.columns]]

    # 3. Guardar el archivo en formato CSV
    output_path = Path(output_dir) / "unified_results.csv"
    
    df_master.to_csv(output_path, index=False)
        
    return df_master

def save_roc_curves(model_names, fprs_list, tprs_list, output_dir=None, prefix="roc_curves"):
    """
    Builds a data frame in long format with the ROC curve data for multiple models and saves it to a CSV file.

    Parameters:
    ----------
    - model_names : list
        List of strings with the names of the models.
    - fprs_list : list of arrays/lists
        List that contains the lists of False Positive Rate values for each model.
    - tprs_list : list of arrays/lists
        List that contains the lists of True Positive Rate values for each model.
    - output_dir : str or pathlib.Path, optional
        Directory where the CSV file will be saved.

    Returns:
    -------
    - df_roc_long : DataFrame
        Unified DataFrame in long format.
    """
    
    # 1. List to hold the individual DataFrames for each model
    individuals_df= []
    
    # 2. Iterate over the models to create individual DataFrames
    for model, fprs, tprs in zip(model_names, fprs_list, tprs_list):
        df_temp = pd.DataFrame({
            'False Positive Rate': fprs,
            'True Positive Rate': tprs,
            'Model': model
        })
        # Add it to the list of DataFrames
        individuals_df.append(df_temp)
        
    # 3. Concatenate all individual DataFrames into a single long format DataFrame
    df_roc_long = pd.concat(individuals_df, ignore_index=True)
    
    # 4. Save the DataFrame if a path is provided
    if output_dir is not None:
        path = Path(output_dir)
        
        # Create the file name
        file_path = path / f"{prefix}_.csv"
        
        # Save it
        df_roc_long.to_csv(file_path, index=False)
        
    return df_roc_long


def save_pr_curves(model_names, precisions_list, recalls_list, output_dir=None, prefix="pr_curves"):
    """
    Builds a data frame in long format with the PR curve data for multiple models and saves it to a CSV file.

    Parameters:
    ----------
    - model_names : list
        List of strings with the names of the models.
    - precisions_list : list of arrays/lists
        List that contains the lists of Precision values for each model.
    - recalls_list : list of arrays/lists
        List that contains the lists of Recall values for each model.
    - output_dir : str or pathlib.Path, optional
        Directory where the CSV file will be saved.

    Returns:
    -------
    - df_pr_long : DataFrame
        Unified DataFrame in long format.
    """
    
    # 1. List to hold the individual DataFrames for each model
    individuals_df= []
    
    # 2. Iterate over the models to create individual DataFrames
    for model, precs, recs in zip(model_names, precisions_list, recalls_list):
        df_temp = pd.DataFrame({
            'Precision': precs,
            'Recall': recs,
            'Model': model
        })
        # Add it to the list of DataFrames
        individuals_df.append(df_temp)
        
    # 3. Concatenate all individual DataFrames into a single long format DataFrame
    df_pr_long = pd.concat(individuals_df, ignore_index=True)
    
    # 4. Save the DataFrame if a path is provided
    if output_dir is not None:
        path = Path(output_dir)
        
        # Create the file name
        file_path = path / f"{prefix}_.csv"
        
        # Save it
        df_pr_long.to_csv(file_path, index=False)
        
    return df_pr_long
