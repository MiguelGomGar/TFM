import pandas as pd
from pathlib import Path

def save_scores(model_names, 
                    test_metrics, 
                    train_metrics=None, 
                    validation_metrics=None, 
                    results_path=None,
                    prefix="scores"):
    """
    Groups the metrics of different models into DataFrames and saves them in CSV files.

    Parameters:
    ----------
    - model_names : list
        List of strings with the names of the models (e.g., ['SVM', 'KNN', 'RF']).
    - test_metrics : list
        List of dictionaries with the test metrics (REQUIRED).
    - train_metrics : list, optional
        List of dictionaries with the training metrics.
    - validation_metrics : list, optional
        List of dictionaries with the validation metrics.
    - results_path : str o pathlib.Path, opcional
        Path to the directory where the CSV files will be saved.
    - prefix : str, optional
        Prefix for the CSV file names.

    Returns:
    -------
    - df_test, df_train, df_val : DataFrames (if train and validation metrics are provided)
    """
    
    # 1. Process test metrics
    test_dict = {name: metrics for name, metrics in zip(model_names, test_metrics)}
    df_test = pd.DataFrame.from_dict(test_dict, orient='index')
    
    # 2. Process training and validation metrics (optional)
    df_train = None
    if train_metrics is not None:
        train_dict = {name: metrics for name, metrics in zip(model_names, train_metrics)}
        df_train = pd.DataFrame.from_dict(train_dict, orient='index')
        
    df_val = None
    if validation_metrics is not None:
        val_dict = {name: metrics for name, metrics in zip(model_names, validation_metrics)}
        df_val = pd.DataFrame.from_dict(val_dict, orient='index')
        
    # 3. Save in CSV format if a path is specified
    if results_path is not None:
        path = Path(results_path)
        
        df_test.to_csv(path / f"{prefix}_test_results.csv", index=True)
        
        if df_train is not None:
            df_train.to_csv(path / f"{prefix}_train_results.csv", index=True)
            
        if df_val is not None:
            df_val.to_csv(path / f"{prefix}_validation_results.csv", index=True)

    return df_test, df_train, df_val

def save_pr_curves_long_format(model_names, 
                            precisions_list, recalls_list, 
                            results_path=None, prefix="pr_curves"):
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
    - results_path : str or pathlib.Path, optional
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
    if results_path is not None:
        path = Path(results_path)
        
        # Create the file name
        file_path = path / f"{prefix}_pr_curves_long.csv"
        
        # Save it
        df_pr_long.to_csv(file_path, index=False)
        
    return df_pr_long