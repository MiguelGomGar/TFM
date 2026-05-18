import pandas as pd
from pathlib import Path

def save_model_results(model_names, 
                    test_metrics, 
                    train_metrics=None, 
                    validation_metrics=None, 
                    results_path=None):
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
        
        df_test.to_csv(path / "test_results.csv", index=True)
        
        if df_train is not None:
            df_train.to_csv(path / "train_results.csv", index=True)
            
        if df_val is not None:
            df_val.to_csv(path / "validation_results.csv", index=True)

    return df_test, df_train, df_val