#%% Imports
import numpy as np
import pandas as pd
from scipy.stats import uniform, loguniform, randint

import logging
import warnings

import optuna
from optuna_integration import OptunaSearchCV
from optuna.samplers import TPESampler
from optuna import visualization as vis
from src.models.plots import plot_optimization_history
from sklearn.metrics import (
    accuracy_score, 
    precision_score, 
    recall_score, 
    f1_score, 
    roc_auc_score,
    roc_curve,
    precision_recall_curve,
    average_precision_score
    )
from sklearn.model_selection import cross_validate, RandomizedSearchCV

#%% Hyperparameters search space
hyperparameters_search_space = {
    # Elastic Net Logistic Regression
    'EN': {
    'clf__l1_ratio': uniform(0, 1),
    'clf__C': loguniform(1e-4, 1e3)
    },
    
    # Support Vector Machine
    'SVM': {
    'clf__C': loguniform(1e-5, 1e3),
    'clf__kernel': ['linear', 'rbf', 'poly', 'sigmoid'],
    'clf__gamma': ['scale', 'auto'] + list(np.logspace(-4, 1, 50)),
    'clf__degree': randint(2, 4),
    'clf__class_weight': [None, 'balanced']
    },
    
    # Decision Tree
    'DT': {
    'clf__max_depth': randint(2, 33),
    'clf__min_samples_split': randint(2, 21),
    'clf__min_samples_leaf': randint(1, 21),
    'clf__criterion': ['gini', 'entropy'],
    },
    
    # Random Forest
    'RF': {
    # Decision trees hyperparameters
    'clf__max_depth': [None] + list(range(2, 21)),
    'clf__min_samples_split': randint(2, 21),
    'clf__min_samples_leaf': randint(1, 21),
    'clf__criterion': ['gini', 'entropy'],
    
    # Ensemble bagging hyperparameters
    'clf__n_estimators': randint(50, 501),
    'clf__max_features': ['sqrt', 'log2', None] + list(np.arange(0.2, 0.9, 0.1)),
    'clf__class_weight': [None, 'balanced', 'balanced_subsample']
    },
    
    # Adaptive Boosting
    'AB': {
    # Decision trees hyperparameters
    'clf__estimator__max_depth': randint(1, 4), 
    'clf__estimator__max_features': ['sqrt', 'log2', None],
    'clf__estimator__min_samples_split': randint(2, 21),
    'clf__estimator__criterion': ['gini', 'entropy'],
    
    # Ensemble boosting hyperparameters
    'clf__n_estimators': randint(50, 501),
    'clf__learning_rate': loguniform(0.001, 1.0) 
    },
    
    # Gradient Boosting
    'GB': {
    # Decision trees hyperparameters
    'clf__max_depth': randint(2, 6),
    'clf__max_features': ['sqrt', 'log2', None] + list(np.arange(0.2, 0.9, 0.1)),
    'clf__min_samples_split': randint(2, 21),
    'clf__min_samples_leaf': randint(1, 21),
    
    # Ensemble boosting hyperparameters
    'clf__n_estimators': randint(50, 501),
    'clf__learning_rate': loguniform(0.001, 1),
    'clf__subsample': uniform(0.5, 0.5)
    },
    
    # Multilayer Perceptron
    'MLP': {
    'clf__hidden_layer_sizes': [(50,), (100,), (100, 50), (100, 100), (200, 100)],
    'clf__alpha': loguniform(1e-5, 1e-1),
    'clf__learning_rate_init': loguniform(0.001, 0.01),
    'clf__batch_size': np.logspace(start = 0, stop = 6, base = 2, num = 7).astype(int).tolist(),
    'clf__activation': ['relu', 'tanh', 'logistic'],
    'clf__solver': ['adam', 'sgd']
    }
}

#%% Training functions

def optimize_model_optuna_search(pipeline, param_distributions, 
                    X_train, y_train, X_test, y_test, 
                    metrics_dict,
                    aim='average_precision', 
                    cv=5, 
                    n_startup_trials=None,
                    n_iter=None, 
                    seed=42):
    """
    Optimizes a model using Bayesian optimization, trains it, and returns 
    cross-validation metrics (mean and standard deviation) and test metrics to assess overfitting,
    as well as the data for the Precision-Recall curve.

    Parameters:
    ----------
    - pipeline : sklearn.pipeline.Pipeline or estimator
        The model or pipeline to evaluate.
    - param_distributions : dict
        Dictionary containing Optuna distributions.
    - X_train, y_train : array-like
        Training data.
    - X_test, y_test : array-like
        Test data.
    - metrics_dict : dict
        Dictionary of metric names and their corresponding scorer strings (e.g., {'Accuracy': 
        'accuracy', ...})
    - aim : str, default="average_precision"
        Metric to optimize in the Optuna search.
    - cv : int, default=5
        Number of partitions (folds) for cross-validation.
    - n_startup_trials : int, default=None
        Number of initial trials for the sampler.
    - n_iter : int, default=None
        Number of iterations for the Optuna search.
    - seed : int, default=42
        Random seed for reproducibility.

    Returns:
    -------
    - best_model : fitted estimator
    - df_results_complete : pd.DataFrame (long format with train, validation, and test metrics)
    - fpr : array (false positive rates for the test ROC curve)
    - tpr : array (true positive rates for the test ROC curve)
    - precisions : array (precision values for the test PR curve)
    - recalls : array (recall values for the test PR curve)
    - study : optuna.study.Study (the Optuna study object)
    """
    
    print("Starting hyperparameter optimization...")
    
    # 1. Silencia los logs de información y advertencias generales de Optuna
    optuna.logging.set_verbosity(optuna.logging.ERROR)

    # 2. Silences specific warnings from Optuna
    warnings.filterwarnings(
    "ignore", 
    category=optuna.exceptions.ExperimentalWarning
)
        
    # ---------------------------------------------------------
    # 1. SET UP OPTUNA SEARCH
    # ---------------------------------------------------------
    
    # Get the number of hyperparameters to optimize
    D = len(param_distributions)
    
    # Define optuna search parameters based on the number of hyperparameters
    if n_startup_trials is None:
        n_startup_trials = 3*D
    
    if n_iter is None:
        n_iter = max(250, 3*D)
    
    # Instantiate a custom sampler
    custom_sampler = TPESampler(n_startup_trials=n_startup_trials, seed=seed)
    
    # Create a study object where the custom sampler can be injected
    custom_study = optuna.create_study(direction="maximize", sampler=custom_sampler)
    
    # ------------------------------------------
    # 2. HYPERPARAMETER OPTIMIZATION WITH OPTUNA
    # ------------------------------------------
    optuna_search = OptunaSearchCV(
        estimator=pipeline,
        param_distributions=param_distributions,
        n_trials=n_iter,
        study=custom_study,
        scoring=aim,
        cv=cv,
        random_state=seed, 
        n_jobs=1,
        verbose=0
    )
    
    # Fit the OptunaSearchCV to find the best hyperparameters
    optuna_search.fit(X_train, y_train)
    
    # Get the best model found by Optuna
    best_model = optuna_search.best_estimator_
    
    # Get the study
    study = optuna_search.study_

    # ---------------------------------------------------------
    # 3. EVALUATION OF THE BEST MODEL ON TRAINING (REFACTORED)
    # ---------------------------------------------------------
    print("Evaluating on the training set...")
        
    # Evaluate the best model using cross-validation on the training set
    cv_results = cross_validate(best_model, X_train, y_train, cv=cv, 
                                scoring=metrics_dict, return_train_score=True)
    
    # ---------------------------------------------------------
    # 4. FINAL EVALUATION ON TEST
    # ---------------------------------------------------------
    print("Evaluating on the test set...")
    
    # Predict on the test set
    y_pred_test = best_model.predict(X_test)

    # Compute probability scores for PR-AUC
    if hasattr(best_model, "predict_proba"):
        y_score_test = best_model.predict_proba(X_test)[:, 1]
    elif hasattr(best_model, "decision_function"):
        y_score_test = best_model.decision_function(X_test)
    else:
        y_score_test = None 
    
    metrics_test = {
        'Accuracy': accuracy_score(y_test, y_pred_test),
        'Precision': precision_score(y_test, y_pred_test, zero_division=0),
        'Recall': recall_score(y_test, y_pred_test, zero_division=0),
        'Specificity': recall_score(y_test, y_pred_test, pos_label=0, zero_division=0), 
        'F1-Score': f1_score(y_test, y_pred_test, zero_division=0)
    }

    if y_score_test is not None:
        metrics_test['ROC-AUC'] = roc_auc_score(y_test, y_score_test)
        fpr, tpr, thresholds = roc_curve(y_test, y_score_test)
        metrics_test['PR-AUC'] = average_precision_score(y_test, y_score_test)
        precisions, recalls, _ = precision_recall_curve(y_test, y_score_test)
    else:
        metrics_test['ROC-AUC'] = np.nan
        metrics_test['PR-AUC'] = np.nan
        precisions, recalls = None, None

    # ---------------------------------------------------------
    # 5. CONSTRUCTION OF THE FINAL RESULTS DATAFRAME
    # ---------------------------------------------------------
    cv_rows = []
    
    # Add train and validation metrics in long format to the list of rows
    for metric in metrics_dict.keys():
        train_scores = cv_results[f'train_{metric}']
        val_scores = cv_results[f'test_{metric}']
        
        for fold_idx, (t_score, v_score) in enumerate(zip(train_scores, val_scores)):
            cv_rows.append({'Metric': metric, 'Dataset': 'Train', 'Score': t_score, 'Fold': fold_idx})
            cv_rows.append({'Metric': metric, 'Dataset': 'Validation', 'Score': v_score, 'Fold': fold_idx})
            
    # Add test metrics as a single row 
    for metric, test_score in metrics_test.items():
        cv_rows.append({'Metric': metric, 'Dataset': 'Test', 'Score': test_score, 'Fold': 0})
            
    # Build the final data frame
    df_results_complete = pd.DataFrame(cv_rows)

    # Retornamos una estructura limpia con el DataFrame unificado
    return (best_model, df_results_complete, fpr, tpr, precisions, recalls, study)

def optimize_model_random_search(pipeline, param_distributions, 
                    X_train, y_train, X_test, y_test, 
                    metrics_dict,
                    aim='average_precision', 
                    cv=5, 
                    n_iter=20, 
                    seed=42):
    """
    Optimizes a model using Randomized Search, trains it, and returns 
    cross-validation metrics (mean and standard deviation) and test metrics to assess overfitting,
    as well as the data for the Precision-Recall curve.

    Parameters:
    ----------
    - pipeline : sklearn.pipeline.Pipeline or estimator
        The model or pipeline to evaluate.
    - param_distributions : dict
        Dictionary containing parameters to sample from.
    - X_train, y_train : array-like
        Training data.
    - X_test, y_test : array-like
        Test data.
    - metrics_dict : dict
        Dictionary of metric names and their corresponding scorer strings (e.g., {'Accuracy': 
        'accuracy', ...})
    - aim : str, default="average_precision"
        Metric to optimize in the RandomizedSearchCV.
    - cv : int, default=5
        Number of partitions (folds) for cross-validation.
    - n_iter : int, default=20
        Number of iterations for the Randomized search.
    - seed : int, default=42
        Random seed for reproducibility.

    Returns:
    -------
    - best_model : fitted estimator
    - df_results_complete : pd.DataFrame (unified results across train, validation, and test)
    - fpr : array (false positive rates for the test ROC curve)
    - tpr : array (true positive rates for the test ROC curve)
    - precisions : array (precision values for the test PR curve)
    - recalls : array (recall values for the test PR curve)
    - random_search : RandomizedSearchCV (the fitted search object)
    """
    
    # ---------------------------------------------------------
    # 1. HYPERPARAMETER OPTIMIZATION WITH RANDOMIZEDSEARCHCV
    # ---------------------------------------------------------
    print("Starting randomized hyperparameter optimization...")
    
    # Set up the search 
    random_search = RandomizedSearchCV(
        estimator=pipeline,
        param_distributions=param_distributions,
        n_iter=n_iter,
        scoring=aim,
        cv=cv,
        random_state=seed, 
        n_jobs=-1,
        verbose=1
    )
    
    # Fit the RandomizedSearchCV to find the best hyperparameters
    random_search.fit(X_train, y_train)
    
    # Get the best model found by the search
    best_model = random_search.best_estimator_

    # ---------------------------------------------------------
    # 2. EVALUATION OF THE BEST MODEL ON TRAINING (REFACTORED)
    # ---------------------------------------------------------
    print("Evaluating on the training set...")
        
    # Evaluate the best model using cross-validation on the training set
    cv_results = cross_validate(best_model, X_train, y_train, cv=cv, 
                                scoring=metrics_dict, return_train_score=True)
    
    # ---------------------------------------------------------
    # 3. FINAL EVALUATION ON TEST
    # ---------------------------------------------------------
    print("Evaluating on the test set...")
    
    # Predict on the test set
    y_pred_test = best_model.predict(X_test)

    # Compute probability scores for PR-AUC
    if hasattr(best_model, "predict_proba"):
        y_score_test = best_model.predict_proba(X_test)[:, 1]
    elif hasattr(best_model, "decision_function"):
        y_score_test = best_model.decision_function(X_test)
    else:
        y_score_test = None 
    
    metrics_test = {
        'Accuracy': accuracy_score(y_test, y_pred_test),
        'Precision': precision_score(y_test, y_pred_test, zero_division=0),
        'Recall': recall_score(y_test, y_pred_test, zero_division=0),
        'Specificity': recall_score(y_test, y_pred_test, pos_label=0, zero_division=0), 
        'F1-Score': f1_score(y_test, y_pred_test, zero_division=0)
    }

    if y_score_test is not None:
        metrics_test['ROC-AUC'] = roc_auc_score(y_test, y_score_test)
        fpr, tpr, thresholds = roc_curve(y_test, y_score_test)
        metrics_test['PR-AUC'] = average_precision_score(y_test, y_score_test)
        precisions, recalls, _ = precision_recall_curve(y_test, y_score_test)
    else:
        metrics_test['ROC-AUC'] = np.nan
        metrics_test['PR-AUC'] = np.nan
        precisions, recalls = None, None

    # ---------------------------------------------------------
    # 4. CONSTRUCTION OF THE FINAL RESULTS DATAFRAME
    # ---------------------------------------------------------
    cv_rows = []
    
    # Add train and validation metrics in long format to the list of rows
    for metric in metrics_dict.keys():
        train_scores = cv_results[f'train_{metric}']
        val_scores = cv_results[f'test_{metric}']
        
        for fold_idx, (t_score, v_score) in enumerate(zip(train_scores, val_scores)):
            cv_rows.append({'Metric': metric, 'Dataset': 'Train', 'Score': t_score, 'Fold': fold_idx})
            cv_rows.append({'Metric': metric, 'Dataset': 'Validation', 'Score': v_score, 'Fold': fold_idx})
            
    # Add test metrics as a single row 
    for metric, test_score in metrics_test.items():
        cv_rows.append({'Metric': metric, 'Dataset': 'Test', 'Score': test_score, 'Fold': 0})
            
    # Build the final data frame
    df_results_complete = pd.DataFrame(cv_rows)

    return (best_model, df_results_complete, fpr, tpr, precisions, recalls)
