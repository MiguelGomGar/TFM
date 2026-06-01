import numpy as np
import optuna
from optuna.integration import OptunaSearchCV
from optuna.samplers import TPESampler
from optuna import visualization as vis
from sklearn.metrics import (
    accuracy_score, 
    precision_score, 
    recall_score, 
    f1_score, 
    roc_auc_score,
    roc_curve,
    precision_recall_curve,
    average_precision_score,
    make_scorer
    )
from sklearn.model_selection import cross_validate, RandomizedSearchCV

def optimize_model_optuna_search(pipeline, 
                    param_distributions, 
                    X_train, y_train, X_test, y_test, 
                    aim='average_precision', 
                    cv=5, 
                    n_startup_trials=30,
                    n_iter=100, 
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
    - aim : str, default="average_precision"
        Metric to optimize in the Optuna search.
    - cv : int, default=5
        Number of partitions (folds) for cross-validation.
    - n_startup_trials : int, default=10
        Number of initial trials for the sampler.
    - n_iter : int, default=50
        Number of iterations for the Optuna search.
    - seed : int, default=42
        Random seed for reproducibility.

    Returns:
    -------
    - best_model : fitted estimator
    - metrics_train_mean : dict (mean of training metrics)
    - metrics_train_std : dict (standard deviation of training metrics)
    - metrics_val_mean : dict (mean of CV metrics during training)
    - metrics_val_std : dict (standard deviation of CV metrics during training)
    - metrics_test : dict (metrics from the final evaluation on the test set)
    - precisions : array (precision values for the test PR curve)
    - recalls : array (recall values for the test PR curve)
    """
    
    # ---------------------------------------------------------
    # 1. HYPERPARAMETER OPTIMIZATION WITH OPTUNA
    # ---------------------------------------------------------
    print("Starting hyperparameter optimization...")
    
    # Instantiate a custom sampler
    custom_sampler = TPESampler(n_startup_trials=n_startup_trials, seed=seed)
    
    # Create a study object where the custom sampler can be injected
    custom_study = optuna.create_study(direction="maximize",
                                sampler=custom_sampler)
    
    # Set up the search 
    optuna_search = OptunaSearchCV(
        estimator=pipeline,
        param_distributions=param_distributions,
        n_trials=n_iter,
        study=custom_study,
        scoring=aim,
        cv=cv,
        random_state=seed , n_jobs=-1
    )
    
    # Fit the OptunaSearchCV to find the best hyperparameters
    optuna_search.fit(X_train, y_train)
    
    # Get the best model found by Optuna
    best_model = optuna_search.best_estimator_
    
    # Get the study
    study = optuna_search.study_

    # Display the optimization history plot
    fig = vis.plot_optimization_history(study)
    fig.show()
    
    # ---------------------------------------------------------
    # 2. EVALUATION OF THE BEST MODEL ON TRAINING
    # ---------------------------------------------------------
    print("Evaluating on the training set...")
    
    # Define the metrics we want to evaluate
    specificity_score = make_scorer(recall_score, pos_label=0)
    
    scoring_dict = {
        'Accuracy': 'accuracy',
        'Precision': 'precision',
        'Recall': 'recall',
        'Specificity': specificity_score,
        'F1-Score': 'f1',
        'ROC-AUC': 'roc_auc',
        'PR-AUC': 'average_precision'
    }
    
    # Evaluate the best model using cross-validation on the training set
    cv_results = cross_validate(best_model, X_train, y_train, cv=cv, 
                                scoring=scoring_dict, return_train_score=True)
    
    # Compute means and standard deviations
    metrics_train_mean = {
        'Accuracy': cv_results['train_Accuracy'].mean(),
        'Precision': cv_results['train_Precision'].mean(),
        'Recall': cv_results['train_Recall'].mean(),
        'Specificity': cv_results['train_Specificity'].mean(),
        'F1-Score': cv_results['train_F1-Score'].mean(),
        'ROC-AUC': cv_results['train_ROC-AUC'].mean(),
        'PR-AUC': cv_results['train_PR-AUC'].mean()
    }
    
    metrics_train_std = {
        'Accuracy': cv_results['train_Accuracy'].std(),
        'Precision': cv_results['train_Precision'].std(),
        'Recall': cv_results['train_Recall'].std(),
        'Specificity': cv_results['train_Specificity'].std(),
        'F1-Score': cv_results['train_F1-Score'].std(),
        'ROC-AUC': cv_results['train_ROC-AUC'].std(),
        'PR-AUC': cv_results['train_PR-AUC'].std()
    }
    
    metrics_val_mean = {
        'Accuracy': cv_results['test_Accuracy'].mean(),
        'Precision': cv_results['test_Precision'].mean(),
        'Recall': cv_results['test_Recall'].mean(),
        'Specificity': cv_results['test_Specificity'].mean(),
        'F1-Score': cv_results['test_F1-Score'].mean(),
        'ROC-AUC': cv_results['test_ROC-AUC'].mean(),
        'PR-AUC': cv_results['test_PR-AUC'].mean()
    }
    
    metrics_val_std = {
        'Accuracy': cv_results['test_Accuracy'].std(),
        'Precision': cv_results['test_Precision'].std(),
        'Recall': cv_results['test_Recall'].std(),
        'Specificity': cv_results['test_Specificity'].std(),
        'F1-Score': cv_results['test_F1-Score'].std(),
        'ROC-AUC': cv_results['test_ROC-AUC'].std(),
        'PR-AUC': cv_results['test_PR-AUC'].std()
    }

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

    # Compute test metrics
    metrics_test = {
        'Accuracy': accuracy_score(y_test, y_pred_test),
        'Precision': precision_score(y_test, y_pred_test, zero_division=0),
        'Recall': recall_score(y_test, y_pred_test, zero_division=0),
        'Specificity': recall_score(y_test, y_pred_test, pos_label=0, zero_division=0), 
        'F1-Score': f1_score(y_test, y_pred_test, zero_division=0)
    }

    # Compute ROC-curve, Precision-Recall curve and respective AUCs if scores are available
    if y_score_test is not None:
        metrics_test['ROC-AUC'] = roc_auc_score(y_test, y_score_test)
        fpr, tpr, thresholds = roc_curve(y_test, y_score_test)
        
        metrics_test['PR-AUC'] = average_precision_score(y_test, y_score_test)
        precisions, recalls, _ = precision_recall_curve(y_test, y_score_test)
    else:
        metrics_test['ROC-AUC'] = np.nan
        metrics_test['PR-AUC'] = np.nan
        precisions, recalls = None, None
    
    print("\n")
    print("Training and evaluation completed!")

    return (best_model, 
            metrics_train_mean, metrics_train_std, metrics_val_mean, 
            metrics_val_std, metrics_test, 
            fpr, tpr,
            precisions, recalls)

def optimize_model_random_search(pipeline, 
                    param_distributions, 
                    X_train, y_train, X_test, y_test, 
                    scoring='PR-AUC', 
                    cv=5, 
                    n_trials=100, 
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
        Dictionary containing hyperparameters distributions.
    - X_train, y_train : array-like
        Training data.
    - X_test, y_test : array-like
        Test data.
    - scoring : str, default="recall"
        Metric to optimize in the Optuna search.
    - cv : int, default=5
        Number of partitions (folds) for cross-validation.
    - n_trials : int, default=1000
        Optuna iterations.
    - seed : int, default=42
        Random seed for reproducibility.

    Returns:
    -------
    - best_model : fitted estimator
    - metrics_train_mean : dict (mean of training metrics)
    - metrics_train_std : dict (standard deviation of training metrics)
    - metrics_val_mean : dict (mean of CV metrics during training)
    - metrics_val_std : dict (standard deviation of CV metrics during training)
    - metrics_test : dict (metrics from the final evaluation on the test set)
    - precisions : array (precision values for the test PR curve)
    - recalls : array (recall values for the test PR curve)
    """
    
    # ---------------------------------------------------------
    # 1. HYPERPARAMETER OPTIMIZATION WITH OPTUNA
    # ---------------------------------------------------------
    print("Starting hyperparameter optimization...")
    
    # Set up RandomizedSearchCV with the provided pipeline and parameter distributions
    random_search = RandomizedSearchCV(
        estimator=pipeline,
        param_distributions=param_distributions,
        n_iter=n_trials,
        scoring=scoring,
        cv=cv,
        random_state=seed 
    )
    
    # Fit the RandomizedSearchCV to find the best hyperparameters
    random_search.fit(X_train, y_train)
    
    # Get the best model found by RandomizedSearchCV
    best_model = random_search.best_estimator_
    
    # ---------------------------------------------------------
    # 2. EVALUATION OF THE BEST MODEL ON TRAINING
    # ---------------------------------------------------------
    print("Evaluating on the training set...")
    
    # Define the metrics we want to evaluate
    specificity_score = make_scorer(recall_score, pos_label=0)
    scoring_dict = {
        'Accuracy': 'accuracy',
        'Precision': 'precision',
        'Recall': 'recall',
        'Specificity': specificity_score,
        'F1-Score': 'f1',
        'ROC-AUC': 'roc_auc',
        'PR-AUC': 'average_precision'
    }
    
    # Evaluate the best model using cross-validation on the training set
    cv_results = cross_validate(best_model, X_train, y_train, cv=cv, 
                                scoring=scoring_dict, n_jobs=1, return_train_score=True)
    
    # Compute means and standard deviations
    metrics_train_mean = {
        'Accuracy': cv_results['train_Accuracy'].mean(),
        'Precision': cv_results['train_Precision'].mean(),
        'Recall': cv_results['train_Recall'].mean(),
        'Specificity': cv_results['train_Specificity'].mean(),
        'F1-Score': cv_results['train_F1-Score'].mean(),
        'ROC-AUC': cv_results['train_ROC-AUC'].mean(),
        'PR-AUC': cv_results['train_PR-AUC'].mean()
    }
    
    metrics_train_std = {
        'Accuracy': cv_results['train_Accuracy'].std(),
        'Precision': cv_results['train_Precision'].std(),
        'Recall': cv_results['train_Recall'].std(),
        'Specificity': cv_results['train_Specificity'].std(),
        'F1-Score': cv_results['train_F1-Score'].std(),
        'ROC-AUC': cv_results['train_ROC-AUC'].std(),
        'PR-AUC': cv_results['train_PR-AUC'].std()
    }
    
    metrics_val_mean = {
        'Accuracy': cv_results['test_Accuracy'].mean(),
        'Precision': cv_results['test_Precision'].mean(),
        'Recall': cv_results['test_Recall'].mean(),
        'Specificity': cv_results['test_Specificity'].mean(),
        'F1-Score': cv_results['test_F1-Score'].mean(),
        'ROC-AUC': cv_results['test_ROC-AUC'].mean(),
        'PR-AUC': cv_results['test_PR-AUC'].mean()
    }
    
    metrics_val_std = {
        'Accuracy': cv_results['test_Accuracy'].std(),
        'Precision': cv_results['test_Precision'].std(),
        'Recall': cv_results['test_Recall'].std(),
        'Specificity': cv_results['test_Specificity'].std(),
        'F1-Score': cv_results['test_F1-Score'].std(),
        'ROC-AUC': cv_results['test_ROC-AUC'].std(),
        'PR-AUC': cv_results['test_PR-AUC'].std()
    }

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

    # Compute test metrics
    metrics_test = {
        'Accuracy': accuracy_score(y_test, y_pred_test),
        'Precision': precision_score(y_test, y_pred_test, zero_division=0),
        'Recall': recall_score(y_test, y_pred_test, zero_division=0),
        'Specificity': recall_score(y_test, y_pred_test, pos_label=0, zero_division=0), 
        'F1-Score': f1_score(y_test, y_pred_test, zero_division=0)
    }

    # Compute PR-AUC and Precision-Recall curve data if scores are available
    if y_score_test is not None:
        metrics_test['ROC-AUC'] = roc_auc_score(y_test, y_score_test)
        metrics_test['PR-AUC'] = average_precision_score(y_test, y_score_test)
        
        tpr, fpr, thresholds = roc_curve(y_test, y_score_test)
        precisions, recalls, _ = precision_recall_curve(y_test, y_score_test)
    else:
        metrics_test['ROC-AUC'] = np.nan
        metrics_test['PR-AUC'] = np.nan
        precisions, recalls = None, None
    
    print("\n")
    print("Training and evaluation completed!")

    return (best_model, 
            metrics_train_mean, metrics_train_std, 
            metrics_val_mean, metrics_val_std, 
            metrics_test, 
            tpr, fpr,
            precisions, recalls)