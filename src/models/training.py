import numpy as np
import optuna
from optuna.integration import OptunaSearchCV
from optuna import visualization as vis
from sklearn.metrics import (
    accuracy_score, 
    precision_score, 
    recall_score, 
    f1_score, 
    precision_recall_curve, 
    average_precision_score
)
from sklearn.model_selection import cross_validate 

optuna.logging.set_verbosity(optuna.logging.WARNING)

def optimize_model(pipeline, 
                    param_distributions, 
                    X_train, y_train, X_test, y_test, 
                    scoring='average_precision', 
                    cv=5, 
                    n_trials=50):
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
    - scoring : str, default="average_precision"
        Metric to optimize in the Optuna search.
    - cv : int, default=5
        Number of partitions (folds) for cross-validation.
    - n_trials : int, default=50
        Optuna iterations.

    Returns:
    -------
    - best_model : fitted estimator
    - metrics_test : dict (metrics from the final evaluation on the test set)
    - metrics_val_mean : dict (mean of CV metrics during training)
    - metrics_val_std : dict (standard deviation of CV metrics during training)
    - precisions : array (precision values for the test PR curve)
    - recalls : array (recall values for the test PR curve)
    """
    
    # 1. Setup OptunaSearchCV with the provided pipeline and parameter distributions
    optuna_search = OptunaSearchCV(
        estimator=pipeline,
        param_distributions=param_distributions,
        n_trials=n_trials,
        scoring=scoring,
        cv=cv,
        n_jobs=-1,      
        random_state=42 
    )

    # ---------------------------------------------------------
    # 2. HYPERPARAMETER OPTIMIZATION WITH OPTUNA
    # ---------------------------------------------------------
    print("Starting hyperparameter optimization...")
    
    # Fit the OptunaSearchCV to find the best hyperparameters
    optuna_search.fit(X_train, y_train)
    
    # Retrieve the best model found by Optuna
    best_model = optuna_search.best_estimator_
    
    # Get the study
    study = optuna_search.study_

    # Display the plot
    fig = vis.plot_optimization_history(study)
    fig.show()
    
    # ---------------------------------------------------------
    # 3. EVALUATION OF THE BEST MODEL ON TRAINING
    # ---------------------------------------------------------
    print("Evaluating on the training set...")
    
    # Define the metrics we want to evaluate
    scoring_dict = {
        'Accuracy': 'accuracy',
        'Precision': 'precision',
        'Recall': 'recall',
        'F1-Score': 'f1',
        'PR-AUC': 'average_precision'
    }
    
    # Evaluate the best model using cross-validation on the training set
    cv_results = cross_validate(best_model, X_train, y_train, cv=cv, 
                                scoring=scoring_dict, n_jobs=-1)
    
    # Compute means and standard deviations
    metrics_val_mean = {
        'Accuracy': cv_results['test_Accuracy'].mean(),
        'Precision': cv_results['test_Precision'].mean(),
        'Recall': cv_results['test_Recall'].mean(),
        'F1-Score': cv_results['test_F1-Score'].mean(),
        'PR-AUC': cv_results['test_PR-AUC'].mean()
    }
    
    metrics_val_std = {
        'Accuracy': cv_results['test_Accuracy'].std(),
        'Precision': cv_results['test_Precision'].std(),
        'Recall': cv_results['test_Recall'].std(),
        'F1-Score': cv_results['test_F1-Score'].std(),
        'PR-AUC': cv_results['test_PR-AUC'].std()
    }

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

    # Compute test metrics
    metrics_test = {
        'Accuracy': accuracy_score(y_test, y_pred_test),
        'Precision': precision_score(y_test, y_pred_test, zero_division=0),
        'Recall': recall_score(y_test, y_pred_test, zero_division=0),
        'F1-Score': f1_score(y_test, y_pred_test, zero_division=0)
    }

    # Compute PR-AUC and Precision-Recall curve data if scores are available
    if y_score_test is not None:
        metrics_test['PR-AUC'] = average_precision_score(y_test, y_score_test)
        precisions, recalls, _ = precision_recall_curve(y_test, y_score_test)
    else:
        metrics_test['PR-AUC'] = np.nan
        precisions, recalls = None, None
    
    print("\n")
    print("Training and evaluation completed!")

    return best_model, metrics_test, metrics_val_mean, metrics_val_std, precisions, recalls