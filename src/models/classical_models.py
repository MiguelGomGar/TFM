import pandas as pd
import optuna
from optuna.integration import OptunaSearchCV
from sklearn.metrics import (
    accuracy_score, 
    precision_score, 
    recall_score, 
    f1_score, 
    precision_recall_curve, 
    average_precision_score
)

# Disable detailed Optuna logs so they don't clutter the notebook
optuna.logging.set_verbosity(optuna.logging.WARNING)

def optimize_and_evaluate_model(pipeline, param_distributions, X_train, y_train, X_test, y_test, scoring='average_precision', cv=5, n_trials=50):
    """
    Optimizes a model using OptunaSearchCV (Bayesian optimization), trains it, 
    and returns the data for the Precision-Recall curve.

    Parameters:
    ----------
    pipeline : sklearn.pipeline.Pipeline or estimator
        The model or pipeline to evaluate.
    param_distributions : dict
        Dictionary with the Optuna distributions to explore.
    X_train, y_train : array-like
        Training data.
    X_test, y_test : array-like
        Testing data.
    scoring : str, default='average_precision'
        Main metric to optimize (PR-AUC is ideal for imbalanced data).
    cv : int, default=5
        Number of cross-validation folds.
    n_trials : int, default=50
        Number of intelligent iterations Optuna will perform.

    Returns:
    -------
    best_model : fitted estimator
        The trained model.
    metrics : dict
        Test metrics (Accuracy, Precision, Recall, F1, PR-AUC).
    precisions : array
        Precision values for the PR curve.
    recalls : array
        Recall values for the PR curve.
    """
    
    # 1. Configure OptunaSearchCV
    optuna_search = OptunaSearchCV(
        estimator=pipeline,
        param_distributions=param_distributions,
        n_trials=n_trials,
        scoring=scoring,
        cv=cv,
        n_jobs=-1,      # Use all available CPU cores
        random_state=42 # For reproducibility
    )

    # 2. Fit the model (Optuna will intelligently search for the best parameters)
    optuna_search.fit(X_train, y_train)
    best_model = optuna_search.best_estimator_

    # 3. Direct predictions for discrete metrics
    y_pred = best_model.predict(X_test)

    # 4. Get probabilities or decision scores for the PR curve
    if hasattr(best_model, "predict_proba"):
        y_score = best_model.predict_proba(X_test)[:, 1]
    elif hasattr(best_model, "decision_function"):
        y_score = best_model.decision_function(X_test)
    else:
        y_score = None 

    # 5. Calculate basic metrics
    metrics = {
        'Accuracy': accuracy_score(y_test, y_pred),
        'Precision': precision_score(y_test, y_pred, zero_division=0),
        'Recall': recall_score(y_test, y_pred, zero_division=0),
        'F1-Score': f1_score(y_test, y_pred, zero_division=0)
    }

    # 6. Calculate PR curve and PR-AUC
    if y_score is not None:
        precisions, recalls, _ = precision_recall_curve(y_test, y_score)
        metrics['PR-AUC'] = average_precision_score(y_test, y_score)
    else:
        precisions, recalls = None, None
        metrics['PR-AUC'] = None

    return best_model, metrics, precisions, recalls