# %% Imports
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import uniform, loguniform, randint

from sklearn.metrics import (
    accuracy_score,
    precision_score,
    recall_score,
    f1_score,
    roc_auc_score,
    roc_curve,
    precision_recall_curve,
    average_precision_score,
    auc,
)
from sklearn.model_selection import cross_validate, RandomizedSearchCV

# %% Hyperparameters search space
clinical_hyperparameters_search_space = {
    # Elastic Net Logistic Regression
    "EN": {
        # Avoid very weak regularization values (large C)
        "clf__l1_ratio": uniform(0.1, 0.9),
        "clf__C": loguniform(1e-3, 1e1),
    },
    # Support Vector Machine
    "SVM": {
        "clf__C": loguniform(1e-3, 1e2),
        "clf__kernel": ["linear", "rbf"],
        "clf__gamma": ["scale", "auto"] + list(np.logspace(-5, -1, 25)),
        "clf__class_weight": [None, "balanced"],
    },
    # Decision Tree
    "DT": {
        "clf__max_depth": randint(2, 13),
        "clf__min_samples_split": randint(8, 41),
        "clf__min_samples_leaf": randint(4, 21),
        "clf__criterion": ["gini", "entropy"],
        "clf__ccp_alpha": loguniform(1e-6, 1e-2),
    },
    # Random Forest
    "RF": {
        "clf__max_depth": [None] + list(range(3, 13)),
        "clf__min_samples_split": randint(8, 31),
        "clf__min_samples_leaf": randint(3, 16),
        "clf__criterion": ["gini", "entropy"],
        "clf__ccp_alpha": loguniform(1e-7, 1e-3),
        "clf__n_estimators": randint(100, 401),
        "clf__max_features": ["sqrt", "log2"] + list(np.arange(0.2, 0.7, 0.1)),
        "clf__class_weight": [None, "balanced", "balanced_subsample"],
        "clf__max_samples": list(np.arange(0.6, 1.0, 0.1)),
    },
    # Extra Trees
    "ET": {
        "clf__max_depth": [None] + list(range(3, 13)),
        "clf__min_samples_split": randint(8, 31),
        "clf__min_samples_leaf": randint(3, 16),
        "clf__criterion": ["gini", "entropy"],
        "clf__ccp_alpha": loguniform(1e-7, 1e-3),
        "clf__n_estimators": randint(100, 401),
        "clf__max_features": ["sqrt", "log2"] + list(np.arange(0.2, 0.7, 0.1)),
        "clf__class_weight": [None, "balanced", "balanced_subsample"],
        "clf__bootstrap": [True],
        "clf__max_samples": list(np.arange(0.6, 1.0, 0.1)),
    },
    # Adaptive Boosting
    "AB": {
        "clf__estimator__max_depth": randint(1, 3),
        "clf__estimator__max_features": ["sqrt", "log2", None],
        "clf__estimator__min_samples_split": randint(10, 41),
        "clf__estimator__min_samples_leaf": randint(4, 16),
        "clf__estimator__criterion": ["gini", "entropy"],
        "clf__estimator__ccp_alpha": loguniform(1e-7, 1e-3),
        "clf__n_estimators": randint(50, 301),
        "clf__learning_rate": loguniform(0.01, 0.3),
    },
    # Gradient Boosting
    "GB": {
        "clf__max_depth": randint(2, 4),
        "clf__max_features": ["sqrt", "log2"] + list(np.arange(0.2, 0.7, 0.1)),
        "clf__min_samples_split": randint(8, 31),
        "clf__min_samples_leaf": randint(3, 16),
        "clf__n_estimators": randint(80, 351),
        "clf__learning_rate": loguniform(0.01, 0.2),
        "clf__subsample": uniform(0.6, 0.3),
    },
    # Multilayer Perceptron
    "MLP": {
        "clf__hidden_layer_sizes": [(32,), (64,), (64, 32), (128, 64)],
        "clf__alpha": loguniform(1e-4, 1e0),
        "clf__learning_rate_init": loguniform(1e-4, 5e-3),
        "clf__batch_size": [16, 32, 64, 128],
        "clf__activation": ["relu", "tanh", "logistic"],
        "clf__solver": ["adam", "sgd"],
    },
}

proteomic_hyperparameters_search_space = {
    # Elastic Net Logistic Regression
    "EN": {"clf__l1_ratio": uniform(0.01, 0.98), "clf__C": loguniform(1e-4, 1e2)},
    # Support Vector Machine
    "SVM": {
        "clf__C": loguniform(1e-3, 1e3),
        "clf__kernel": ["linear", "rbf"],
        "clf__gamma": ["scale", "auto"] + list(np.logspace(-4, -1, 10)),
        "clf__class_weight": [None, "balanced"],
    },
    # Decision Tree
    "DT": {
        "clf__max_depth": randint(2, 11),
        "clf__min_samples_split": randint(5, 21),
        "clf__min_samples_leaf": randint(3, 15),
        "clf__criterion": ["gini", "entropy"],
    },
    # Random Forest
    "RF": {
        "clf__n_estimators": randint(50, 301),
        "clf__max_depth": randint(3, 13),
        "clf__min_samples_split": randint(5, 21),
        "clf__min_samples_leaf": randint(3, 15),
        "clf__max_features": ["sqrt", "log2"],
        "clf__bootstrap": [True],
        "clf__max_samples": uniform(0.5, 0.4),
    },
    # Extra Trees
    "ET": {
        "clf__n_estimators": randint(50, 301),
        "clf__max_depth": randint(3, 13),
        "clf__min_samples_split": randint(5, 21),
        "clf__min_samples_leaf": randint(3, 15),
        "clf__max_features": ["sqrt", "log2"],
        "clf__bootstrap": [True],
        "clf__max_samples": uniform(0.5, 0.4),
    },
    # AdaBoost
    "AB": {
        "clf__n_estimators": randint(50, 301),
        "clf__learning_rate": loguniform(1e-3, 0.5),
        "clf__estimator__max_depth": randint(1, 3),
    },
    # Gradient Boosting
    "GB": {
        "clf__n_estimators": randint(50, 301),
        "clf__max_depth": randint(2, 6),
        "clf__learning_rate": loguniform(1e-3, 0.2),
        "clf__subsample": uniform(0.5, 0.4),
        "clf__min_samples_leaf": randint(3, 15),
        "clf__max_features": ["sqrt", "log2"],
    },
    # Multi-layer Perceptron (Neural Network)
    "MLP": {
        "clf__hidden_layer_sizes": [(50,), (100,), (50, 50)],
        "clf__activation": ["relu", "tanh"],
        "clf__alpha": loguniform(1e-4, 1e0),
        "clf__learning_rate_init": loguniform(1e-4, 1e-2),
        "clf__early_stopping": [True],
    },
}


# %% Training function
def optimize_model_random_search(
    pipeline,
    param_distributions,
    X_train,
    y_train,
    X_test,
    y_test,
    metrics_dict,
    aim="average_precision",
    cv=5,
    n_iter=20,
    seed=42,
):
    """
    Optimizes a model using Randomized Search, trains it, and returns
    cross-validation metrics (mean and standard deviation) and test metrics to
    assess overfitting, as well as the data for the Precision-Recall curve.

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
        Dictionary of metric names and their corresponding scorer strings (e.g.,
        {'Accuracy': 'accuracy', ...})
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
    - df_results_complete : pd.DataFrame (unified results across train,
    validation, and test)
    - fpr : array (false positive rates for the test ROC curve)
    - tpr : array (true positive rates for the test ROC curve)
    - precisions : array (precision values for the test PR curve)
    - recalls : array (recall values for the test PR curve)
    - random_search : RandomizedSearchCV (the fitted search object)
    """

    # ---------------------------------------------------------
    # 1. HYPERPARAMETER OPTIMIZATION WITH RANDOMIZED SEARCH CV
    # ---------------------------------------------------------
    # Set up the search
    random_search = RandomizedSearchCV(
        estimator=pipeline,
        param_distributions=param_distributions,
        n_iter=n_iter,
        scoring=aim,
        cv=cv,
        random_state=seed,
        n_jobs=-1,
        verbose=1,
    )

    # Fit the RandomizedSearchCV to find the best hyperparameters
    random_search.fit(X_train, y_train)

    # Get the best model found by the search
    best_model = random_search.best_estimator_

    # ---------------------------------------------------------
    # 2. INTERNAL CROSS-VALIDATION ON TRAINING SET
    # ---------------------------------------------------------
    print("Performing internal and external cross-validation...")

    # Evaluate the best model using cross-validation on the training set
    cv_results = cross_validate(
        best_model,
        X_train,
        y_train,
        cv=cv,
        scoring=metrics_dict,
        return_train_score=True,
    )

    # ---------------------------------------------------------
    # 3. EXTERNAL VALIDATION ON TEST SET
    # ---------------------------------------------------------
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
        "Accuracy": accuracy_score(y_test, y_pred_test),
        "Precision": precision_score(y_test, y_pred_test, zero_division=0),
        "Recall": recall_score(y_test, y_pred_test, zero_division=0),
        "F1": f1_score(y_test, y_pred_test, zero_division=0),
        "Specificity": recall_score(y_test, y_pred_test, pos_label=0, zero_division=0),
    }

    if y_score_test is not None:
        metrics_test["ROC-AUC"] = roc_auc_score(y_test, y_score_test)
        fpr, tpr, thresholds = roc_curve(y_test, y_score_test)
        metrics_test["PR-AUC"] = average_precision_score(y_test, y_score_test)
        precisions, recalls, _ = precision_recall_curve(y_test, y_score_test)
    else:
        metrics_test["ROC-AUC"] = np.nan
        metrics_test["PR-AUC"] = np.nan
        precisions, recalls = None, None

    # ---------------------------------------------------------
    # 4. CONSTRUCTION OF THE FINAL RESULTS DATAFRAME
    # ---------------------------------------------------------
    cv_rows = []

    # Add train and validation metrics in long format to the list of rows
    for metric in metrics_dict.keys():
        train_scores = cv_results[f"train_{metric}"]
        val_scores = cv_results[f"test_{metric}"]

        for fold_idx, (t_score, v_score) in enumerate(zip(train_scores, val_scores)):
            cv_rows.append(
                {
                    "Metric": metric,
                    "Dataset": "Train",
                    "Score": t_score,
                    "Fold": fold_idx,
                }
            )
            cv_rows.append(
                {
                    "Metric": metric,
                    "Dataset": "Validation",
                    "Score": v_score,
                    "Fold": fold_idx,
                }
            )

    # Add test metrics as a single row
    for metric, test_score in metrics_test.items():
        cv_rows.append(
            {"Metric": metric, "Dataset": "Test", "Score": test_score, "Fold": 0}
        )

    # Build the final data frame
    df_results_complete = pd.DataFrame(cv_rows)

    return (best_model, df_results_complete, fpr, tpr, precisions, recalls)
