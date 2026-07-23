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


def plot_internal_validation(
    df_results_complete, metrics_list, figsize=(10, 6), title="Overfitting analysis"
):
    """
    Plots a grouped bar chart comparing Train and Validation scores for the
    selected metrics from the output of optimize_model_random_search.

    Parameters:
    ----------
    - df_results_complete : pd.DataFrame
        Long-format dataframe with at least the columns ['Metric', 'Dataset',
        'Score'].
    - metrics_list : list of str
        Metrics to display on the x-axis.
    - figsize : tuple, default=(10, 6)
        Figure size used for the plot.
    - title : str, default="Internal validation metrics"
        Plot title.

    Returns:
    -------
    - fig : matplotlib.figure.Figure
        Created figure.
    - ax : matplotlib.axes.Axes
        Axes containing the plot.
    - df_summary : pd.DataFrame
        Aggregated dataframe with mean and standard deviation per metric and
        dataset.
    """

    required_columns = {"Metric", "Dataset", "Score"}
    missing_columns = required_columns.difference(df_results_complete.columns)
    if missing_columns:
        raise ValueError(
            "df_results_complete must contain the columns: "
            f"{sorted(required_columns)}"
        )

    if not metrics_list:
        raise ValueError("metrics_list must contain at least one metric name")

    selected_metrics = list(dict.fromkeys(metrics_list))
    df_plot = df_results_complete[
        df_results_complete["Dataset"].isin(["Train", "Validation"])
        & df_results_complete["Metric"].isin(selected_metrics)
    ].copy()

    if df_plot.empty:
        raise ValueError("No Train/Validation rows were found for the selected metrics")

    df_summary = df_plot.groupby(["Metric", "Dataset"], as_index=False)["Score"].agg(
        mean="mean", std="std"
    )
    df_summary["std"] = df_summary["std"].fillna(0.0)

    mean_pivot = df_summary.pivot(
        index="Metric", columns="Dataset", values="mean"
    ).reindex(selected_metrics)
    std_pivot = df_summary.pivot(
        index="Metric", columns="Dataset", values="std"
    ).reindex(selected_metrics)

    train_means = mean_pivot.get(
        "Train", pd.Series(index=selected_metrics, dtype=float)
    ).reindex(selected_metrics)
    validation_means = mean_pivot.get(
        "Validation", pd.Series(index=selected_metrics, dtype=float)
    ).reindex(selected_metrics)
    train_stds = (
        std_pivot.get("Train", pd.Series(index=selected_metrics, dtype=float))
        .fillna(0.0)
        .reindex(selected_metrics)
    )
    validation_stds = (
        std_pivot.get("Validation", pd.Series(index=selected_metrics, dtype=float))
        .fillna(0.0)
        .reindex(selected_metrics)
    )

    x_positions = np.arange(len(selected_metrics))
    bar_width = 0.35

    fig, ax = plt.subplots(figsize=figsize)
    ax.bar(
        x_positions - bar_width / 2,
        train_means.to_numpy(),
        bar_width,
        yerr=train_stds.to_numpy(),
        capsize=4,
        label="Train",
        color="#4C78A8",
        alpha=0.9,
    )
    ax.bar(
        x_positions + bar_width / 2,
        validation_means.to_numpy(),
        bar_width,
        yerr=validation_stds.to_numpy(),
        capsize=4,
        label="Validation",
        color="#F58518",
        alpha=0.9,
    )

    ax.set_xticks(x_positions)
    ax.set_xticklabels(selected_metrics)
    ax.set_ylabel("Score")
    ax.set_title(title)
    ax.legend()
    ax.grid(axis="y", alpha=0.3)
    ax.set_ylim(0, 1)
    fig.tight_layout()

    return fig, ax, df_summary


def plot_external_validation(
    df_results_complete, baseline, metric, figsize=(10, 6), title=None
):
    """
    Plots a vertical bar chart for the selected external evaluation metric
    across trained models.

    Parameters:
    ----------
    - df_results_complete : pd.DataFrame
        Long-format dataframe with at least the columns ['Model', 'Metric',
        'Dataset', 'Score'].
    - baseline : float
        Reference value drawn as a horizontal dashed line.
    - metric : str
        Metric to display on the y-axis.
    - figsize : tuple, default=(10, 6)
        Figure size used for the plot.
    - title : str, optional
        Plot title. If None, a default title is generated from metric.

    Returns:
    -------
    - fig : matplotlib.figure.Figure
        Created figure.
    - ax : matplotlib.axes.Axes
        Axes containing the plot.
    - df_plot : pd.DataFrame
        Aggregated dataframe used for plotting.
    """

    required_columns = {"Model", "Metric", "Dataset", "Score"}
    missing_columns = required_columns.difference(df_results_complete.columns)
    if missing_columns:
        raise ValueError(
            "df_results_complete must contain the columns: "
            f"{sorted(required_columns)}"
        )

    if metric is None or str(metric).strip() == "":
        raise ValueError("metric must be a non-empty string")

    df_plot = df_results_complete[
        df_results_complete["Dataset"].eq("Test")
        & df_results_complete["Metric"].eq(metric)
    ].copy()

    if df_plot.empty:
        raise ValueError(f"No Test rows were found for metric '{metric}'")

    df_plot = (
        df_plot.groupby("Model", as_index=False)["Score"]
        .mean()
        .sort_values("Score", ascending=False)
    )

    plot_title = title if title is not None else "External Validation"

    fig, ax = plt.subplots(figsize=figsize)
    y_positions = np.arange(len(df_plot))
    ax.barh(
        y_positions,
        df_plot["Score"],
        color="#4C78A8",
        alpha=0.9,
    )
    ax.axvline(
        baseline,
        linestyle="--",
        color="#D62728",
        linewidth=2,
        label=f"Baseline = {baseline:.3f}",
    )

    ax.set_yticks(y_positions)
    ax.set_yticklabels(df_plot["Model"])
    ax.invert_yaxis()
    ax.set_xlabel(metric)
    ax.set_title(plot_title)
    ax.legend()
    ax.grid(axis="x", alpha=0.3)
    ax.set_xlim(0, 1)
    fig.tight_layout()

    return fig, ax, df_plot


def plot_roc_curves(df_curve, metric=None, figsize=(8, 6), title=None):
    """
    Plots ROC curves for all models contained in a long-format curve dataframe.

    Parameters:
    ----------
    - df_curve : pd.DataFrame
        DataFrame with at least the columns ['False Positive Rate', 'True
        Positive Rate', 'Model'].
    - metric : str, optional
        Label used in the plot title and legend context.
    - figsize : tuple, default=(8, 6)
        Figure size used for the plot.
    - title : str, optional
        Plot title. If None, a default title is generated.

    Returns:
    -------
    - fig : matplotlib.figure.Figure
        Created figure.
    - ax : matplotlib.axes.Axes
        Axes containing the plot.
    """

    required_columns = {"False Positive Rate", "True Positive Rate", "Model"}
    missing_columns = required_columns.difference(df_curve.columns)
    if missing_columns:
        raise ValueError(
            "df_curve must contain the columns: " f"{sorted(required_columns)}"
        )

    if metric is not None and str(metric).strip() == "":
        raise ValueError("metric must be a non-empty string when provided")

    plot_title = (
        title
        if title is not None
        else (f"ROC curves" if metric is None else f"ROC curves for {metric}")
    )

    fig, ax = plt.subplots(figsize=figsize)

    for model_name, df_model in df_curve.groupby("Model", sort=False):
        df_model = df_model.sort_values("False Positive Rate").copy()
        auc_value = auc(df_model["False Positive Rate"], df_model["True Positive Rate"])
        ax.plot(
            df_model["False Positive Rate"],
            df_model["True Positive Rate"],
            linewidth=2,
            label=f"{model_name} (AUC = {auc_value:.3f})",
        )

    ax.plot([0, 1], [0, 1], linestyle="--", color="grey", linewidth=1.5, label="Random")
    ax.set_xlabel("False Positive Rate")
    ax.set_ylabel("True Positive Rate")
    ax.set_title(plot_title)
    ax.legend(loc="lower right")
    ax.grid(alpha=0.3)
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    fig.tight_layout()

    return fig, ax


def plot_pr_curves(df_curve, metric=None, baseline=None, figsize=(8, 6), title=None):
    """
    Plots Precision-Recall curves for all models contained in a long-format
    curve dataframe.

    Parameters:
    ----------
    - df_curve : pd.DataFrame
        DataFrame with at least the columns ['Recall', 'Precision', 'Model'].
    - metric : str, optional
        Label used in the plot title and legend context.
    - baseline : float, optional
        Reference prevalence line drawn across the plot. If None, no baseline
        is drawn.
    - figsize : tuple, default=(8, 6)
        Figure size used for the plot.
    - title : str, optional
        Plot title. If None, a default title is generated.

    Returns:
    -------
    - fig : matplotlib.figure.Figure
        Created figure.
    - ax : matplotlib.axes.Axes
        Axes containing the plot.
    """

    required_columns = {"Recall", "Precision", "Model"}
    missing_columns = required_columns.difference(df_curve.columns)
    if missing_columns:
        raise ValueError(
            "df_curve must contain the columns: " f"{sorted(required_columns)}"
        )

    if metric is not None and str(metric).strip() == "":
        raise ValueError("metric must be a non-empty string when provided")

    plot_title = (
        title
        if title is not None
        else (
            f"Precision-Recall curves"
            if metric is None
            else f"Precision-Recall curves for {metric}"
        )
    )

    fig, ax = plt.subplots(figsize=figsize)

    for model_name, df_model in df_curve.groupby("Model", sort=False):
        df_model = df_model.sort_values("Recall").copy()
        auc_value = auc(df_model["Recall"], df_model["Precision"])
        ax.plot(
            df_model["Recall"],
            df_model["Precision"],
            linewidth=2,
            label=f"{model_name} (AUC = {auc_value:.3f})",
        )

    if baseline is not None:
        ax.axhline(
            baseline,
            linestyle="--",
            color="grey",
            linewidth=1.5,
            label=f"Baseline = {baseline:.3f}",
        )

    ax.set_xlabel("Recall")
    ax.set_ylabel("Precision")
    ax.set_title(plot_title)
    ax.legend(loc="lower left")
    ax.grid(alpha=0.3)
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    fig.tight_layout()

    return fig, ax
