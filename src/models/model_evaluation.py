"""Evaluation of the fitted models on the external validation set."""

import numpy as np
import pandas as pd
from sklearn.metrics import (
    accuracy_score,
    average_precision_score,
    f1_score,
    precision_recall_curve,
    precision_score,
    recall_score,
    roc_auc_score,
    roc_curve,
)

from src.config import MODEL_DISPLAY_NAMES, MODEL_ORDER


def get_decision_scores(model, X) -> np.ndarray:
    """Return continuous scores for the positive class.

    Uses predict_proba when available and falls back to decision_function,
    which is what SVC exposes since it is fitted without probability
    calibration.

    Parameters
    ----------
    model : sklearn estimator
        Fitted classifier or pipeline.
    X : array-like or pd.DataFrame
        Samples to score.

    Returns
    -------
    np.ndarray
        One score per sample; higher means more likely to be positive.

    Raises
    ------
    AttributeError
        If the model exposes neither predict_proba nor decision_function.
    """
    if hasattr(model, "predict_proba"):
        return model.predict_proba(X)[:, 1]
    if hasattr(model, "decision_function"):
        return model.decision_function(X)
    raise AttributeError(
        f"{type(model).__name__} exposes neither predict_proba nor "
        "decision_function, so ranking metrics cannot be computed."
    )


def compute_hard_metrics(y_test, y_pred) -> dict:
    """Compute the threshold-dependent ('hard') classification metrics.

    Factored out of evaluate_on_test so that other callers (e.g. a
    threshold-sensitivity analysis of a single model) can score an arbitrary
    binary prediction vector without needing a fitted model or a continuous
    score.

    Parameters
    ----------
    y_test : array-like
        Binary external validation target (0/1).
    y_pred : array-like
        Binary predictions (0/1), already binarized at whatever threshold the
        caller chose.

    Returns
    -------
    dict
        Keys 'Accuracy', 'Precision', 'Recall', 'Specificity', 'F1'.
    """
    return {
        "Accuracy": accuracy_score(y_test, y_pred),
        "Precision": precision_score(y_test, y_pred, zero_division=0),
        "Recall": recall_score(y_test, y_pred, zero_division=0),
        "Specificity": recall_score(y_test, y_pred, pos_label=0, zero_division=0),
        "F1": f1_score(y_test, y_pred, zero_division=0),
    }


def evaluate_on_test(model, X_test, y_test):
    """Score a fitted model on the held-out external validation set.

    Parameters
    ----------
    model : sklearn estimator
        Fitted classifier or pipeline.
    X_test : pd.DataFrame
        External validation features.
    y_test : pd.Series or np.ndarray
        Binary external validation target (0/1).

    Returns
    -------
    metrics : pd.DataFrame
        Long-format frame with columns ['Metric', 'Dataset', 'Score', 'Fold'],
        where Dataset is 'Test' and Fold is NaN, matching the layout produced
        by the internal cross-validation.
    curves : dict
        Keys 'fpr', 'tpr', 'precision', 'recall', 'roc_auc' and 'pr_auc'.
    """
    y_pred = model.predict(X_test)
    y_score = get_decision_scores(model, X_test)

    roc_auc = roc_auc_score(y_test, y_score)
    pr_auc = average_precision_score(y_test, y_score)

    scores = {
        **compute_hard_metrics(y_test, y_pred),
        "ROC-AUC": roc_auc,
        "PR-AUC": pr_auc,
    }

    metrics = pd.DataFrame(
        {
            "Metric": list(scores),
            "Dataset": "Test",
            "Score": list(scores.values()),
            "Fold": np.nan,
        }
    )

    false_positive_rate, true_positive_rate, _ = roc_curve(y_test, y_score)
    precision, recall, _ = precision_recall_curve(y_test, y_score)

    curves = {
        "fpr": false_positive_rate,
        "tpr": true_positive_rate,
        "precision": precision,
        "recall": recall,
        "roc_auc": roc_auc,
        "pr_auc": pr_auc,
    }

    return metrics, curves


def build_auc_table(metrics_df: pd.DataFrame, metric: str) -> pd.DataFrame:
    """Extract the external validation scores of one metric, model by model.

    Parameters
    ----------
    metrics_df : pd.DataFrame
        Consolidated metrics with columns ['Model', 'Metric', 'Dataset',
        'Score'], as returned by save_metrics_results.
    metric : str
        Metric to extract, e.g. 'ROC-AUC' or 'PR-AUC'.

    Returns
    -------
    pd.DataFrame
        Columns ['Model', 'Metric', 'Score'], sorted by score (highest first).
    """
    selection = metrics_df[
        (metrics_df["Dataset"] == "Test") & (metrics_df["Metric"] == metric)
    ]
    table = selection[["Model", "Metric", "Score"]].copy()
    return table.sort_values(by="Score", ascending=False).reset_index(drop=True)


def load_curve_areas(metrics_df: pd.DataFrame, metric: str, model_order=None) -> dict:
    """Collect the external validation area under the curve of every model.

    Reads the areas back from the consolidated metrics table instead of the
    in-memory curves, so the curve figures can be redrawn from disk alone.

    Parameters
    ----------
    metrics_df : pd.DataFrame
        Consolidated metrics with columns ['Model', 'Metric', 'Dataset',
        'Score'], as saved in models_metrics.csv.
    metric : str
        Metric to extract, e.g. 'ROC-AUC' or 'PR-AUC'.
    model_order : list of str, optional
        Models to include, in legend order. Defaults to MODEL_ORDER, restricted
        to the models actually present in metrics_df.

    Returns
    -------
    dict
        Mapping of model abbreviation to the requested area.
    """
    model_order = MODEL_ORDER if model_order is None else model_order
    selection = metrics_df[
        (metrics_df["Dataset"] == "Test") & (metrics_df["Metric"] == metric)
    ]
    areas = dict(zip(selection["Model"], selection["Score"]))
    return {model: areas[model] for model in model_order if model in areas}


def build_internal_validation_table(
    internal_validation_by_model: dict, model_order=None
) -> pd.DataFrame:
    """Stack the per-model internal validation summaries into one ordered table.

    Parameters
    ----------
    internal_validation_by_model : dict
        Mapping of model abbreviation to its internal-validation DataFrame, as
        returned by summarize_internal_validation or load_internal_validation.
    model_order : list of str, optional
        Reporting order of the models. Defaults to MODEL_ORDER.

    Returns
    -------
    pd.DataFrame
        Concatenation of the summaries, sorted by the reporting order.
    """
    model_order = MODEL_ORDER if model_order is None else model_order

    table = pd.concat(internal_validation_by_model.values(), ignore_index=True)
    table["Model"] = pd.Categorical(
        table["Model"], categories=model_order, ordered=True
    )
    table = table.sort_values(by="Model")
    table["Model"] = table["Model"].astype(str)
    return table


def build_modality_comparison_table(
    baseline_metrics: pd.DataFrame,
    comparison_metrics: pd.DataFrame,
    baseline_label: str = "Clinical",
    comparison_label: str = "Multimodal",
    metrics=("ROC-AUC", "PR-AUC"),
    extra_arms: list[tuple[str, pd.DataFrame]] | None = None,
) -> pd.DataFrame:
    """Pair the external validation scores of two or more modelling arms.

    All arms must have been trained on the same cohort and partition for the
    comparison to be meaningful.

    Parameters
    ----------
    baseline_metrics : pd.DataFrame
        Consolidated metrics of the reference arm, with columns
        ['Model', 'Metric', 'Dataset', 'Score'].
    comparison_metrics : pd.DataFrame
        Consolidated metrics of the arm being compared against the baseline,
        same layout.
    baseline_label : str, default 'Clinical'
        Name of the reference arm, used as its 'Modality' value.
    comparison_label : str, default 'Multimodal'
        Name of the arm being compared, used as its 'Modality' value.
    metrics : tuple of str, default ('ROC-AUC', 'PR-AUC')
        Metrics to compare.
    extra_arms : list of (str, pd.DataFrame), optional
        Additional modelling arms to include between the baseline and the
        comparison arm (e.g. a pure proteomic arm alongside clinical and
        multimodal), each as a (label, metrics_df) pair with the same layout
        as baseline_metrics.

    Returns
    -------
    pd.DataFrame
        Long-format frame with columns ['Model', 'Metric', 'Modality', 'Score'],
        ordered by MODEL_ORDER and then by the requested metric order.
    """
    extra_arms = extra_arms or []
    arms = [
        (baseline_label, baseline_metrics),
        *extra_arms,
        (comparison_label, comparison_metrics),
    ]

    frames = []
    for modality, metrics_df in arms:
        selection = metrics_df[
            (metrics_df["Dataset"] == "Test") & (metrics_df["Metric"].isin(metrics))
        ]
        frame = selection[["Model", "Metric", "Score"]].copy()
        frame["Modality"] = modality
        frames.append(frame)

    comparison = pd.concat(frames, ignore_index=True)
    modality_order = [label for label, _ in arms]
    comparison["Model"] = pd.Categorical(
        comparison["Model"], categories=MODEL_ORDER, ordered=True
    )
    comparison["Metric"] = pd.Categorical(
        comparison["Metric"], categories=list(metrics), ordered=True
    )
    comparison["Modality"] = pd.Categorical(
        comparison["Modality"],
        categories=modality_order,
        ordered=True,
    )
    comparison = comparison.sort_values(by=["Model", "Metric", "Modality"])
    comparison["Model"] = comparison["Model"].astype(str)
    comparison["Metric"] = comparison["Metric"].astype(str)
    comparison["Modality"] = comparison["Modality"].astype(str)

    return comparison[["Model", "Metric", "Modality", "Score"]].reset_index(drop=True)


def build_modality_delta_table(
    comparison: pd.DataFrame,
    baseline_label: str = "Clinical",
    comparison_label: str = "Multimodal",
) -> pd.DataFrame:
    """Turn the long comparison frame into one row per model and metric.

    Parameters
    ----------
    comparison : pd.DataFrame
        Long-format comparison as returned by build_modality_comparison_table.
    baseline_label : str, default 'Clinical'
        Name of the reference arm, as used in the 'Modality' column.
    comparison_label : str, default 'Multimodal'
        Name of the arm being compared, as used in the 'Modality' column.

    Returns
    -------
    pd.DataFrame
        Columns ['Model', 'Metric', baseline_label, comparison_label, 'Delta'],
        where Delta is the comparison score minus the baseline one.
    """
    wide = comparison.pivot_table(
        index=["Model", "Metric"], columns="Modality", values="Score", observed=True
    ).reset_index()
    wide.columns.name = None

    for modality in (baseline_label, comparison_label):
        if modality not in wide.columns:
            wide[modality] = np.nan

    wide["Delta"] = wide[comparison_label] - wide[baseline_label]
    wide["Model"] = pd.Categorical(wide["Model"], categories=MODEL_ORDER, ordered=True)
    wide = wide.sort_values(by=["Model", "Metric"])
    wide["Model"] = wide["Model"].astype(str)

    return wide[
        ["Model", "Metric", baseline_label, comparison_label, "Delta"]
    ].reset_index(drop=True)


def build_performance_table(
    models_metrics: pd.DataFrame,
    metrics: list,
    decimals: int = 3,
    use_display_names: bool = True,
) -> pd.DataFrame:
    """Build a manuscript-ready, multi-metric performance table for one phase.

    Reads the long-format scores already produced by the modelling phase (as
    saved in ``models_metrics.csv``: five cross-validation folds for Train and
    Validation, one held-out score for Test) and pivots them into a single
    wide table, one row per model, with a two-level column MultiIndex: metric
    (e.g. 'ROC-AUC') on top and partition ('Train', 'Validation', 'Test')
    below. Train and Validation are reported as "mean ± SD" across folds;
    Test — the external validation score — has no SD since it is a single
    evaluation on the held-out set.

    Parameters
    ----------
    models_metrics : pd.DataFrame
        Long-format table with columns ['Model', 'Metric', 'Dataset',
        'Score'], as produced by the modelling phase for every model.
    metrics : list of str
        Metrics to report, e.g. ['ROC-AUC', 'PR-AUC'].
    decimals : int, default 3
        Number of decimals used to format the scores.
    use_display_names : bool, default True
        If True, the row index uses the full model names
        (MODEL_DISPLAY_NAMES) instead of the abbreviations.

    Returns
    -------
    pd.DataFrame
        Row index 'Model' (ordered by MODEL_ORDER); column MultiIndex
        ['Metric', 'Partition'] with partitions ('Train', 'Validation',
        'Test'). Each cell is a formatted string; models or metrics entirely
        absent from `models_metrics` are dropped.

    Raises
    ------
    ValueError
        If no requested metric is found in `models_metrics`.
    """
    partitions = ("Train", "Validation", "Test")
    columns = pd.MultiIndex.from_product(
        [metrics, partitions], names=["Metric", "Partition"]
    )
    table = pd.DataFrame(
        index=pd.Index(MODEL_ORDER, name="Model"), columns=columns, dtype=object
    )

    found_any = False
    for metric in metrics:
        metric_scores = models_metrics[models_metrics["Metric"] == metric]
        for partition in partitions:
            partition_scores = metric_scores[metric_scores["Dataset"] == partition]
            summary = partition_scores.groupby("Model")["Score"].agg(["mean", "std"])
            for model_name in MODEL_ORDER:
                if model_name not in summary.index:
                    continue
                found_any = True
                mean = summary.loc[model_name, "mean"]
                if partition == "Test":
                    formatted = f"{mean:.{decimals}f}"
                else:
                    std = summary.loc[model_name, "std"]
                    formatted = f"{mean:.{decimals}f} ± {std:.{decimals}f}"
                table.loc[model_name, (metric, partition)] = formatted

    if not found_any:
        raise ValueError(f"None of {metrics} were found in models_metrics.")

    table = table.dropna(how="all").dropna(axis="columns", how="all")

    if use_display_names:
        table.index = table.index.map(
            lambda abbreviation: MODEL_DISPLAY_NAMES.get(abbreviation, abbreviation)
        )
        table.index.name = "Model"

    return table


def build_comparison_table(
    delta_table: pd.DataFrame,
    metrics: list,
    baseline_label: str,
    comparison_label: str,
    decimals: int = 3,
    use_display_names: bool = True,
) -> pd.DataFrame:
    """Reshape a modality/filtering delta table into a multi-metric table.

    Takes the long ['Model', 'Metric', baseline_label, comparison_label,
    'Delta'] table produced by build_modality_delta_table and formats it into
    one row per model with a two-level column MultiIndex: metric on top and
    [baseline_label, comparison_label, 'Delta'] below, so the incremental
    value of each arm is readable at a glance across every metric at once.

    Parameters
    ----------
    delta_table : pd.DataFrame
        Output of build_modality_delta_table, with columns
        ['Model', 'Metric', baseline_label, comparison_label, 'Delta'].
    metrics : list of str
        Metrics to report, e.g. ['ROC-AUC', 'PR-AUC'].
    baseline_label : str
        Name of the reference arm, as used in delta_table's columns.
    comparison_label : str
        Name of the arm being compared, as used in delta_table's columns.
    decimals : int, default 3
        Number of decimals used to format the scores and the delta.
    use_display_names : bool, default True
        If True, the row index uses the full model names
        (MODEL_DISPLAY_NAMES) instead of the abbreviations.

    Returns
    -------
    pd.DataFrame
        Row index 'Model' (ordered by MODEL_ORDER); column MultiIndex
        ['Metric', ''] with sub-columns [baseline_label, comparison_label,
        'Delta']. Models entirely absent from `delta_table` are dropped.
    """
    sub_columns = (baseline_label, comparison_label, "Delta")
    columns = pd.MultiIndex.from_product([metrics, sub_columns], names=["Metric", ""])
    table = pd.DataFrame(
        index=pd.Index(MODEL_ORDER, name="Model"), columns=columns, dtype=object
    )

    for metric in metrics:
        metric_table = delta_table[delta_table["Metric"] == metric]
        for model_name in MODEL_ORDER:
            model_row = metric_table[metric_table["Model"] == model_name]
            if model_row.empty:
                continue
            baseline_score = model_row[baseline_label].iloc[0]
            comparison_score = model_row[comparison_label].iloc[0]
            delta = model_row["Delta"].iloc[0]
            sign = "+" if delta >= 0 else ""
            table.loc[model_name, (metric, baseline_label)] = f"{baseline_score:.{decimals}f}"
            table.loc[model_name, (metric, comparison_label)] = (
                f"{comparison_score:.{decimals}f}"
            )
            table.loc[model_name, (metric, "Delta")] = f"{sign}{delta:.{decimals}f}"

    table = table.dropna(how="all")

    if use_display_names:
        table.index = table.index.map(
            lambda abbreviation: MODEL_DISPLAY_NAMES.get(abbreviation, abbreviation)
        )
        table.index.name = "Model"

    return table


def _clean_parameter_name(parameter: str) -> str:
    """Strip every pipeline/meta-estimator prefix from a parameter name.

    Parameter names come out of RandomizedSearchCV as scikit-learn paths such
    as 'clf__C' or, for meta-estimators like AdaBoost, 'clf__estimator__max_depth'.
    Only the last path segment is kept, since that is the actual hyperparameter
    name a reader recognizes.

    Parameters
    ----------
    parameter : str
        Raw parameter name, possibly with '__'-separated prefixes.

    Returns
    -------
    str
        The last path segment, e.g. 'C' or 'max_depth'.
    """
    return parameter.split("__")[-1]


def _collapse_best_params(best_params: pd.DataFrame) -> pd.Series:
    """Reduce one phase's long best_params table to a Model/Parameter Series.

    Parameters
    ----------
    best_params : pd.DataFrame
        Long-format table with columns ['Model', 'Parameter', 'Value'], as
        saved in best_params.csv by a modelling phase. Parameter names may
        carry scikit-learn pipeline prefixes (e.g. 'clf__C',
        'clf__estimator__max_depth').

    Returns
    -------
    pd.Series
        Value, indexed by a ['Model', 'Parameter'] MultiIndex, with every
        pipeline prefix stripped from the parameter names (e.g.
        'clf__estimator__max_depth' -> 'max_depth'). If two raw parameter
        names collapse to the same cleaned name for the same model (e.g. a
        naming collision across estimators), their values are joined with
        '; ' rather than silently dropped.
    """
    table = best_params.copy()
    table["Parameter"] = table["Parameter"].map(_clean_parameter_name)

    table["Value"] = table["Value"].astype(str)
    return table.groupby(["Model", "Parameter"], sort=False)["Value"].agg(
        lambda values: "; ".join(dict.fromkeys(values))
    )


def build_hyperparameters_table(
    phase_best_params: dict, use_display_names: bool = True
) -> pd.DataFrame:
    """Merge every phase's tuned hyperparameters into a single wide table.

    Instead of one table per modelling phase differing only in the value
    column, this aligns every phase's best_params on the same
    ['Model', 'Parameter'] rows and lays them out side by side, one column
    per phase, so the tuned value of a given hyperparameter can be compared
    across phases at a glance.

    Parameters
    ----------
    phase_best_params : dict of {str: pd.DataFrame}
        Mapping of phase label (e.g. 'Clinical', 'Multimodal') to that
        phase's long-format best_params table, in the order the phase
        columns should appear.
    use_display_names : bool, default True
        If True, the 'Model' index level uses the full model names
        (MODEL_DISPLAY_NAMES) instead of the abbreviations.

    Returns
    -------
    pd.DataFrame
        Indexed by a ['Model', 'Parameter'] MultiIndex (models in
        MODEL_ORDER, parameters sorted alphabetically within each model,
        unioned across every phase); column MultiIndex ['Value', phase
        label] with one sub-column per entry in `phase_best_params`. A
        hyperparameter not tuned in a given phase (e.g. a different search
        space) is left blank for that phase's column rather than dropping
        the row.
    """
    per_phase = {
        phase_label: _collapse_best_params(best_params)
        for phase_label, best_params in phase_best_params.items()
    }
    table = pd.DataFrame(per_phase)
    table.index = table.index.set_names(["Model", "Parameter"])
    table = table.reset_index()

    table["Model"] = pd.Categorical(table["Model"], categories=MODEL_ORDER, ordered=True)
    table = table.sort_values(by=["Model", "Parameter"])
    table["Model"] = table["Model"].astype(str)

    if use_display_names:
        table["Model"] = table["Model"].map(
            lambda abbreviation: MODEL_DISPLAY_NAMES.get(abbreviation, abbreviation)
        )

    table = table.set_index(["Model", "Parameter"])
    table.columns = pd.MultiIndex.from_product(
        [["Value"], table.columns], names=["", "Phase"]
    )
    return table
