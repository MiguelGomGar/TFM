"""Hyperparameter optimization, internal validation and phase orchestration."""

from pathlib import Path

import pandas as pd
from sklearn.model_selection import RandomizedSearchCV, cross_validate

from src.config import (
    FEATURE_SELECTION_MODEL,
    MODEL_ORDER,
    N_JOBS,
    OBJECTIVE_METRIC,
    RISK_SCORE_BASELINE,
    SCORING_METRICS,
    SEED,
)
from src.models.feature_selection import (
    apply_feature_filter,
    select_features_with_elastic_net,
)
from src.models.model_evaluation import (
    build_auc_table,
    evaluate_on_test,
    summarize_curve_areas,
)
from src.models.model_zoo import build_model_pipeline, get_display_name
from src.utils.io import save_csv, save_figure
from src.models.results_saving import (
    save_curves_results,
    save_metrics_results,
    save_model,
)
from src.visualization.model_evaluation import (
    plot_internal_validation,
    plot_metric_by_model,
    plot_model_pr_curves,
    plot_model_roc_curves,
)


def optimize_model(
    pipeline,
    param_distributions,
    X_train,
    y_train,
    cv,
    aim: str,
    n_iter: int,
    seed: int,
    n_jobs: int = N_JOBS,
):
    """Tune a pipeline with a randomized search over its hyperparameters.

    Parameters
    ----------
    pipeline : sklearn.pipeline.Pipeline
        Unfitted pipeline with 'preprocessor' and 'clf' steps.
    param_distributions : dict
        Search space keyed by 'clf__*' parameter names.
    X_train : pd.DataFrame
        Training features.
    y_train : pd.Series
        Binary training target.
    cv : sklearn cross-validation splitter
        Splitter used inside the search.
    aim : str
        Scikit-learn scorer name maximized by the search, e.g.
        'average_precision'.
    n_iter : int
        Number of candidate hyperparameter settings sampled.
    seed : int
        Random seed for the sampler.
    n_jobs : int, default N_JOBS
        Number of parallel workers.

    Returns
    -------
    best_model : sklearn.pipeline.Pipeline
        Pipeline refitted on the whole training set with the best settings.
    best_params : dict
        Hyperparameters selected by the search.
    """
    search = RandomizedSearchCV(
        estimator=pipeline,
        param_distributions=param_distributions,
        n_iter=n_iter,
        scoring=aim,
        cv=cv,
        random_state=seed,
        n_jobs=n_jobs,
        refit=True,
    )
    search.fit(X_train, y_train)

    return search.best_estimator_, search.best_params_


def cross_validate_model(model, X_train, y_train, cv, scoring=None) -> pd.DataFrame:
    """Run the internal cross-validation used to assess overfitting.

    Both the training folds and the held-out validation fold are scored, so
    that the gap between them can be inspected.

    Parameters
    ----------
    model : sklearn.pipeline.Pipeline
        Fitted or unfitted pipeline; it is cloned and refitted on each fold.
    X_train : pd.DataFrame
        Training features.
    y_train : pd.Series
        Binary training target.
    cv : sklearn cross-validation splitter
        Splitter defining the folds.
    scoring : dict, optional
        Mapping of metric label to scikit-learn scorer name. Defaults to
        SCORING_METRICS.

    Returns
    -------
    pd.DataFrame
        Long-format frame with columns ['Metric', 'Dataset', 'Score', 'Fold'],
        where Dataset is 'Train' or 'Validation'.
    """
    scoring = SCORING_METRICS if scoring is None else scoring

    results = cross_validate(
        model,
        X_train,
        y_train,
        cv=cv,
        scoring=scoring,
        return_train_score=True,
        n_jobs=N_JOBS,
    )

    records = []
    for metric_label in scoring:
        for dataset, key in (
            ("Train", f"train_{metric_label}"),
            ("Validation", f"test_{metric_label}"),
        ):
            for fold_index, score in enumerate(results[key], start=1):
                records.append(
                    {
                        "Metric": metric_label,
                        "Dataset": dataset,
                        "Score": score,
                        "Fold": fold_index,
                    }
                )

    return pd.DataFrame(records)


def summarize_internal_validation(
    cv_results: pd.DataFrame, model_name: str
) -> pd.DataFrame:
    """Aggregate the per-fold cross-validation scores into mean and std.

    Parameters
    ----------
    cv_results : pd.DataFrame
        Long-format frame as returned by cross_validate_model.
    model_name : str
        Model abbreviation, added as a column.

    Returns
    -------
    pd.DataFrame
        Columns ['Model', 'Metric', 'Dataset', 'Mean', 'Std', 'N_Folds'].
    """
    summary = (
        cv_results.groupby(["Metric", "Dataset"], sort=False)["Score"]
        .agg(Mean="mean", Std="std", N_Folds="count")
        .reset_index()
    )
    summary.insert(0, "Model", model_name)
    return summary


def _build_best_params_table(best_params_by_model: dict) -> pd.DataFrame:
    """Flatten the selected hyperparameters of every model into a table.

    Parameters
    ----------
    best_params_by_model : dict
        Mapping of model abbreviation to its best_params dictionary.

    Returns
    -------
    pd.DataFrame
        Columns ['Model', 'Parameter', 'Value'].
    """
    records = [
        {"Model": model, "Parameter": parameter, "Value": str(value)}
        for model, params in best_params_by_model.items()
        for parameter, value in sorted(params.items())
    ]
    return pd.DataFrame(records, columns=["Model", "Parameter", "Value"])


def run_modelling_phase(
    X_train,
    X_test,
    y_train,
    y_test,
    search_spaces: dict,
    output_dir,
    n_iter: int,
    cv,
    phase_title: str = "",
    apply_filter: bool = False,
    seed: int = SEED,
    model_order=None,
    scoring=None,
    objective_metric: str = OBJECTIVE_METRIC,
    baseline=None,
    logger=None,
) -> pd.DataFrame:
    """Train, tune, evaluate and report every model of one modelling phase.

    The Elastic Net is always fitted first on the full feature set. When
    ``apply_filter`` is True, the predictors whose coefficients it shrank to
    zero are dropped before the remaining models are trained, which is the
    feature selection strategy described in the methodology.

    Parameters
    ----------
    X_train, X_test : pd.DataFrame
        Training and external validation features.
    y_train, y_test : pd.Series
        Binary training and external validation targets.
    search_spaces : dict
        Hyperparameter search spaces keyed by model abbreviation.
    output_dir : str or Path
        Directory where every figure, table and fitted model is written.
    n_iter : int
        Randomized search budget for this phase.
    cv : sklearn cross-validation splitter
        Splitter used both by the search and by the internal validation.
    phase_title : str, default ''
        Suffix appended to the plot titles, e.g. 'clinical data'.
    apply_filter : bool, default False
        Whether to restrict the non-Elastic-Net models to the selected features.
    seed : int, default SEED
        Random seed propagated to the preprocessors and estimators.
    model_order : list of str, optional
        Models to fit, in reporting order. Defaults to MODEL_ORDER.
    scoring : dict, optional
        Metric label to scorer name mapping. Defaults to SCORING_METRICS.
    objective_metric : str, default OBJECTIVE_METRIC
        Key of ``scoring`` maximized by the randomized search.
    baseline : dict, optional
        Metric label to reference value for the bar plots. Defaults to
        RISK_SCORE_BASELINE.
    logger : logging.Logger, optional
        Logger used to report progress.

    Returns
    -------
    pd.DataFrame
        Consolidated metrics with columns ['Model', 'Metric', 'Dataset',
        'Score'], covering the training folds, the validation folds and the
        external validation set.
    """
    model_order = MODEL_ORDER if model_order is None else model_order
    scoring = SCORING_METRICS if scoring is None else scoring
    baseline = RISK_SCORE_BASELINE if baseline is None else baseline
    aim = scoring[objective_metric]

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    metrics_by_model = {}
    curves_by_model = {}
    best_params_by_model = {}

    # The feature selection model is fitted first on every available predictor:
    # its regularization defines the filter applied to the remaining models.
    selection_model = FEATURE_SELECTION_MODEL
    if selection_model not in model_order:
        raise ValueError(
            f"The feature selection model {selection_model!r} must be part of "
            f"the models to fit, got {model_order}."
        )
    ordered_models = [selection_model] + [
        model for model in model_order if model != selection_model
    ]

    X_train_models, X_test_models = X_train, X_test

    for abbreviation in ordered_models:
        display_name = get_display_name(abbreviation)
        if logger is not None:
            logger.info(f"Optimizing model: {display_name}")

        if abbreviation == selection_model:
            X_train_current, X_test_current = X_train, X_test
        else:
            X_train_current, X_test_current = X_train_models, X_test_models

        pipeline = build_model_pipeline(X_train_current, abbreviation, seed=seed)
        best_model, best_params = optimize_model(
            pipeline=pipeline,
            param_distributions=search_spaces[abbreviation],
            X_train=X_train_current,
            y_train=y_train,
            cv=cv,
            aim=aim,
            n_iter=n_iter,
            seed=seed,
        )
        best_params_by_model[abbreviation] = best_params
        save_model(
            fitted_pipeline=best_model, output_dir=output_dir, identifier=abbreviation
        )

        # Internal validation on the training set and external validation on the
        # held-out test set.
        cv_results = cross_validate_model(
            best_model, X_train_current, y_train, cv=cv, scoring=scoring
        )
        test_metrics, curves = evaluate_on_test(best_model, X_test_current, y_test)

        metrics_by_model[abbreviation] = pd.concat(
            [cv_results, test_metrics], ignore_index=True
        )
        curves_by_model[abbreviation] = curves

        summary = summarize_internal_validation(cv_results, abbreviation)
        save_csv(
            summary, output_dir / f"internal_validation_{abbreviation.lower()}.csv"
        )
        figure = plot_internal_validation(summary, model_name=display_name)
        save_figure(
            figure, output_dir / f"internal_validation_{abbreviation.lower()}.png"
        )

        # Derive the filter right after the selection model has been tuned.
        if abbreviation == selection_model:
            _, irrelevant_features, _ = select_features_with_elastic_net(
                best_model,
                output_dir=output_dir,
                identifier=abbreviation,
                logger=logger,
            )
            if apply_filter:
                X_train_models = apply_feature_filter(
                    X_train, irrelevant_features, logger=logger
                )
                X_test_models = apply_feature_filter(
                    X_test, irrelevant_features, logger=logger
                )
                if logger is not None:
                    logger.info(
                        "Remaining models will be trained on "
                        f"{X_train_models.shape[1]} feature(s)."
                    )

    if logger is not None:
        logger.info("Saving metrics, curves and plots...")

    # Reorder to the reporting order before consolidating the results.
    metrics_by_model = {
        abbreviation: metrics_by_model[abbreviation] for abbreviation in model_order
    }
    metrics_df = save_metrics_results(
        models_dict=metrics_by_model, output_dir=output_dir
    )
    save_csv(
        _build_best_params_table(best_params_by_model), output_dir / "best_params.csv"
    )

    roc_curves = save_curves_results(
        model_order,
        [curves_by_model[model]["fpr"] for model in model_order],
        [curves_by_model[model]["tpr"] for model in model_order],
        curve_type="roc",
        output_dir=output_dir,
    )
    pr_curves = save_curves_results(
        model_order,
        [curves_by_model[model]["recall"] for model in model_order],
        [curves_by_model[model]["precision"] for model in model_order],
        curve_type="pr",
        output_dir=output_dir,
    )

    title_suffix = f" ({phase_title})" if phase_title else ""

    figure = plot_model_roc_curves(
        roc_curves,
        summarize_curve_areas(curves_by_model, "roc_auc"),
        title=f"ROC curves{title_suffix}",
    )
    save_figure(figure, output_dir / "curves_roc.png")

    figure = plot_model_pr_curves(
        pr_curves,
        summarize_curve_areas(curves_by_model, "pr_auc"),
        prevalence=float(y_test.mean()),
        title=f"Precision-recall curves{title_suffix}",
    )
    save_figure(figure, output_dir / "curves_pr.png")

    for metric, filename in (("ROC-AUC", "auc_roc"), ("PR-AUC", "auc_pr")):
        auc_table = build_auc_table(metrics_df, metric)
        save_csv(auc_table, output_dir / f"{filename}_by_model.csv")
        figure = plot_metric_by_model(
            auc_table,
            metric=metric,
            baseline=baseline.get(metric),
            title=f"{metric} by model{title_suffix}",
        )
        save_figure(figure, output_dir / f"{filename}_by_model.png")

    return metrics_df
