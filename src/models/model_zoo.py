"""Estimator catalogue and pipeline factory for the modelling phases."""

import pandas as pd
from sklearn.base import BaseEstimator
from sklearn.ensemble import (
    AdaBoostClassifier,
    ExtraTreesClassifier,
    GradientBoostingClassifier,
    RandomForestClassifier,
)
from sklearn.linear_model import LogisticRegression
from sklearn.neural_network import MLPClassifier
from sklearn.pipeline import Pipeline
from sklearn.svm import SVC
from sklearn.tree import DecisionTreeClassifier

from src.config import MODEL_DISPLAY_NAMES, SCALED_MODELS
from src.models.data_preprocessing import (
    get_full_preprocessor,
    get_trees_preprocessor,
)


def get_estimator(abbreviation: str, seed: int) -> BaseEstimator:
    """Build an unfitted classifier from its project abbreviation.

    Only the parameters that are held fixed across the randomized search are set
    here; everything else is sampled from the search spaces in ``src.config``.

    Parameters
    ----------
    abbreviation : str
        Model abbreviation. One of 'EN', 'SVM', 'DT', 'RF', 'ET', 'AB', 'GB'
        or 'MLP'.
    seed : int
        Random seed passed to every estimator that accepts a random_state.

    Returns
    -------
    sklearn.base.BaseEstimator
        Unfitted classifier instance.

    Raises
    ------
    ValueError
        If the abbreviation is not one of the supported models.
    """
    if abbreviation == "EN":
        # 'saga' is the only solver supporting an elastic-net penalty and must
        # therefore never be sampled by the search. The penalty type itself is
        # controlled by l1_ratio: since scikit-learn 1.8 the `penalty` argument
        # is deprecated (removed in 1.10) and l1_ratio alone selects L1, L2 or
        # elastic net, so it is deliberately not passed here.
        return LogisticRegression(solver="saga", max_iter=10000, random_state=seed)

    if abbreviation == "SVM":
        # No probability=True: probability calibration would refit the model
        # internally and slow the search down. Ranking metrics (ROC-AUC and
        # PR-AUC) are computed from decision_function instead.
        return SVC(random_state=seed)

    if abbreviation == "DT":
        return DecisionTreeClassifier(random_state=seed)

    if abbreviation == "RF":
        return RandomForestClassifier(random_state=seed)

    if abbreviation == "ET":
        return ExtraTreesClassifier(random_state=seed)

    if abbreviation == "AB":
        # The base estimator is set explicitly because the search spaces tune it
        # through 'clf__estimator__*' keys.
        return AdaBoostClassifier(
            estimator=DecisionTreeClassifier(random_state=seed), random_state=seed
        )

    if abbreviation == "GB":
        return GradientBoostingClassifier(random_state=seed)

    if abbreviation == "MLP":
        return MLPClassifier(random_state=seed, max_iter=5000)

    raise ValueError(
        f"Unknown model abbreviation: {abbreviation!r}. "
        f"Expected one of {sorted(MODEL_DISPLAY_NAMES)}."
    )


def build_model_pipeline(X: pd.DataFrame, abbreviation: str, seed: int) -> Pipeline:
    """Assemble the preprocessing + classifier pipeline for one model.

    Scale-sensitive models (see SCALED_MODELS in src.config) get the
    standardizing preprocessor; tree-based ensembles get the unscaled one.
    The step names are fixed to 'preprocessor' and 'clf' because the
    hyperparameter search spaces address the classifier as 'clf__*' and
    get_relevant_features reads the preprocessor by name.

    Parameters
    ----------
    X : pd.DataFrame
        Training features, used to infer the numeric and categorical columns.
    abbreviation : str
        Model abbreviation (see get_estimator).
    seed : int
        Random seed for the imputer and the classifier.

    Returns
    -------
    sklearn.pipeline.Pipeline
        Unfitted pipeline with steps ('preprocessor', 'clf').
    """
    if abbreviation in SCALED_MODELS:
        preprocessor = get_full_preprocessor(X, seed=seed)
    else:
        preprocessor = get_trees_preprocessor(X, seed=seed)

    return Pipeline(
        steps=[
            ("preprocessor", preprocessor),
            ("clf", get_estimator(abbreviation, seed=seed)),
        ]
    )


def get_display_name(abbreviation: str) -> str:
    """Return the human-readable name of a model, for plot titles.

    Parameters
    ----------
    abbreviation : str
        Model abbreviation (see get_estimator).

    Returns
    -------
    str
        Display name, falling back to the abbreviation itself if unknown.
    """
    return MODEL_DISPLAY_NAMES.get(abbreviation, abbreviation)
