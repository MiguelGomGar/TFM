"""Agreement analysis and majority-voting ensemble for the top multimodal models.

Elastic Net (EN), SVM and MLP are the three best-performing models on the
multimodal (clinical + proteomic) external validation set trained by pipeline
12 (see ``results/models/multimodal_data/models_metrics.csv``). This module
reuses their already-fitted pipelines to:

1. Compare the positive-class probabilities they assign to the same held-out
   patients, before any binarization.
2. Combine their binary predictions into a simple majority-voting ensemble
   and score it on the same external validation set.

The SVM is always fitted with ``probability=False`` in the modelling phases
(see ``model_zoo.get_estimator``), since Platt scaling would slow the
hyperparameter search down and ranking metrics (ROC-AUC, PR-AUC) do not need
it. Comparing raw probabilities across models, however, does need a
calibrated SVM, so this module refits one copy of it, reusing the
already-selected hyperparameters, with ``probability=True`` enabled. The
majority vote itself does not depend on this recalibration, since
``SVC.predict()`` thresholds ``decision_function`` at zero regardless of the
``probability`` flag.
"""

from pathlib import Path

import joblib
import numpy as np
import pandas as pd
from sklearn.metrics import (
    average_precision_score,
    precision_recall_curve,
    roc_auc_score,
    roc_curve,
)
from sklearn.pipeline import Pipeline

from src.models.model_zoo import build_model_pipeline


def load_fitted_pipeline(model_dir, abbreviation: str) -> Pipeline:
    """Load one fitted pipeline persisted by a modelling phase.

    Parameters
    ----------
    model_dir : str or Path
        Output directory of the modelling phase (e.g. MULTIMODAL_MODELS_DIR).
    abbreviation : str
        Model abbreviation, matching the ``optimized_{abbreviation}.joblib``
        filename saved by ``results_saving.save_model``.

    Returns
    -------
    sklearn.pipeline.Pipeline
        Fitted pipeline with 'preprocessor' and 'clf' steps.

    Raises
    ------
    FileNotFoundError
        If no matching joblib file exists in model_dir.
    """
    file_path = Path(model_dir) / f"optimized_{abbreviation}.joblib"
    if not file_path.exists():
        raise FileNotFoundError(f"No fitted pipeline found at {file_path}.")
    return joblib.load(file_path)


def get_model_feature_columns(fitted_pipeline: Pipeline) -> list[str]:
    """Return the exact predictors a fitted pipeline was trained on.

    Reads ``feature_names_in_`` off the fitted preprocessor rather than
    re-deriving the feature set from ``features_kept.csv`` files, since those
    reflect the *current* state of the feature-selection phases and may have
    drifted from whatever feature set was in use when this particular model
    was actually fitted.

    Parameters
    ----------
    fitted_pipeline : sklearn.pipeline.Pipeline
        Fitted pipeline with a 'preprocessor' step.

    Returns
    -------
    list of str
        Predictor names, in the order the preprocessor expects them.
    """
    return list(fitted_pipeline.named_steps["preprocessor"].feature_names_in_)


def get_positive_class_probability(model, X) -> np.ndarray:
    """Return the calibrated probability of the positive class.

    Unlike ``model_evaluation.get_decision_scores``, this never falls back to
    ``decision_function``: every model compared in the probability violin
    plot must return an actual probability in [0, 1], not an arbitrary-scale
    ranking score, or the comparison across models would not be meaningful.

    Parameters
    ----------
    model : sklearn estimator
        Fitted classifier or pipeline exposing predict_proba.
    X : array-like or pd.DataFrame
        Samples to score.

    Returns
    -------
    np.ndarray
        Positive-class probability per sample.

    Raises
    ------
    AttributeError
        If the model does not expose predict_proba (e.g. an SVM fitted with
        probability=False).
    """
    if not hasattr(model, "predict_proba"):
        raise AttributeError(
            f"{type(model).__name__} does not expose predict_proba; the "
            "probability comparison requires a calibrated model."
        )
    return model.predict_proba(X)[:, 1]


def recalibrate_svm_probabilities(
    fitted_svm_pipeline: Pipeline, X_train, y_train, seed: int
) -> Pipeline:
    """Refit an already-tuned SVM pipeline with Platt scaling enabled.

    The classifier's hyperparameters (C, kernel, gamma, class_weight) are
    read off the already-tuned pipeline and reused as-is: no new search is
    run. Only ``probability`` is switched on, which makes SVC fit an internal
    5-fold Platt scaling on the training data only, so the external
    validation set is never touched by the recalibration.

    A fresh pipeline is built with ``model_zoo.build_model_pipeline`` (rather
    than cloning the loaded pipeline) so that the preprocessor and classifier
    are constructed by the current, installed scikit-learn version instead of
    depending on whatever internal state was pickled by the version that
    originally fitted the model.

    Parameters
    ----------
    fitted_svm_pipeline : sklearn.pipeline.Pipeline
        Already-tuned SVM pipeline (probability=False), as saved by phase 3.
    X_train : pd.DataFrame
        Training features, restricted to the columns the SVM was fitted on.
    y_train : pd.Series
        Binary training target.
    seed : int
        Random seed forwarded to the preprocessor and to the calibration
        folds.

    Returns
    -------
    sklearn.pipeline.Pipeline
        Newly fitted pipeline with predict_proba available.
    """
    tuned_params = fitted_svm_pipeline.named_steps["clf"].get_params()
    tuned_params["probability"] = True
    tuned_params["random_state"] = seed

    calibrated_pipeline = build_model_pipeline(X_train, "SVM", seed=seed)
    calibrated_pipeline.named_steps["clf"].set_params(**tuned_params)
    calibrated_pipeline.fit(X_train, y_train)

    return calibrated_pipeline


def build_probability_distribution_table(probabilities_by_model: dict) -> pd.DataFrame:
    """Stack per-model test-set probabilities into a long-format table.

    Parameters
    ----------
    probabilities_by_model : dict
        Mapping of model abbreviation to its array of positive-class
        probabilities on the test set, in the same patient order for every
        model.

    Returns
    -------
    pd.DataFrame
        Columns ['Model', 'Probability'], one row per (model, patient) pair.
    """
    frames = [
        pd.DataFrame({"Model": model, "Probability": probabilities})
        for model, probabilities in probabilities_by_model.items()
    ]
    return pd.concat(frames, ignore_index=True)


def majority_vote(predictions_by_model: dict) -> tuple[np.ndarray, np.ndarray]:
    """Combine binary predictions from several models by majority voting.

    Parameters
    ----------
    predictions_by_model : dict
        Mapping of model abbreviation to its binary (0/1) predictions on the
        same test set, in the same patient order.

    Returns
    -------
    vote_share : np.ndarray
        Fraction of models predicting the positive class for each patient.
        Only takes as many distinct values as there are models plus one
        (e.g. 0, 1/3, 2/3, 1 for three models), so it is a coarse ranking
        score, not a calibrated probability.
    ensemble_pred : np.ndarray
        Binary ensemble prediction: 1 if a strict majority of models voted
        positive, 0 otherwise. With an even number of models a tie defaults
        to 0.
    """
    votes = np.vstack(list(predictions_by_model.values()))
    n_models = votes.shape[0]
    vote_count = votes.sum(axis=0)
    vote_share = vote_count / n_models
    ensemble_pred = (vote_count > n_models / 2).astype(int)
    return vote_share, ensemble_pred


def build_ensemble_curves(y_test, scores_by_model: dict) -> dict:
    """Compute ROC and PR curve coordinates and AUCs for several scores.

    Ranking metrics only, by design: this analysis reports how well each
    score orders patients (ROC-AUC, PR-AUC and the curves themselves), not
    any threshold-dependent ('hard') metric. For the ensemble, pass
    ``vote_share`` (from majority_vote) as its score; because it only takes
    as many distinct values as there are models plus one, its curves are
    coarser step functions than the individual models' smooth curves.

    Parameters
    ----------
    y_test : array-like
        Binary external validation target.
    scores_by_model : dict
        Mapping of model/ensemble label to its continuous ranking score on
        the test set (predict_proba, decision_function or vote_share), in
        the same patient order as y_test.

    Returns
    -------
    dict
        Mapping of label to a dict with keys 'fpr', 'tpr', 'precision',
        'recall', 'roc_auc' and 'pr_auc', matching the layout returned by
        model_evaluation.evaluate_on_test's curves.
    """
    curves = {}
    for label, scores in scores_by_model.items():
        false_positive_rate, true_positive_rate, _ = roc_curve(y_test, scores)
        precision, recall, _ = precision_recall_curve(y_test, scores)
        curves[label] = {
            "fpr": false_positive_rate,
            "tpr": true_positive_rate,
            "precision": precision,
            "recall": recall,
            "roc_auc": roc_auc_score(y_test, scores),
            "pr_auc": average_precision_score(y_test, scores),
        }
    return curves


def build_agreement_table(predictions_by_model: dict, y_test, ensemble_pred=None) -> pd.DataFrame:
    """Summarize how often the models agree and how they err when they don't.

    Parameters
    ----------
    predictions_by_model : dict
        Mapping of model abbreviation to its binary (0/1) predictions on the
        same test set, in the same patient order (the same input accepted by
        majority_vote).
    y_test : array-like
        Binary external validation target, in the same patient order.
    ensemble_pred : np.ndarray, optional
        Binary majority-voting decision per patient, from majority_vote. If
        given, added as an 'Ensemble' column.

    Returns
    -------
    pd.DataFrame
        One row per patient, columns [<model columns>, 'True',
        'N_Positive_Votes', 'Unanimous', 'Ensemble' (if given)], where
        'Unanimous' flags patients on which every individual model agreed
        (with each other, not necessarily with the truth).
    """
    table = pd.DataFrame(predictions_by_model)
    table["True"] = np.asarray(y_test)
    table["N_Positive_Votes"] = table[list(predictions_by_model)].sum(axis=1)
    n_models = len(predictions_by_model)
    table["Unanimous"] = table["N_Positive_Votes"].isin([0, n_models])
    if ensemble_pred is not None:
        table["Ensemble"] = np.asarray(ensemble_pred)
    return table.reset_index(drop=True)
