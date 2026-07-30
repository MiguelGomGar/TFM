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
it. Rather than refitting it with ``probability=True`` just to compare it
against the other two models' probabilities, this module scores every model
with ``model_evaluation.get_decision_scores``, which uses ``predict_proba``
when available and falls back to ``decision_function`` for the SVM. The
scores being compared are therefore not all on the same [0, 1] scale: EN and
MLP contribute calibrated probabilities, while the SVM contributes a
signed, unbounded distance to the separating hyperplane.
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


def build_score_distribution_table(scores_by_model: dict) -> pd.DataFrame:
    """Stack per-model test-set scores into a long-format table.

    Parameters
    ----------
    scores_by_model : dict
        Mapping of model abbreviation to its array of positive-class scores
        on the test set (predict_proba, or decision_function for the SVM),
        in the same patient order for every model.

    Returns
    -------
    pd.DataFrame
        Columns ['Model', 'Score'], one row per (model, patient) pair.
    """
    frames = [
        pd.DataFrame({"Model": model, "Score": scores})
        for model, scores in scores_by_model.items()
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
