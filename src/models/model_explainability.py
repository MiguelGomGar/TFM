"""SHAP explainability for the project's best-performing models.

Every modelling phase elsewhere reports *how well* a model discriminates
(ROC-AUC, PR-AUC, hard metrics) but never *why* it predicts what it predicts.
This module derives Shapley additive explanations from the already-fitted
pipelines, at three levels:

1. Global: which predictors drive each model, and whether the three best models
   agree on the ranking.
2. Local: why a particular patient was classified the way they were.
3. By block: how much of the total attribution goes to the clinical variables
   versus the proteins, which is the attribution-based counterpart to the
   AUC-based modality comparison.

Two design decisions shape the whole module.

**Only the classifier is explained, not the whole pipeline.** The preprocessor
is applied once and the SHAP analysis runs on the transformed matrix. Feeding
raw data to the explainers would re-run the IterativeImputer thousands of times
per patient, which is both slow and stochastic. This is sound here because the
preprocessor encodes categorical variables with an OrdinalEncoder (one
ColumnTransformer lane per column), never one-hot, so transformed features map
one-to-one onto the original predictors and no post-hoc aggregation is needed.
The consequence to keep in mind when reading the results is that attributions
live in standardized units; since StandardScaler is monotone per variable, the
sign and the ranking carry over to the original scale, but the magnitudes are
not in clinical units.

**Each model is explained in its own native decision score** — the same score
``model_evaluation.get_decision_scores`` feeds to ROC/PR and the threshold
analysis thresholds on. That keeps this analysis consistent with the rest of
the project, but it means the *absolute* SHAP magnitudes are not comparable
across models (log-odds for EN, margin for the SVM, logit for the MLP).
Every cross-model comparison here is therefore built on scale-free quantities:
Spearman rank correlations and per-block shares.
"""

import time

import numpy as np
import pandas as pd
import shap
from scipy.stats import spearmanr
from sklearn.model_selection import train_test_split

from src.config import SEED, SHAP_ADDITIVITY_TOLERANCE, SHAP_BACKGROUND_SIZE, TEST_SIZE
from src.models.best_model import compute_youden_threshold, get_model_feature_columns, load_fitted_pipeline
from src.models.model_evaluation import get_decision_scores
from src.models.results_saving import _clean_feature_names
from src.utils.dataframe_utils import split_feature_blocks
from src.utils.io import read_csv

# Guard for the logit transform of the MLP probabilities: a probability of
# exactly 0 or 1 would map to +/- infinity and poison every SHAP value.
_PROBABILITY_EPSILON = 1e-12


# %% Preparation
def get_transformed_matrices(fitted_pipeline, X_train, X_test):
    """Apply a fitted preprocessor to both partitions and name the columns.

    Parameters
    ----------
    fitted_pipeline : sklearn.pipeline.Pipeline
        Fitted pipeline with 'preprocessor' and 'clf' steps.
    X_train : pd.DataFrame
        Training predictors, used to build the background set.
    X_test : pd.DataFrame
        External validation predictors, the patients being explained.

    Returns
    -------
    tuple
        (X_train_transformed, X_test_transformed, feature_names), where the
        arrays are 2D numpy matrices and feature_names holds the cleaned
        predictor names in the preprocessor's own output order.

    Notes
    -----
    The output order is *not* the input column order: the ColumnTransformer
    emits the numeric lane first and then one lane per categorical variable.
    Always pair the SHAP values with these names, never with X.columns.
    """
    preprocessor = fitted_pipeline.named_steps["preprocessor"]
    feature_names = _clean_feature_names(preprocessor.get_feature_names_out())
    return (
        np.asarray(preprocessor.transform(X_train)),
        np.asarray(preprocessor.transform(X_test)),
        feature_names,
    )


def build_background_set(X_train_transformed, size=SHAP_BACKGROUND_SIZE, seed=SEED, logger=None):
    """Draw the reference set every SHAP contribution is measured against.

    A SHAP value answers "how much did this variable move the prediction away
    from what the model would have said knowing nothing about it?", and the
    background set is what defines that "knowing nothing" baseline.

    Parameters
    ----------
    X_train_transformed : np.ndarray
        Transformed training matrix.
    size : int, default SHAP_BACKGROUND_SIZE
        Number of training patients to sample.
    seed : int, default SEED
        Random seed, making the sample reproducible across runs.
    logger : logging.Logger, optional
        Logger for the size report.

    Returns
    -------
    np.ndarray
        Background matrix of shape (min(size, n_train), n_features).
    """
    n_train = X_train_transformed.shape[0]
    if size >= n_train:
        if logger:
            logger.warning(
                f"Requested a background set of {size} patients but only {n_train} "
                f"training patients are available; using all of them."
            )
        return X_train_transformed
    return np.asarray(shap.sample(X_train_transformed, size, random_state=seed))


# %% Explainers
def _mlp_logit_output(clf):
    """Wrap an MLP's predicted probability as a log-odds function.

    The MLP is explained on the logit scale rather than on the probability
    scale so that the additivity property holds against a linear-in-features
    quantity, exactly as it does for the two linear models. Explaining
    predict_proba with shap's link='logit' instead would make the additivity
    check ambiguous, which is the very property this analysis needs to verify.

    Parameters
    ----------
    clf : sklearn.neural_network.MLPClassifier
        Fitted classifier.

    Returns
    -------
    callable
        Function mapping a transformed matrix to a 1D array of log-odds.
        Probabilities are clipped away from 0 and 1 before the transform.
    """

    def logit_output(X):
        probability = clf.predict_proba(X)[:, 1]
        probability = np.clip(probability, _PROBABILITY_EPSILON, 1 - _PROBABILITY_EPSILON)
        return np.log(probability / (1 - probability))

    return logit_output


def get_explained_output(fitted_pipeline, abbreviation):
    """Return the model output function the SHAP values will decompose.

    Parameters
    ----------
    fitted_pipeline : sklearn.pipeline.Pipeline
        Fitted pipeline with a 'clf' step.
    abbreviation : str
        Model abbreviation ('EN', 'SVM' or 'MLP').

    Returns
    -------
    tuple
        (output_function, output_label). The label names the score space
        ('log-odds', 'SVM margin' or 'logit(p)') for plot titles and logs.
    """
    clf = fitted_pipeline.named_steps["clf"]
    if abbreviation == "MLP":
        return _mlp_logit_output(clf), "logit(p)"
    label = "SVM margin" if abbreviation == "SVM" else "log-odds"
    return clf.decision_function, label


def build_explainer(fitted_pipeline, abbreviation, background, logger=None):
    """Build the most precise SHAP explainer available for a fitted model.

    The choice is driven by what each model actually is:

    - **EN** (LogisticRegression) and **SVM** *when its tuned kernel is
      linear*: the decision function is linear in the transformed features, so
      LinearExplainer gives the exact Shapley values in closed form, instantly.
      The coefficient/intercept pair is passed explicitly rather than the
      estimator, so the result does not depend on shap's introspection of
      sklearn internals.
    - **SVM with a non-linear kernel**: falls back to KernelExplainer. Not the
      current case (the tuned multimodal SVM uses a linear kernel) but kept so
      a future re-tuning that selects RBF still works.
    - **MLP**: KernelExplainer. DeepExplainer is *not* an option: it implements
      DeepLIFT-style backpropagation through the graph of an automatic
      differentiation framework (TensorFlow/Keras or PyTorch), whereas
      sklearn's MLPClassifier is plain NumPy with no such graph and no shap
      bridge. KernelExplainer is model-agnostic and, at this problem size, cheap.

    Parameters
    ----------
    fitted_pipeline : sklearn.pipeline.Pipeline
        Fitted pipeline with 'preprocessor' and 'clf' steps.
    abbreviation : str
        Model abbreviation ('EN', 'SVM' or 'MLP').
    background : np.ndarray
        Reference set from build_background_set.
    logger : logging.Logger, optional
        Logger for the explainer choice.

    Returns
    -------
    tuple
        (explainer, kind), where kind is 'exact' or 'approximate' and selects
        the additivity tolerance applied in check_shap_additivity.
    """
    clf = fitted_pipeline.named_steps["clf"]
    is_linear = abbreviation == "EN" or (abbreviation == "SVM" and clf.kernel == "linear")

    if is_linear:
        coefficients = np.asarray(clf.coef_).ravel()
        intercept = float(np.asarray(clf.intercept_).ravel()[0])
        if logger:
            logger.info(
                f"[{abbreviation}] Using LinearExplainer (exact) over a background of "
                f"{background.shape[0]} patients."
            )
        # An Independent masker is the current spelling of the interventional
        # perturbation: features are masked by substituting values drawn from
        # the background independently of the rest of the row. It matches what
        # KernelExplainer does for the MLP, keeping the three models comparable.
        masker = shap.maskers.Independent(background)
        return shap.LinearExplainer((coefficients, intercept), masker), "exact"

    output_function, output_label = get_explained_output(fitted_pipeline, abbreviation)
    if logger:
        reason = (
            f"kernel='{clf.kernel}' is not linear"
            if abbreviation == "SVM"
            else "sklearn's MLPClassifier has no autodiff graph for DeepExplainer"
        )
        logger.info(
            f"[{abbreviation}] Using KernelExplainer (approximate) on {output_label}: {reason}."
        )
    return shap.KernelExplainer(output_function, background), "approximate"


def compute_shap_values(explainer, X_test_transformed, feature_names, logger=None):
    """Compute the SHAP values and wrap them in a shap.Explanation.

    Normalizes the two output conventions shap uses: LinearExplainer returns a
    single 2D array, while KernelExplainer may return a list of one array per
    class for a classifier-like output. Anything other than a
    (n_samples, n_features) matrix is reduced to one before returning, so
    downstream code never has to branch on the explainer type.

    KernelExplainer's L1 feature selection is switched off. Its default,
    ``l1_reg="num_features(10)"``, keeps only the ten features it deems most
    relevant *per patient* and assigns exactly zero to the rest, which would
    silently truncate the importance ranking and distort the per-block
    aggregation. That default exists for cases where the coalition space is
    badly under-sampled; here shap draws 2 * n_features + 2048 coalitions for
    35 features, so the underlying regression is well determined and needs no
    regularization. Note this cannot be caught by the additivity check, which
    passes either way because the regression enforces the sum constraint.

    Parameters
    ----------
    explainer : shap.Explainer
        Explainer from build_explainer.
    X_test_transformed : np.ndarray
        Transformed external validation matrix.
    feature_names : list of str
        Predictor names, aligned to the columns of X_test_transformed.
    logger : logging.Logger, optional
        Logger for the elapsed time.

    Returns
    -------
    shap.Explanation
        Explanation carrying .values, .base_values, .data and .feature_names,
        ready for shap's own plotting functions.
    """
    keyword_arguments = {"l1_reg": 0} if isinstance(explainer, shap.KernelExplainer) else {}

    start = time.perf_counter()
    values = explainer.shap_values(X_test_transformed, **keyword_arguments)
    elapsed = time.perf_counter() - start

    values = np.asarray(values)
    if values.ndim == 3:
        # (n_samples, n_features, n_outputs): keep the positive class.
        values = values[:, :, -1]

    base_value = float(np.asarray(explainer.expected_value).ravel()[-1])
    if logger:
        logger.info(
            f"Computed SHAP values for {values.shape[0]} patients x "
            f"{values.shape[1]} features in {elapsed:.1f}s (base value {base_value:.4f})."
        )

    return shap.Explanation(
        values=values,
        base_values=np.full(values.shape[0], base_value),
        data=X_test_transformed,
        feature_names=list(feature_names),
    )


# %% Global importance
def build_global_importance_table(explanation, abbreviation) -> pd.DataFrame:
    """Rank predictors by mean absolute SHAP value for one model.

    Parameters
    ----------
    explanation : shap.Explanation
        Explanation from compute_shap_values.
    abbreviation : str
        Model abbreviation, recorded in the 'Model' column.

    Returns
    -------
    pd.DataFrame
        Columns ['Model', 'Feature', 'Mean_Abs_SHAP', 'Mean_SHAP',
        'Importance_Share', 'Rank'], sorted by importance. 'Mean_SHAP' keeps
        the sign, showing the average direction of the effect, while
        'Importance_Share' normalizes the magnitude to sum to 1 and is the
        quantity that can be compared across models.
    """
    values = np.asarray(explanation.values)
    mean_absolute = np.abs(values).mean(axis=0)
    total = mean_absolute.sum()

    table = pd.DataFrame(
        {
            "Model": abbreviation,
            "Feature": list(explanation.feature_names),
            "Mean_Abs_SHAP": mean_absolute,
            "Mean_SHAP": values.mean(axis=0),
            "Importance_Share": mean_absolute / total if total else 0.0,
        }
    )
    table = table.sort_values("Mean_Abs_SHAP", ascending=False).reset_index(drop=True)
    table["Rank"] = np.arange(1, len(table) + 1)
    return table


def build_shap_values_table(explanation, y_test) -> pd.DataFrame:
    """Lay out the full per-patient SHAP matrix for export.

    Saving this makes every downstream figure reproducible without recomputing
    the explainers, which matters for the MLP's KernelExplainer.

    Parameters
    ----------
    explanation : shap.Explanation
        Explanation from compute_shap_values.
    y_test : array-like
        Binary external validation target, aligned to the explained patients.

    Returns
    -------
    pd.DataFrame
        One row per patient: 'Test_Index', 'True_Label', 'Base_Value', then one
        column per predictor holding its SHAP value.
    """
    values = np.asarray(explanation.values)
    table = pd.DataFrame(values, columns=list(explanation.feature_names))
    table.insert(0, "Base_Value", np.asarray(explanation.base_values))
    table.insert(0, "True_Label", np.asarray(y_test))
    table.insert(0, "Test_Index", np.arange(len(table)))
    return table


def build_ranking_comparison_table(importance_by_model) -> pd.DataFrame:
    """Put the per-model importance rankings side by side.

    Parameters
    ----------
    importance_by_model : dict
        Mapping of model abbreviation to its global importance table.

    Returns
    -------
    pd.DataFrame
        One row per predictor with a 'Rank_{model}' and a 'Share_{model}'
        column per model, plus 'Mean_Rank', sorted by 'Mean_Rank'.
    """
    merged = None
    for abbreviation, table in importance_by_model.items():
        columns = table[["Feature", "Rank", "Importance_Share"]].rename(
            columns={"Rank": f"Rank_{abbreviation}", "Importance_Share": f"Share_{abbreviation}"}
        )
        merged = columns if merged is None else merged.merge(columns, on="Feature", how="outer")

    rank_columns = [column for column in merged.columns if column.startswith("Rank_")]
    merged["Mean_Rank"] = merged[rank_columns].mean(axis=1)
    return merged.sort_values("Mean_Rank").reset_index(drop=True)


def compute_rank_correlations(importance_by_model) -> pd.DataFrame:
    """Quantify how much the models agree on which predictors matter.

    Spearman is used rather than a comparison of raw SHAP magnitudes because
    the three models are explained on different score scales; only the ordering
    is comparable.

    Parameters
    ----------
    importance_by_model : dict
        Mapping of model abbreviation to its global importance table.

    Returns
    -------
    pd.DataFrame
        Columns ['Model_A', 'Model_B', 'Spearman_Rho', 'P_Value', 'N_Features'],
        one row per unordered pair of models.
    """
    abbreviations = list(importance_by_model)
    rankings = {
        abbreviation: table.set_index("Feature")["Rank"]
        for abbreviation, table in importance_by_model.items()
    }

    rows = []
    for index, model_a in enumerate(abbreviations):
        for model_b in abbreviations[index + 1 :]:
            paired = pd.concat([rankings[model_a], rankings[model_b]], axis=1, join="inner")
            rho, p_value = spearmanr(paired.iloc[:, 0], paired.iloc[:, 1])
            rows.append(
                {
                    "Model_A": model_a,
                    "Model_B": model_b,
                    "Spearman_Rho": float(rho),
                    "P_Value": float(p_value),
                    "N_Features": int(len(paired)),
                }
            )
    return pd.DataFrame(rows)


# %% Block contribution
def build_block_contribution_table(explanation, abbreviation, blocks) -> pd.DataFrame:
    """Aggregate the attribution of the clinical and proteomic blocks.

    Reports both the summed and the per-feature mean attribution, and both are
    needed to read the result honestly. The sum says how much of the model's
    total attribution a block carries, but it mechanically favours whichever
    block contributes more variables; the per-feature mean corrects for that
    and says how much a typical protein weighs against a typical clinical
    variable. Quoting only the sum would overstate the proteomic block.

    Parameters
    ----------
    explanation : shap.Explanation
        Explanation from compute_shap_values.
    abbreviation : str
        Model abbreviation, recorded in the 'Model' column.
    blocks : dict
        Mapping of block name to its list of predictor names, as returned by
        dataframe_utils.split_feature_blocks.

    Returns
    -------
    pd.DataFrame
        Columns ['Model', 'Block', 'N_Features', 'Sum_Mean_Abs_SHAP', 'Share',
        'Mean_Abs_SHAP_Per_Feature'].
    """
    mean_absolute = pd.Series(
        np.abs(np.asarray(explanation.values)).mean(axis=0),
        index=list(explanation.feature_names),
    )
    total = mean_absolute.sum()

    rows = []
    for block_name, block_features in blocks.items():
        present = [feature for feature in block_features if feature in mean_absolute.index]
        block_total = float(mean_absolute[present].sum()) if present else 0.0
        rows.append(
            {
                "Model": abbreviation,
                "Block": block_name,
                "N_Features": len(present),
                "Sum_Mean_Abs_SHAP": block_total,
                "Share": block_total / total if total else 0.0,
                "Mean_Abs_SHAP_Per_Feature": block_total / len(present) if present else 0.0,
            }
        )
    return pd.DataFrame(rows)


# %% Local explanations
def select_local_cases(y_test, y_score, threshold) -> pd.DataFrame:
    """Pick the individual patients worth explaining with a waterfall plot.

    Selection is by rule and fully deterministic, so the reported cases are
    stable across runs. Four archetypes plus a borderline patient:

    - Confident TP / TN: the model at its most convincing, showing which
      variables it leans on when it is right.
    - Confident FN: a recurrence the model confidently called safe. This is the
      clinically costliest failure mode and the most informative single plot.
    - Confident FP: a confident false alarm.
    - Borderline: the patient whose score sits closest to the decision
      threshold, where the attributions nearly cancel out.

    Parameters
    ----------
    y_test : array-like
        Binary external validation target.
    y_score : array-like
        Continuous decision score, in the same space the threshold applies to.
    threshold : float
        Decision cut-off separating predicted positives from negatives.

    Returns
    -------
    pd.DataFrame
        Columns ['Case', 'Test_Index', 'True_Label', 'Predicted_Label',
        'Score']. Patients are identified by their positional index in the
        external validation set, never by their study code. Archetypes with no
        matching patient are omitted.
    """
    y_test = np.asarray(y_test)
    y_score = np.asarray(y_score)
    y_predicted = (y_score > threshold).astype(int)

    # Each archetype is a mask plus the score extreme that makes it the most
    # "confident" example of its kind.
    archetypes = {
        "Confident TP": ((y_test == 1) & (y_predicted == 1), "max"),
        "Confident TN": ((y_test == 0) & (y_predicted == 0), "min"),
        "Confident FN": ((y_test == 1) & (y_predicted == 0), "min"),
        "Confident FP": ((y_test == 0) & (y_predicted == 1), "max"),
    }

    rows = []
    for case_name, (mask, direction) in archetypes.items():
        candidates = np.flatnonzero(mask)
        if candidates.size == 0:
            continue
        scores = y_score[candidates]
        chosen = candidates[np.argmax(scores) if direction == "max" else np.argmin(scores)]
        rows.append({"Case": case_name, "Test_Index": int(chosen)})

    rows.append(
        {"Case": "Borderline", "Test_Index": int(np.argmin(np.abs(y_score - threshold)))}
    )

    table = pd.DataFrame(rows)
    table["True_Label"] = y_test[table["Test_Index"]]
    table["Predicted_Label"] = y_predicted[table["Test_Index"]]
    table["Score"] = y_score[table["Test_Index"]]
    return table


# %% Quality control
def check_shap_additivity(explanation, model_output, abbreviation, kind) -> pd.DataFrame:
    """Verify that the SHAP values reconstruct the model's own predictions.

    The defining property of Shapley additive explanations is
    ``base_value + sum(shap_values) == model output``. Checking it end to end
    catches misaligned feature orders, a wrong output space and silent shape
    errors in one go, and it is the only meaningful correctness test available
    for an approximate explainer.

    Parameters
    ----------
    explanation : shap.Explanation
        Explanation from compute_shap_values.
    model_output : array-like
        The model's own score for the explained patients, evaluated in the same
        space the explainer worked in (get_explained_output).
    abbreviation : str
        Model abbreviation, recorded in the 'Model' column.
    kind : str
        'exact' or 'approximate', selecting the tolerance from
        SHAP_ADDITIVITY_TOLERANCE.

    Returns
    -------
    pd.DataFrame
        Single row: ['Model', 'Explainer', 'N_Samples', 'Max_Abs_Error',
        'Mean_Abs_Error', 'Tolerance', 'Passed', 'Zero_Fraction'].

    Notes
    -----
    'Zero_Fraction' is reported alongside because additivity alone does not
    prove the values are sound: an explainer that zeroes out most features and
    piles their credit onto the rest still satisfies the sum constraint
    exactly. A fraction far above zero on a dense model means the attributions
    were truncated and the ranking should not be trusted.
    """
    values = np.asarray(explanation.values)
    reconstructed = np.asarray(explanation.base_values) + values.sum(axis=1)
    error = np.abs(reconstructed - np.asarray(model_output))
    tolerance = SHAP_ADDITIVITY_TOLERANCE[kind]

    return pd.DataFrame(
        [
            {
                "Model": abbreviation,
                "Explainer": kind,
                "N_Samples": int(error.size),
                "Max_Abs_Error": float(error.max()),
                "Mean_Abs_Error": float(error.mean()),
                "Tolerance": tolerance,
                "Passed": bool(error.max() <= tolerance),
                "Zero_Fraction": float((values == 0).mean()),
            }
        ]
    )


def check_shap_dimensions(explanation, n_samples, feature_names) -> None:
    """Assert the SHAP matrix has the expected shape and no invalid entries.

    Fails loudly rather than letting a mis-shaped or non-finite matrix
    propagate silently into the importance tables and figures.

    Parameters
    ----------
    explanation : shap.Explanation
        Explanation from compute_shap_values.
    n_samples : int
        Expected number of explained patients.
    feature_names : list of str
        Expected predictor names.

    Raises
    ------
    ValueError
        If the shape does not match, or if any value is NaN or infinite.
    """
    values = np.asarray(explanation.values)
    expected_shape = (n_samples, len(feature_names))
    if values.shape != expected_shape:
        raise ValueError(f"SHAP values have shape {values.shape}, expected {expected_shape}.")
    if not np.isfinite(values).all():
        raise ValueError("SHAP values contain NaN or infinite entries.")
    if list(explanation.feature_names) != list(feature_names):
        raise ValueError("SHAP feature names do not match the preprocessor output names.")


# %% Orchestration
def load_youden_threshold(abbreviation, threshold_source_file, y_test, y_score, logger=None) -> float:
    """Reuse the Youden cut-off already chosen by the threshold analysis.

    Reading it back keeps the local cases consistent with the confusion
    matrices of the threshold analysis phase: a patient labeled a false
    negative here is the same patient counted as a false negative there.
    Recomputed on the fly if that phase has not been run yet.

    Parameters
    ----------
    abbreviation : str
        Model abbreviation.
    threshold_source_file : str or Path
        Path to the threshold_info.csv saved by the threshold analysis phase.
    y_test : array-like
        Binary external validation target.
    y_score : array-like
        Continuous decision score from model_evaluation.get_decision_scores.
    logger : logging.Logger, optional
        Logger for the fallback warning.

    Returns
    -------
    float
        The Youden-optimal decision threshold.
    """
    if threshold_source_file.exists():
        thresholds = read_csv(threshold_source_file)
        optimal = thresholds[
            (thresholds["Model"] == abbreviation)
            & (thresholds["Scenario"].str.startswith("Optimal"))
        ]
        if not optimal.empty:
            return float(optimal["Threshold"].iloc[0])

    if logger:
        logger.warning(
            f"[{abbreviation}] No stored threshold in {threshold_source_file}; recomputing it."
        )
    return compute_youden_threshold(y_test, y_score)["threshold"]


def explain_model(
    abbreviation: str,
    model_dir,
    threshold_source_file,
    df: pd.DataFrame,
    y: pd.Series,
    seed: int = SEED,
    test_size: float = TEST_SIZE,
    background_size: int = SHAP_BACKGROUND_SIZE,
    logger=None,
) -> dict:
    """Run the full SHAP analysis for one fitted pipeline.

    Parameters
    ----------
    abbreviation : str
        Model abbreviation, matching the ``optimized_{abbreviation}.joblib``
        filename saved by the multimodal modelling phase.
    model_dir : str or Path
        Output directory of the modelling phase the pipeline was fitted in.
    threshold_source_file : str or Path
        Path to the threshold_info.csv saved by the threshold analysis phase,
        used to keep the local cases consistent with it.
    df : pd.DataFrame
        Cleaned multimodal dataset (predictors + target columns).
    y : pd.Series
        Encoded binary target, aligned to df's index.
    seed : int, default SEED
        Random seed for the train/test split and the background sample.
    test_size : float, default TEST_SIZE
        Size of the external validation split.
    background_size : int, default SHAP_BACKGROUND_SIZE
        Number of training patients sampled for the SHAP background set.
    logger : logging.Logger, optional
        Logger for progress and result reporting.

    Returns
    -------
    dict
        Keys 'explanation', 'importance', 'blocks', 'block_contribution',
        'shap_values_table', 'local_cases' and 'validation'.
    """
    if logger:
        logger.info(f"Loading the fitted {abbreviation} pipeline from {model_dir}...")
    model = load_fitted_pipeline(model_dir, abbreviation)
    feature_columns = get_model_feature_columns(model)

    # Same seed, test_size and stratification as the multimodal modelling phase,
    # so the reconstructed partition matches it record by record. Unlike the
    # threshold analysis, the training half is needed here: it is the reference
    # distribution the SHAP contributions are measured against.
    X = df[feature_columns]
    X_train, X_test, _, y_test = train_test_split(
        X, y, test_size=test_size, random_state=seed, shuffle=True, stratify=y
    )
    if logger:
        logger.info(
            f"[{abbreviation}] Reconstructed the partition: {X_train.shape[0]} training and "
            f"{X_test.shape[0]} external validation patients ({y_test.mean():.1%} recurrence)."
        )

    # ------------------------------------------------------------------
    # 1. SHAP values on the transformed matrix
    # ------------------------------------------------------------------
    X_train_transformed, X_test_transformed, feature_names = get_transformed_matrices(
        model, X_train, X_test
    )
    background = build_background_set(
        X_train_transformed, size=background_size, seed=seed, logger=logger
    )
    explainer, kind = build_explainer(model, abbreviation, background, logger=logger)
    explanation = compute_shap_values(explainer, X_test_transformed, feature_names, logger=logger)

    # ------------------------------------------------------------------
    # 2. Quality control, before anything is derived from the values
    # ------------------------------------------------------------------
    check_shap_dimensions(explanation, X_test_transformed.shape[0], feature_names)
    output_function, output_label = get_explained_output(model, abbreviation)
    validation = check_shap_additivity(
        explanation, output_function(X_test_transformed), abbreviation, kind
    )
    if logger:
        logger.info(
            f"[{abbreviation}] Additivity on {output_label}: max error "
            f"{validation['Max_Abs_Error'].iloc[0]:.2e} vs tolerance "
            f"{validation['Tolerance'].iloc[0]:.0e} -> "
            f"{'PASSED' if validation['Passed'].iloc[0] else 'FAILED'}; "
            f"{validation['Zero_Fraction'].iloc[0]:.1%} of the values are exactly zero."
        )
        if not validation["Passed"].iloc[0]:
            logger.error(
                f"[{abbreviation}] SHAP values do not reconstruct the model output; "
                f"treat this model's explanations as unreliable."
            )
        if validation["Zero_Fraction"].iloc[0] > 0.05:
            logger.warning(
                f"[{abbreviation}] An unexpectedly large share of the SHAP values is exactly "
                f"zero, which suggests the explainer truncated the attributions; the "
                f"importance ranking and the block shares would be biased."
            )

    # ------------------------------------------------------------------
    # 3. Global importance and block contribution
    # ------------------------------------------------------------------
    importance = build_global_importance_table(explanation, abbreviation)
    blocks = split_feature_blocks(feature_names)
    if not blocks["Clinical"] or not blocks["Proteomic"]:
        raise ValueError(
            f"[{abbreviation}] Expected both feature blocks to be non-empty, got "
            f"{len(blocks['Clinical'])} clinical and {len(blocks['Proteomic'])} proteomic."
        )
    if logger:
        logger.info(
            f"[{abbreviation}] Feature blocks: {len(blocks['Clinical'])} clinical, "
            f"{len(blocks['Proteomic'])} proteomic (total {len(feature_names)})."
        )
    block_contribution = build_block_contribution_table(explanation, abbreviation, blocks)
    if logger:
        for _, row in block_contribution.iterrows():
            logger.info(
                f"[{abbreviation}] {row['Block']}: {row['Share']:.1%} of total |SHAP| across "
                f"{int(row['N_Features'])} predictors "
                f"({row['Mean_Abs_SHAP_Per_Feature']:.4f} per predictor)."
            )

    # ------------------------------------------------------------------
    # 4. Local cases, selected in the threshold analysis' score space
    # ------------------------------------------------------------------
    y_score = get_decision_scores(model, X_test)
    threshold = load_youden_threshold(abbreviation, threshold_source_file, y_test, y_score, logger=logger)
    local_cases = select_local_cases(y_test, y_score, threshold)
    if logger:
        logger.info(
            f"[{abbreviation}] Selected {len(local_cases)} local cases at threshold "
            f"{threshold:.4f}: {', '.join(local_cases['Case'])}."
        )

    return {
        "explanation": explanation,
        "importance": importance,
        "blocks": blocks,
        "block_contribution": block_contribution,
        "shap_values_table": build_shap_values_table(explanation, y_test),
        "local_cases": local_cases,
        "validation": validation,
    }
