"""Feature selection driven by the Elastic Net regularization.

The methodology implements feature selection through the regularization of the
Elastic Net logistic regression: every predictor whose coefficient is shrunk to
exactly zero is filtered out before the remaining models are trained.
"""

from pathlib import Path

import pandas as pd

from src.config import TARGET_VARIABLE
from src.utils.io import save_csv
from src.utils.results_saving import (
    _clean_feature_names,
    get_relevant_features,
    save_feature_selection_results,
)


def _build_coefficient_table(fitted_pipeline) -> pd.DataFrame:
    """Extract the fitted coefficients of a regularized pipeline.

    Parameters
    ----------
    fitted_pipeline : sklearn.pipeline.Pipeline
        Fitted pipeline with 'preprocessor' and 'clf' steps, where the
        classifier exposes a coef_ attribute.

    Returns
    -------
    pd.DataFrame
        Columns ['Feature', 'Coefficient', 'Abs_Coefficient', 'Selected'],
        sorted by absolute coefficient magnitude (largest first).
    """
    feature_names = fitted_pipeline.named_steps["preprocessor"].get_feature_names_out()
    coefficients = fitted_pipeline.named_steps["clf"].coef_[0]

    table = pd.DataFrame(
        {
            "Feature": _clean_feature_names(feature_names),
            "Coefficient": coefficients,
        }
    )
    table["Abs_Coefficient"] = table["Coefficient"].abs()
    table["Selected"] = table["Coefficient"] != 0
    return table.sort_values(by="Abs_Coefficient", ascending=False).reset_index(
        drop=True
    )


def _warn_if_penalty_cannot_shrink(fitted_pipeline, logger=None) -> None:
    """Log a warning when the fitted Elastic Net cannot zero out coefficients.

    Since scikit-learn 1.8 the penalty type is derived from l1_ratio alone, and
    only the 'saga' solver supports an elastic-net penalty. With l1_ratio = 0
    the penalty is a pure L2 and no coefficient can ever reach exactly zero,
    which would silently disable this whole filtering step.

    Parameters
    ----------
    fitted_pipeline : sklearn.pipeline.Pipeline
        Fitted pipeline whose 'clf' step is a LogisticRegression.
    logger : logging.Logger, optional
        Logger used to report the warning. If None, nothing is reported.

    Returns
    -------
    None
    """
    if logger is None:
        return

    classifier = fitted_pipeline.named_steps["clf"]
    solver = getattr(classifier, "solver", None)
    l1_ratio = getattr(classifier, "l1_ratio", None)

    if solver != "saga":
        logger.warning(
            f"The Elastic Net was fitted with solver={solver!r}; only 'saga' "
            "supports an elastic-net penalty, so no coefficient will be zeroed."
        )
    if not l1_ratio:
        logger.warning(
            f"The selected l1_ratio is {l1_ratio!r}: the penalty is a pure L2 "
            "and no coefficient can reach exactly zero. Check that the search "
            "space keeps l1_ratio strictly above 0."
        )


def select_features_with_elastic_net(
    fitted_en_pipeline, output_dir, identifier: str = "EN", logger=None
):
    """Split the predictors into those kept and those dropped by the Elastic Net.

    The pipeline must have been fitted on the training set only, so that the
    selection never sees the external validation set.

    Parameters
    ----------
    fitted_en_pipeline : sklearn.pipeline.Pipeline
        Fitted Elastic Net pipeline with 'preprocessor' and 'clf' steps.
    output_dir : str or Path
        Directory where the selection artifacts are written.
    identifier : str, default 'EN'
        Suffix used in the artifact filenames.
    logger : logging.Logger, optional
        Logger used to report the selection outcome.

    Returns
    -------
    relevant_features : list of str
        Predictors with a non-zero coefficient, sorted by absolute magnitude.
    irrelevant_features : list of str
        Predictors whose coefficient was shrunk to exactly zero.
    coefficients : pd.DataFrame
        Full coefficient table (see _build_coefficient_table).
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    _warn_if_penalty_cannot_shrink(fitted_en_pipeline, logger=logger)

    relevant_features, irrelevant_features = get_relevant_features(fitted_en_pipeline)
    coefficients = _build_coefficient_table(fitted_en_pipeline)

    # Full coefficient table plus the two requested lists, one file each.
    save_csv(coefficients, output_dir / f"feature_selection_{identifier}.csv")

    kept = coefficients.loc[coefficients["Selected"], ["Feature", "Coefficient"]].copy()
    kept.insert(0, "Rank", range(1, len(kept) + 1))
    save_csv(kept, output_dir / "features_kept.csv")

    removed = coefficients.loc[~coefficients["Selected"], ["Feature"]].copy()
    save_csv(removed, output_dir / "features_removed.csv")

    save_feature_selection_results(
        relevant_cols=relevant_features,
        irrelevant_cols=irrelevant_features,
        output_dir=output_dir,
        identifier=identifier,
    )

    if logger is not None:
        logger.info(
            f"Elastic Net kept {len(relevant_features)} feature(s) and removed "
            f"{len(irrelevant_features)}: {irrelevant_features}"
        )

    return relevant_features, irrelevant_features, coefficients


def apply_feature_filter(X: pd.DataFrame, irrelevant_features, logger=None):
    """Drop the features rejected by the Elastic Net from a feature matrix.

    If the regularization happened to zero out every predictor, the matrix is
    returned untouched so that the phase does not fail on an empty design
    matrix.

    Parameters
    ----------
    X : pd.DataFrame
        Feature matrix to filter.
    irrelevant_features : list of str
        Column names to drop.
    logger : logging.Logger, optional
        Logger used to report a degenerate selection.

    Returns
    -------
    pd.DataFrame
        Filtered feature matrix.
    """
    columns_to_drop = [column for column in irrelevant_features if column in X.columns]

    if len(columns_to_drop) == X.shape[1]:
        if logger is not None:
            logger.warning(
                "The Elastic Net zeroed out every predictor; keeping the full "
                "feature set so that the remaining models can still be fitted."
            )
        return X

    return X.drop(columns=columns_to_drop)


def get_clinical_predictors(multimodal_df, clinical_reference_df) -> list:
    """List the clinical predictors available in the multi-modal dataset.

    The clinical columns are taken from the cleaned clinical dataset so that the
    reduced arm uses exactly the same predictors as the earlier phases.

    Parameters
    ----------
    multimodal_df : pd.DataFrame
        Matched multi-modal dataset.
    clinical_reference_df : pd.DataFrame
        Cleaned clinical dataset used as the reference for the column list.

    Returns
    -------
    list of str
        Clinical predictor names, excluding the target variable.
    """
    return [
        column
        for column in clinical_reference_df.columns
        if column != TARGET_VARIABLE and column in multimodal_df.columns
    ]
