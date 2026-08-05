"""Data cleaning helpers for clinical data."""

from dataclasses import dataclass
from logging import Logger

import pandas as pd

from src.config import (
    GROUP_ALLOCATION_VARIABLE,
    HIGHLY_CORRELATED_FEATURES,
    IDENTIFIER_VARIABLE,
    MISSING_RATE_THRESHOLD,
    PLAUSIBLE_RANGES,
    RISK_SCORES_PREFIX,
    TARGET_VARIABLE,
)
from src.data.feature_engineering import compute_risk_scores


def drop_columns(df: pd.DataFrame, columns: str | list) -> pd.DataFrame:
    """Drop specified columns from the dataframe.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.
    columns : str or list of str
        Column name(s) to drop. Non-existent columns are silently ignored.

    Returns
    -------
    pd.DataFrame
        DataFrame with specified columns removed.
    """
    return df.drop(columns=columns, errors="ignore")


def mask_out_of_range_values(
    dataframe: pd.DataFrame, ranges: dict[str, tuple[float, float]]
) -> tuple[pd.DataFrame, dict[str, int]]:
    """Set physiologically impossible values to missing.

    Values falling outside the plausible range of a feature are recording
    errors rather than genuine variability. They are replaced by NaN so that
    the downstream imputation step handles them like any other missing value,
    instead of being clipped to the bound and treated as observed data.

    Parameters
    ----------
    dataframe : pd.DataFrame
        Input dataframe.
    ranges : dict of str to tuple of float
        Mapping of column name to its inclusive (minimum, maximum) bounds.
        Columns absent from the dataframe are silently ignored.

    Returns
    -------
    pd.DataFrame
        Copy of the dataframe with out-of-range values replaced by NaN.
    dict of str to int
        Number of values masked per affected column.
    """
    result = dataframe.copy()
    masked_counts = {}

    for column, (minimum, maximum) in ranges.items():
        if column not in result.columns:
            continue
        out_of_range = result[column].notna() & (
            (result[column] < minimum) | (result[column] > maximum)
        )
        count = int(out_of_range.sum())
        if count:
            result.loc[out_of_range, column] = pd.NA
            masked_counts[column] = count

    return result, masked_counts


def drop_columns_by_prefix(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
    """Drop all columns with a specific prefix from the dataframe.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.
    prefix : str
        Column name prefix to match. All columns starting with this string
        will be removed.

    Returns
    -------
    pd.DataFrame
        DataFrame with matching columns removed.
    """
    columns_to_drop = [col for col in df.columns if col.startswith(prefix)]
    return df.drop(columns=columns_to_drop, errors="ignore")


def _select_high_missingness_columns(
    dataframe: pd.DataFrame, threshold: float
) -> list[str]:
    """Select column names where missing-value rate exceeds the threshold.

    Parameters
    ----------
    dataframe : pd.DataFrame
        Input dataframe.
    threshold : float
        Missing-rate threshold (0-1). Columns with missing rate > threshold
        are selected.

    Returns
    -------
    list of str
        Column names with missing rate exceeding the threshold.
    """
    missing_rates = dataframe.isna().mean()
    return missing_rates[missing_rates > threshold].index.tolist()


def drop_high_missingness_columns(
    dataframe: pd.DataFrame, threshold: float
) -> pd.DataFrame:
    """Drop columns with missing-value rate exceeding the threshold.

    Parameters
    ----------
    dataframe : pd.DataFrame
        Input dataframe.
    threshold : float
        Missing-rate threshold (0-1). Columns with missing rate > threshold
        are dropped.

    Returns
    -------
    pd.DataFrame
        DataFrame with high-missingness columns removed.
    """
    drop_columns = _select_high_missingness_columns(dataframe, threshold)
    return dataframe.drop(columns=drop_columns)


def drop_high_missingness_rows(
    dataframe: pd.DataFrame, threshold: float
) -> pd.DataFrame:
    """Drop rows with missing-value rate exceeding the threshold.

    Parameters
    ----------
    dataframe : pd.DataFrame
        Input dataframe.
    threshold : float
        Missing-rate threshold (0-1). Rows with missing rate > threshold
        are dropped.

    Returns
    -------
    pd.DataFrame
        DataFrame with high-missingness rows removed.
    """
    missing_rates = dataframe.isna().mean(axis=1)
    keep_rows = missing_rates[missing_rates <= threshold].index
    return dataframe.loc[keep_rows]


def inner_join(df1: pd.DataFrame, df2: pd.DataFrame, on: str | list) -> pd.DataFrame:
    """Perform an inner join (merge) of two dataframes on specified column(s).

    Parameters
    ----------
    df1 : pd.DataFrame
        Left dataframe.
    df2 : pd.DataFrame
        Right dataframe.
    on : str or list of str
        Column name(s) to join on. Must exist in both dataframes.

    Returns
    -------
    pd.DataFrame
        Result of the inner join, containing only rows where keys match in both
        input dataframes.
    """
    return df1.merge(df2, on=on, how="inner")


def remove_prefix_from_columns(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
    """Remove a prefix from all column names.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.
    prefix : str
        Prefix to remove from the start of each column name.

    Returns
    -------
    pd.DataFrame
        Copy of the dataframe with the prefix removed from all column names.
    """
    df = df.copy()
    df.columns = [col.removeprefix(prefix) for col in df.columns]
    return df


@dataclass(frozen=True)
class AnalysisDatasets:
    """The datasets every downstream phase is built on.

    Attributes
    ----------
    clinical : pd.DataFrame
        Cleaned clinical features for the full cohort.
    proteomic : pd.DataFrame
        Protein abundances joined with the target and grouping variables.
    clinical_matched : pd.DataFrame
        Clinical features restricted to the patients who also have proteomics,
        so the clinical and multimodal arms are trained on the same cohort.
    multimodal : pd.DataFrame
        Clinical and proteomic features joined. Keeps the identifier column,
        which the modelling phases exclude by selecting predictors explicitly.
    risk_scores : pd.DataFrame
        Published clinical risk scores, computed before any column is dropped.
    """

    clinical: pd.DataFrame
    proteomic: pd.DataFrame
    clinical_matched: pd.DataFrame
    multimodal: pd.DataFrame
    risk_scores: pd.DataFrame


def build_analysis_datasets(
    clinical_data: pd.DataFrame,
    proteomic_data: pd.DataFrame,
    missing_rate_threshold: float = MISSING_RATE_THRESHOLD,
    plausible_ranges: dict[str, tuple[float, float]] | None = None,
    identifier_variable: str = IDENTIFIER_VARIABLE,
    risk_scores_prefix: str = RISK_SCORES_PREFIX,
    highly_correlated_features: str | list[str] = HIGHLY_CORRELATED_FEATURES,
    target_variable: str = TARGET_VARIABLE,
    group_allocation_variable: str = GROUP_ALLOCATION_VARIABLE,
    logger: Logger | None = None,
) -> AnalysisDatasets:
    """Turn the intermediate clinical and raw proteomic data into the four
    analysis-ready datasets.

    The order of operations matters and is load-bearing:

    - The risk scores are computed before any cleaning, so they see every
      predictor they need even if some are dropped for missingness later.
    - The identifier is set aside before the missingness filters. It is never
      missing, so leaving it in would deflate every row's missing rate and
      change which rows survive.
    - ``clinical_matched`` is taken from the multimodal join rather than
      re-filtered, which guarantees it holds exactly the multimodal patients.

    Parameters
    ----------
    clinical_data : pd.DataFrame
        Intermediate clinical dataset, already selected and renamed.
    proteomic_data : pd.DataFrame
        Raw proteomic panel, one row per patient.
    missing_rate_threshold : float
        Columns and rows missing more than this fraction are dropped.
    plausible_ranges : dict, optional
        Feature to (minimum, maximum) physiological bounds. Defaults to
        config.PLAUSIBLE_RANGES.
    identifier_variable : str
        Patient identifier column, used to join and then dropped.
    risk_scores_prefix : str
        Prefix of the precomputed risk score columns, dropped from the
        predictors so the models cannot read the answer off a score.
    highly_correlated_features : str or list of str
        Predictors dropped after the collinearity analysis.
    target_variable : str
        Outcome column carried into the proteomic dataset.
    group_allocation_variable : str
        Trial arm column carried into the proteomic dataset.
    logger : logging.Logger, optional
        Logger for the progress messages.

    Returns
    -------
    AnalysisDatasets
        The clinical, proteomic, matched-clinical, multimodal and risk score
        tables, in the state they are saved to data/clean/.
    """
    if plausible_ranges is None:
        plausible_ranges = PLAUSIBLE_RANGES

    def log(message: str) -> None:
        if logger is not None:
            logger.info(message)

    log("Masking physiologically implausible values...")
    clinical_data, masked_counts = mask_out_of_range_values(
        clinical_data, plausible_ranges
    )
    if masked_counts:
        for column, count in masked_counts.items():
            log(f"  Masked {count} out-of-range value(s) in '{column}'")
    else:
        log("  No out-of-range values found.")

    log("Computing risk scores...")
    risk_scores = compute_risk_scores(clinical_data)

    identifier_column = clinical_data[identifier_variable]

    log("Dropping features with high missing rates...")
    clinical_data = drop_high_missingness_columns(
        clinical_data.drop(columns=identifier_variable),
        threshold=missing_rate_threshold,
    )

    log("Dropping rows with high missing rates...")
    clinical_data = drop_high_missingness_rows(
        clinical_data, threshold=missing_rate_threshold
    )
    clinical_data.insert(
        0, identifier_variable, identifier_column.loc[clinical_data.index]
    )

    log("Dropping highly correlated features...")
    clinical_data = drop_columns(clinical_data, highly_correlated_features)

    log("Dropping risk scores...")
    clinical_data = drop_columns_by_prefix(clinical_data, risk_scores_prefix)

    log(f"Joining clinical and proteomic data on {identifier_variable}...")
    multimodal_data = inner_join(clinical_data, proteomic_data, on=identifier_variable)

    log("Extracting merge subset for proteomic data...")
    clinical_merge_subset = clinical_data[
        [identifier_variable, group_allocation_variable, target_variable]
    ]
    proteomic_data = inner_join(
        proteomic_data, clinical_merge_subset, on=identifier_variable
    )

    log("Extracting clinical data matched to the proteomic cohort...")
    clinical_matched_data = multimodal_data[clinical_data.columns]

    log("Dropping identifier features for all datasets...")
    return AnalysisDatasets(
        clinical=drop_columns(clinical_data, identifier_variable),
        proteomic=drop_columns(proteomic_data, identifier_variable),
        clinical_matched=drop_columns(clinical_matched_data, identifier_variable),
        multimodal=multimodal_data,
        risk_scores=risk_scores,
    )
