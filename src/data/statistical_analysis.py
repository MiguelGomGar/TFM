"""Statistical data preparation helpers."""

import numpy as np
import pandas as pd
import scipy.stats as stats
from tableone import TableOne


def compute_numeric_distribution(df: pd.DataFrame, col_name: str) -> pd.Series:
    """Extract a numeric column and remove missing values.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.
    col_name : str
        Name of a numeric column.

    Returns
    -------
    pd.Series
        Numeric series with NaN values removed.

    Raises
    ------
    ValueError
        If the specified column is not numeric.
    """
    if not np.issubdtype(df[col_name].dtype, np.number):
        raise ValueError(f"The column {col_name} must be numeric.")

    return df[col_name].dropna()


def compute_categorical_distribution(df: pd.DataFrame, col_name: str) -> pd.DataFrame:
    """Calculate value counts and percentages for a categorical variable.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.
    col_name : str
        Name of a categorical column.

    Returns
    -------
    pd.DataFrame
        Summary table with columns ['feature', 'n', 'pct', 'pct_label'],
        one row per unique value (excluding NaN).
    """
    clean_df = df[df[col_name].notna()]
    df_summary = clean_df[col_name].value_counts().reset_index()
    df_summary.columns = [col_name, "n"]
    df_summary["pct"] = df_summary["n"] / df_summary["n"].sum()
    df_summary["pct_label"] = df_summary["pct"].map(lambda x: f"{x * 100:.1f}%")
    return df_summary


def compute_stratified_numeric_distribution(
    df: pd.DataFrame, col_name: str, target_var: str
) -> pd.DataFrame:
    """Extract a numeric column stratified by a grouping variable, removing NaNs.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.
    col_name : str
        Name of a numeric column.
    target_var : str
        Name of a categorical stratification variable.

    Returns
    -------
    pd.DataFrame
        Two-column dataframe with col_name and target_var, NaN rows removed.

    Raises
    ------
    ValueError
        If col_name is not numeric.
    """
    if not np.issubdtype(df[col_name].dtype, np.number):
        raise ValueError(f"The column {col_name} must be numeric.")

    return df[[col_name, target_var]].dropna()


def compute_stratified_categorical_distribution(
    df: pd.DataFrame, col_name: str, target_var: str
) -> pd.DataFrame | None:
    """Calculate row-normalized cross-tabulation of a categorical variable by group.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.
    col_name : str
        Name of a categorical feature to cross-tabulate.
    target_var : str
        Name of a categorical grouping variable.

    Returns
    -------
    pd.DataFrame or None
        Row-normalized cross-tabulation (0-100) with col_name on rows and
        target_var on columns. Returns None if col_name == target_var.
    """
    if col_name == target_var:
        return None

    df_clean = df[[col_name, target_var]].dropna()
    return (
        pd.crosstab(df_clean[col_name], df_clean[target_var], normalize="index") * 100
    )


def compute_qq(df: pd.DataFrame, feature: str, ci_level: float = 0.95):
    """Compute Q-Q plot coordinates and confidence bands for a numeric feature.

    Generates theoretical vs. observed quantiles and a confidence band around
    the normal-fit line for assessing normality.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe.
    feature : str
        Name of a numeric column to assess for normality.
    ci_level : float, default 0.95
        Confidence level (0-1) for the confidence band.

    Returns
    -------
    dict or None
        Dictionary with keys: 'feature', 'ci_level', 'osm', 'osr' (observed),
        'slope', 'intercept', 'y_line_x', 'y_line_y' (normal fit), 'y_lower',
        'y_upper' (confidence band). Returns None if feature has no data.
    """
    clean_data = df[feature].dropna().sort_values()
    n = len(clean_data)
    if n == 0:
        return None

    osm, osr = stats.probplot(clean_data, dist="norm", fit=False)
    osm = np.asarray(osm)
    osr = np.asarray(osr)

    slope, intercept, _ = stats.probplot(clean_data, dist="norm", fit=True)[1]
    p = (np.arange(1, n + 1) - 0.5) / n
    z = stats.norm.ppf(p)
    pdf = stats.norm.pdf(z)
    se = (slope / pdf) * np.sqrt(p * (1.0 - p) / n)
    crit = stats.norm.ppf(1.0 - (1.0 - ci_level) / 2.0)

    y_fit_obs = slope * osm + intercept

    return {
        "feature": feature,
        "ci_level": ci_level,
        "osm": osm,
        "osr": osr,
        "slope": slope,
        "intercept": intercept,
        "y_line_x": np.linspace(osm.min(), osm.max(), 100),
        "y_line_y": slope * np.linspace(osm.min(), osm.max(), 100) + intercept,
        "y_lower": y_fit_obs - crit * se,
        "y_upper": y_fit_obs + crit * se,
    }


def create_table1(
    data: pd.DataFrame,
    strat_var: str,
    cat_vars=None,
    nonnormal_vars=None,
) -> pd.DataFrame:
    """Create a Table 1 summary of patient characteristics with statistical tests.

    Stratifies descriptive statistics by a grouping variable and performs
    appropriate statistical tests (Chi-squared, t-test, Wilcoxon, ANOVA, etc.).
    Wrapper around the tableone.TableOne library.

    Parameters
    ----------
    data : pd.DataFrame
        Clinical dataframe with all features and the stratification variable.
    strat_var : str
        Column name for stratification (typically the outcome/group variable).
    cat_vars : list of str, optional
        Categorical variable names. If None, inferred from data types.
    nonnormal_vars : list of str, optional
        Continuous variables to treat as non-normal (use non-parametric tests).

    Returns
    -------
    pd.DataFrame
        Table 1 summary with descriptive statistics and p-values from
        stratified comparisons (Chi-squared for categorical, t-test/Wilcoxon
        for continuous depending on normality).
    """
    columns = [col for col in data.columns if col != strat_var]
    categorical = (
        None
        if cat_vars is None
        else [col for col in dict.fromkeys(cat_vars) if col != strat_var]
    )

    table = TableOne(
        data,
        columns=columns,
        categorical=categorical,
        nonnormal=nonnormal_vars,
        htest_name=True,
        pval=True,
        groupby=strat_var,
    )

    return table.tableone
