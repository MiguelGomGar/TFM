"""Statistical data preparation helpers."""

import numpy as np
import pandas as pd
import scipy.stats as stats
from tableone import TableOne

from src.utils.dataframe_utils import get_categorical_columns, get_numeric_columns


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
        Two-column dataframe with col_name and target_var, NaN rows removed and
        rows ordered by the levels of target_var. That ordering is what lets the
        plotting pipeline recover the level order from the saved CSV, which no
        longer carries the categorical dtype.

    Raises
    ------
    ValueError
        If col_name is not numeric.
    """
    if not np.issubdtype(df[col_name].dtype, np.number):
        raise ValueError(f"The column {col_name} must be numeric.")

    stratified = df[[col_name, target_var]].dropna()
    return stratified.sort_values(by=target_var, kind="stable")


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


def compute_qq(
    df: pd.DataFrame, feature: str, ci_level: float = 0.95
) -> dict | None:
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


def build_qq_table(qq_data: dict) -> pd.DataFrame:
    """Flatten the output of ``compute_qq`` into a saveable table.

    Everything ``plot_qq`` needs is preserved: the per-point coordinates become
    columns and the four scalars are repeated down the rows. The normal-fit line
    is not stored because it is fully determined by the slope, the intercept and
    the range of the theoretical quantiles (see ``load_qq_data``).

    Parameters
    ----------
    qq_data : dict
        Dictionary returned by compute_qq().

    Returns
    -------
    pd.DataFrame
        Columns ['feature', 'ci_level', 'slope', 'intercept',
        'theoretical_quantiles', 'observed_quantiles', 'ci_lower', 'ci_upper'].
    """
    return pd.DataFrame(
        {
            "feature": qq_data["feature"],
            "ci_level": qq_data["ci_level"],
            "slope": qq_data["slope"],
            "intercept": qq_data["intercept"],
            "theoretical_quantiles": qq_data["osm"],
            "observed_quantiles": qq_data["osr"],
            "ci_lower": qq_data["y_lower"],
            "ci_upper": qq_data["y_upper"],
        }
    )


def load_qq_data(qq_table: pd.DataFrame) -> dict:
    """Rebuild the ``compute_qq`` dictionary from a saved Q-Q table.

    Parameters
    ----------
    qq_table : pd.DataFrame
        Table produced by build_qq_table() and read back from disk.

    Returns
    -------
    dict
        Same structure as compute_qq(), ready to be passed to plot_qq().
    """
    osm = qq_table["theoretical_quantiles"].to_numpy()
    slope = float(qq_table["slope"].iloc[0])
    intercept = float(qq_table["intercept"].iloc[0])
    y_line_x = np.linspace(osm.min(), osm.max(), 100)

    return {
        "feature": qq_table["feature"].iloc[0],
        "ci_level": float(qq_table["ci_level"].iloc[0]),
        "osm": osm,
        "osr": qq_table["observed_quantiles"].to_numpy(),
        "slope": slope,
        "intercept": intercept,
        "y_line_x": y_line_x,
        "y_line_y": slope * y_line_x + intercept,
        "y_lower": qq_table["ci_lower"].to_numpy(),
        "y_upper": qq_table["ci_upper"].to_numpy(),
    }


def create_table1(
    df: pd.DataFrame,
    target_var: str,
    nonnormal_vars: list[str] | None = None,
) -> pd.DataFrame:
    """Generate a Table 1 (descriptive statistics table) for clinical studies.

    Automatically identifies numeric and categorical variables and generates
    summary statistics both overall and stratified by the target variable.
    Numeric variables use median/IQR for non-normal distributions and
    mean/SD for normal distributions. Categorical variables show counts
    and percentages. Variable names are explicitly shown.

    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe with variables to summarize.
    target_var : str
        Name of the categorical stratification variable.
    nonnormal_vars : list[str], optional
        List of numeric column names with non-normal distributions.
        If None, defaults to an empty list.

    Returns
    -------
    pd.DataFrame
        A formatted Table 1 with variable names explicitly displayed,
        overall and stratified summary statistics, and p-values.
        Missing column is excluded from the output.

    Examples
    --------
    >>> df = pd.DataFrame({
    ...     'age': [25, 30, 35, 40, 45],
    ...     'group': ['A', 'B', 'A', 'B', 'A'],
    ...     'gender': ['M', 'F', 'M', 'F', 'M'],
    ...     'bmi': [22.5, 24.3, 23.1, 25.2, 22.8]
    ... })
    >>> table1 = create_table1(df, target_var='group', nonnormal_vars=['age'])
    >>> print(table1)
    """
    if nonnormal_vars is None:
        nonnormal_vars = []

    if target_var not in df.columns:
        raise ValueError(f"target_var '{target_var}' not found in dataframe columns.")

    numeric_cols = get_numeric_columns(df)
    categorical_cols = get_categorical_columns(df)

    if target_var in numeric_cols:
        numeric_cols.remove(target_var)
    if target_var in categorical_cols:
        categorical_cols.remove(target_var)

    all_vars = numeric_cols + categorical_cols

    if not all_vars:
        raise ValueError("No variables to summarize after excluding target_var.")

    table1_obj = TableOne(
        data=df,
        columns=all_vars,
        categorical=categorical_cols,
        groupby=target_var,
        nonnormal=nonnormal_vars,
        pval=True,
        missing=False,
    )

    table1_df = table1_obj.tableone.copy()

    if "Missing" in table1_df.columns:
        table1_df = table1_df.drop(columns=["Missing"])

    table1_df.index.name = "Variable"

    return table1_df
