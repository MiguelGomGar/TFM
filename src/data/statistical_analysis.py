"""Statistical data preparation helpers."""

import numpy as np
import pandas as pd
import scipy.stats as stats


def compute_numeric_distribution(df: pd.DataFrame, col_name: str) -> pd.Series:
    """Return the numeric series to plot for a continuous variable."""

    if not np.issubdtype(df[col_name].dtype, np.number):
        raise ValueError(f"The column {col_name} must be numeric.")

    return df[col_name].dropna()


def compute_categorical_distribution(df: pd.DataFrame, col_name: str) -> pd.DataFrame:
    """Return summary counts and percentages for a categorical variable."""

    clean_df = df[df[col_name].notna()]
    df_summary = clean_df[col_name].value_counts().reset_index()
    df_summary.columns = [col_name, "n"]
    df_summary["pct"] = df_summary["n"] / df_summary["n"].sum()
    df_summary["pct_label"] = df_summary["pct"].map(lambda x: f"{x * 100:.1f}%")
    return df_summary


def compute_stratified_numeric_distribution(
    df: pd.DataFrame, col_name: str, target_var: str
) -> pd.DataFrame:
    """Return the cleaned dataframe needed for a stratified numeric plot."""

    if not np.issubdtype(df[col_name].dtype, np.number):
        raise ValueError(f"The column {col_name} must be numeric.")

    return df[[col_name, target_var]].dropna()


def compute_stratified_categorical_distribution(
    df: pd.DataFrame, col_name: str, target_var: str
) -> pd.DataFrame:
    """Return a normalized cross-tab for a stratified categorical plot."""

    if col_name == target_var:
        return None

    df_clean = df[[col_name, target_var]].dropna()
    return (
        pd.crosstab(df_clean[col_name], df_clean[target_var], normalize="index") * 100
    )


def compute_qq(df: pd.DataFrame, feature: str, ci_level: float = 0.95):
    """Compute the data needed for a Q-Q plot with confidence bands."""

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
    exact_vars=None,
):
    """
    Create a Table 1 dataframe summarizing patient characteristics stratified by strat_var.
    Performs standard statistical tests:
    - Categorical variables: Chi-squared or Fisher's Exact
    - Normal continuous: Student's t-test or ANOVA
    - Non-normal continuous: Wilcoxon Rank-Sum or Kruskal-Wallis
    """
    if cat_vars is None:
        cat_vars = []
    if nonnormal_vars is None:
        nonnormal_vars = []
    if exact_vars is None:
        exact_vars = []

    strat_groups = data[strat_var].unique()
    strat_groups = [g for g in strat_groups if pd.notna(g)]
    strat_groups.sort()

    rows = []

    overall_n = len(data)
    n_row = {"Feature": "n", "Overall": str(overall_n)}
    for g in strat_groups:
        n_row[f"Group: {g}"] = str(len(data[data[strat_var] == g]))
    n_row["p-value"] = ""
    n_row["Test"] = ""
    rows.append(n_row)

    for col in data.columns:
        if col == strat_var:
            continue

        is_cat = (
            col in cat_vars
            or data[col].dtype.name in ["category", "object", "bool"]
            or not np.issubdtype(data[col].dtype, np.number)
        )

        if is_cat:
            header_row = {"Feature": col, "Overall": "", "p-value": "", "Test": ""}
            for g in strat_groups:
                header_row[f"Group: {g}"] = ""

            p_val_str = ""
            test_name = ""
            try:
                ct = pd.crosstab(data[col], data[strat_var])
                if not ct.empty and ct.shape[0] >= 1 and ct.shape[1] >= 2:
                    if col in exact_vars and ct.shape == (2, 2):
                        _, p_val = stats.fisher_exact(ct)
                        test_name = "Fisher's Exact"
                    else:
                        _, p_val, _, _ = stats.chi2_contingency(ct)
                        test_name = "Chi-squared"
                    p_val_str = f"{p_val:.3f}" if p_val >= 0.001 else "<0.001"
            except Exception:
                pass

            header_row["p-value"] = p_val_str
            header_row["Test"] = test_name
            rows.append(header_row)

            levels = sorted(data[col].dropna().unique().tolist(), key=str)
            for lvl in levels:
                lvl_row = {"Feature": f"  {lvl}"}
                count_all = len(data[data[col] == lvl])
                pct_all = (count_all / data[col].notna().sum()) * 100
                lvl_row["Overall"] = f"{count_all} ({pct_all:.1f}%)"

                for g in strat_groups:
                    subset = data[data[strat_var] == g]
                    count_g = len(subset[subset[col] == lvl])
                    pct_g = (
                        (count_g / subset[col].notna().sum() * 100)
                        if subset[col].notna().sum() > 0
                        else 0.0
                    )
                    lvl_row[f"Group: {g}"] = f"{count_g} ({pct_g:.1f}%)"

                lvl_row["p-value"] = ""
                lvl_row["Test"] = ""
                rows.append(lvl_row)

        else:
            is_nonnormal = col in nonnormal_vars
            row_data = {"Feature": col}

            p_val_str = ""
            test_name = ""
            try:
                groups_data = [
                    data[data[strat_var] == g][col].dropna().values
                    for g in strat_groups
                ]
                groups_data = [g for g in groups_data if len(g) > 0]

                if len(groups_data) >= 2:
                    if is_nonnormal:
                        if len(groups_data) == 2:
                            _, p_val = stats.mannwhitneyu(
                                groups_data[0], groups_data[1]
                            )
                            test_name = "Wilcoxon Rank-Sum"
                        else:
                            _, p_val = stats.kruskal(*groups_data)
                            test_name = "Kruskal-Wallis"
                    else:
                        if len(groups_data) == 2:
                            _, p_val = stats.ttest_ind(
                                groups_data[0], groups_data[1], equal_var=False
                            )
                            test_name = "Welch's t-test"
                        else:
                            _, p_val = stats.f_oneway(*groups_data)
                            test_name = "ANOVA"
                    p_val_str = f"{p_val:.3f}" if p_val >= 0.001 else "<0.001"
            except Exception:
                pass

            row_data["p-value"] = p_val_str
            row_data["Test"] = test_name

            if is_nonnormal:
                median = data[col].median()
                q25 = data[col].quantile(0.25)
                q75 = data[col].quantile(0.75)
                row_data["Overall"] = f"{median:.2f} [{q25:.2f}, {q75:.2f}]"

                for g in strat_groups:
                    subset = data[data[strat_var] == g][col]
                    g_median = subset.median()
                    g_q25 = subset.quantile(0.25)
                    g_q75 = subset.quantile(0.75)
                    row_data[f"Group: {g}"] = (
                        f"{g_median:.2f} [{g_q25:.2f}, {g_q75:.2f}]"
                    )
            else:
                mean = data[col].mean()
                sd = data[col].std()
                row_data["Overall"] = f"{mean:.2f} (±{sd:.2f})"

                for g in strat_groups:
                    subset = data[data[strat_var] == g][col]
                    g_mean = subset.mean()
                    g_sd = subset.std()
                    row_data[f"Group: {g}"] = f"{g_mean:.2f} (±{g_sd:.2f})"

            rows.append(row_data)

    return pd.DataFrame(rows)
