# %% Load modules
import pandas as pd
import numpy as np
import scipy.stats as stats
import matplotlib.pyplot as plt
import seaborn as sns

# %% Color palette
colors = {
    "no": "#4682b4",
    "yes": "#800080",
    "control": "#4682b4",
    "intervention": "#800080",
}


# %% Single Variable Distribution Plots
def plot_numeric_distribution(df, col_name):
    """
    Renders a professional histogram for a single continuous numeric variable.
    """
    if not np.issubdtype(df[col_name].dtype, np.number):
        raise ValueError(f"The column {col_name} must be numeric.")

    plt.figure(figsize=(7, 5))
    sns.histplot(
        data=df,
        x=col_name,
        bins=30,
        color="#16a085",
        edgecolor="white",
        alpha=0.7,
        kde=False,
    )

    # Custom styling
    plt.title(f"Distribution of {col_name}", fontsize=12, fontweight="bold", pad=15)
    plt.xlabel(col_name, fontsize=10, fontweight="bold", color="#2c3e50")
    plt.ylabel("n", fontsize=10, fontweight="bold", color="#2c3e50")
    plt.xticks(fontweight="bold", color="#34495e")
    plt.yticks(fontweight="bold", color="#34495e")
    plt.grid(axis="y", color="#eaeded", linewidth=0.4)
    plt.gca().set_axisbelow(True)
    sns.despine()
    plt.tight_layout()

    return plt.gcf()


def plot_categorical_distribution(df, col_name):
    """
    Renders a professional bar chart for a single categorical variable,
    displaying dynamic percentage labels on top of each bar.
    """
    clean_df = df[df[col_name].notna()]
    df_summary = clean_df[col_name].value_counts().reset_index()
    df_summary.columns = [col_name, "n"]
    df_summary["pct"] = df_summary["n"] / df_summary["n"].sum()
    df_summary["pct_label"] = df_summary["pct"].map(lambda x: f"{x * 100:.1f}%")

    plt.figure(figsize=(8, len(df_summary) * 0.5 + 2))
    # Horizontal bar plot
    bars = plt.barh(
        df_summary[col_name].astype(str),
        df_summary["n"],
        color="#16a085",
        edgecolor="#2c3e50",
        alpha=0.8,
        height=0.6,
        linewidth=0.5,
    )

    # Add labels
    max_val = df_summary["n"].max()
    for bar, label in zip(bars, df_summary["pct_label"]):
        width = bar.get_width()
        plt.text(
            width + (max_val * 0.02),
            bar.get_y() + bar.get_height() / 2,
            label,
            ha="left",
            va="center",
            fontsize=9.5,
            fontweight="bold",
            color="#2c3e50",
        )

    plt.xlim(0, max_val * 1.15)
    plt.title(f"Distribution of {col_name}", fontsize=12, fontweight="bold", pad=15)
    plt.xlabel("n", fontsize=10, fontweight="bold", color="#2c3e50")
    plt.ylabel(None)
    plt.xticks(fontweight="bold", color="#34495e")
    plt.yticks(fontweight="bold", color="#34495e")
    plt.grid(axis="x", color="#eaeded", linewidth=0.4)
    plt.gca().set_axisbelow(True)
    sns.despine(left=True, bottom=True)
    plt.tight_layout()

    return plt.gcf()


def plot_stratified_numeric_distribution(df, col_name, target_var):
    """
    Renders a professional violin plot for a continuous numeric variable,
    stratified by a target categorical variable.
    """
    if not np.issubdtype(df[col_name].dtype, np.number):
        raise ValueError(f"The column {col_name} must be numeric.")

    fig, ax = plt.subplots(figsize=(8, 5))
    sns.violinplot(
        data=df,
        y=target_var,
        x=col_name,
        hue=target_var,
        palette=colors,
        dodge=False,
        inner="quartile",
        cut=0,
        linewidth=1.1,
        ax=ax,
    )

    handles, labels = ax.get_legend_handles_labels()
    if handles:
        ax.legend(
            handles,
            labels,
            title=target_var,
            loc="center left",
            bbox_to_anchor=(1.02, 0.5),
            frameon=True,
            borderaxespad=0.0,
        )

    ax.set_title(
        f"Distribution of {col_name} stratified by {target_var}",
        fontsize=12,
        fontweight="bold",
        pad=15,
    )
    ax.set_xlabel(col_name, fontsize=10, fontweight="bold", color="#2c3e50")
    ax.set_ylabel(None)
    ax.tick_params(axis="x", labelsize=10, labelcolor="#34495e")
    ax.tick_params(axis="y", labelsize=10, labelcolor="#34495e")
    ax.grid(axis="x", color="#eaeded", linewidth=0.4)
    ax.set_axisbelow(True)
    sns.despine()
    plt.tight_layout(rect=(0, 0, 0.82, 1))

    return fig


def plot_stratified_categorical_distribution(df, col_name, target_var):
    """
    Renders a horizontal 100% stacked bar chart with internal percentage labels.
    """
    if col_name == target_var:
        return None

    # Calculate proportions
    df_clean = df[[col_name, target_var]].dropna()
    df_ct = (
        pd.crosstab(df_clean[col_name], df_clean[target_var], normalize="index") * 100
    )

    plt.figure(figsize=(9, len(df_ct) * 0.5 + 2))

    # Plot stacked bar chart
    ax = df_ct.plot(
        kind="barh",
        stacked=True,
        colormap="cividis",
        edgecolor="#2c3e50",
        alpha=0.85,
        linewidth=0.4,
        ax=plt.gca(),
    )

    # Add percentage labels inside bars
    for p in ax.patches:
        width = p.get_width()
        if width > 5:  # only show if width is reasonable
            x = p.get_x() + width / 2
            y = p.get_y() + p.get_height() / 2
            ax.text(
                x,
                y,
                f"{width:.0f}%",
                ha="center",
                va="center",
                color="black",
                fontweight="bold",
                fontsize=10,
            )

    plt.xlim(0, 100)
    plt.gca().xaxis.set_major_formatter(plt.FuncFormatter(lambda val, _: f"{val:.0f}%"))
    plt.title(
        f"Distribution of {col_name} stratified by {target_var}",
        fontsize=12,
        fontweight="bold",
        pad=15,
    )
    plt.xlabel(None)
    plt.ylabel(None)
    plt.legend(
        title=target_var,
        loc="center left",
        bbox_to_anchor=(1.02, 0.5),
        frameon=True,
    )
    plt.xticks(fontweight="bold", color="#34495e")
    plt.yticks(fontweight="bold", color="#34495e")
    sns.despine()
    plt.tight_layout(rect=(0, 0, 0.82, 1))

    return plt.gcf()


# %% Normality Tests & Q-Q Plots
def plot_qq(df, feature, ci_level=0.95):
    """
    Single Variable Q-Q Plot with Confidence Bands
    """
    clean_data = df[feature].dropna().sort_values()
    n = len(clean_data)
    if n == 0:
        return None

    # Theoretical quantiles
    osm, osr = stats.probplot(clean_data, dist="norm", fit=False)
    osm = np.asarray(osm)
    osr = np.asarray(osr)

    # Fit line
    slope, intercept, r_val = stats.probplot(clean_data, dist="norm", fit=True)[1]

    plt.figure(figsize=(7, 5))
    plt.scatter(osm, osr, color="#16a085", alpha=0.6, label="Observed")

    # Line of fit
    x_line = np.linspace(osm.min(), osm.max(), 100)
    y_line = slope * x_line + intercept
    plt.plot(
        x_line,
        y_line,
        color="#e74c3c",
        linestyle="-",
        linewidth=1.5,
        label="Normal Line",
    )

    # Confidence Bands (approximate standard asymptotic bands for QQ plot)
    # se = (slope / pdf(x)) * sqrt(p*(1-p)/n)
    # Using a simple approximation or standard error band:
    p = (np.arange(1, n + 1) - 0.5) / n
    z = stats.norm.ppf(p)
    pdf = stats.norm.pdf(z)

    # Standard error of the order statistics
    se = (slope / pdf) * np.sqrt(p * (1.0 - p) / n)
    crit = stats.norm.ppf(1.0 - (1.0 - ci_level) / 2.0)

    y_fit_obs = slope * osm + intercept
    y_lower = y_fit_obs - crit * se
    y_upper = y_fit_obs + crit * se

    plt.fill_between(
        osm,
        y_lower,
        y_upper,
        color="#16a085",
        alpha=0.15,
        label=f"{ci_level*100:.0f}% CI Band",
    )

    plt.title(f"Q-Q Plot: {feature}", fontsize=12, fontweight="bold")
    plt.xlabel("Theoretical Quantiles (Standard Normal)", fontsize=10, color="#2c3e50")
    plt.ylabel(f"Observed Values for {feature}", fontsize=10, color="#2c3e50")
    plt.legend(loc="upper left")
    plt.grid(color="#eaeded", linewidth=0.4)
    sns.despine()
    plt.tight_layout()

    return plt.gcf()


# %% Summary Tables
def create_table1(data, strat_var, cat_vars=None, nonnormal_vars=None, exact_vars=None):
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

    # Initialize list of summary rows
    rows = []

    # 1. Row for Overall Count N
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

        # Determine variable type
        is_cat = (
            col in cat_vars
            or data[col].dtype.name in ["category", "object", "bool"]
            or not np.issubdtype(data[col].dtype, np.number)
        )

        if is_cat:
            # Categorical Summarization
            # Add main feature header
            header_row = {"Feature": col, "Overall": "", "p-value": "", "Test": ""}
            for g in strat_groups:
                header_row[f"Group: {g}"] = ""

            # Statistical test
            p_val_str = ""
            test_name = ""
            try:
                # Prepare contingency table
                ct = pd.crosstab(data[col], data[strat_var])
                if not ct.empty and ct.shape[0] >= 1 and ct.shape[1] >= 2:
                    if col in exact_vars and ct.shape == (2, 2):
                        odds, p_val = stats.fisher_exact(ct)
                        test_name = "Fisher's Exact"
                    else:
                        chi2, p_val, dof, ex = stats.chi2_contingency(ct)
                        test_name = "Chi-squared"
                    p_val_str = f"{p_val:.3f}" if p_val >= 0.001 else "<0.001"
            except Exception:
                pass

            header_row["p-value"] = p_val_str
            header_row["Test"] = test_name
            rows.append(header_row)

            # Add level categories
            levels = sorted(data[col].dropna().unique().tolist(), key=str)
            for lvl in levels:
                lvl_row = {"Feature": f"  {lvl}"}

                # Overall
                count_all = len(data[data[col] == lvl])
                pct_all = (count_all / data[col].notna().sum()) * 100
                lvl_row["Overall"] = f"{count_all} ({pct_all:.1f}%)"

                # Stratified
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
            # Continuous Summarization
            is_nonnormal = col in nonnormal_vars

            row_data = {"Feature": col}

            # Statistical test
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
                            stat, p_val = stats.mannwhitneyu(
                                groups_data[0], groups_data[1]
                            )
                            test_name = "Wilcoxon Rank-Sum"
                        else:
                            stat, p_val = stats.kruskal(*groups_data)
                            test_name = "Kruskal-Wallis"
                    else:
                        if len(groups_data) == 2:
                            stat, p_val = stats.ttest_ind(
                                groups_data[0], groups_data[1], equal_var=False
                            )
                            test_name = "Welch's t-test"
                        else:
                            stat, p_val = stats.f_oneway(*groups_data)
                            test_name = "ANOVA"
                    p_val_str = f"{p_val:.3f}" if p_val >= 0.001 else "<0.001"
            except Exception:
                pass

            row_data["p-value"] = p_val_str
            row_data["Test"] = test_name

            # Overall Stats
            if is_nonnormal:
                median = data[col].median()
                q25 = data[col].quantile(0.25)
                q75 = data[col].quantile(0.75)
                row_data["Overall"] = f"{median:.2f} [{q25:.2f}, {q75:.2f}]"

                # Stratified
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

                # Stratified
                for g in strat_groups:
                    subset = data[data[strat_var] == g][col]
                    g_mean = subset.mean()
                    g_sd = subset.std()
                    row_data[f"Group: {g}"] = f"{g_mean:.2f} (±{g_sd:.2f})"

            rows.append(row_data)

    # Convert list of rows to a DataFrame
    table1_df = pd.DataFrame(rows)

    return table1_df
