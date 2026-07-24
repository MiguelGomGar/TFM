import pandas as pd
import numpy as np
import scipy.stats as stats
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm


def compute_num_corr_matrix(df):
    """
    Compute numeric correlation matrix for all numeric columns in the dataframe.
    """
    numeric_df = df.select_dtypes(include=[np.number])
    if numeric_df.empty:
        return pd.DataFrame()
    return numeric_df.corr(method="spearman")


def compute_cramers_v(x, y):
    """
    Compute Cramer's V for two categorical columns.
    """
    contingency_table = pd.crosstab(x, y)
    n_samples = contingency_table.to_numpy().sum()
    n_rows, n_cols = contingency_table.shape

    if n_samples == 0 or min(n_rows - 1, n_cols - 1) == 0:
        return 0.0

    try:
        chi2, _, _, _ = stats.chi2_contingency(contingency_table, correction=False)
    except Exception:
        return 0.0

    v_value = np.sqrt(chi2 / (n_samples * min(n_rows - 1, n_cols - 1)))
    return float(v_value)


def compute_cat_corr_matrix(df):
    """
    Compute categorical correlation matrix for all categorical columns in the dataframe.
    """
    cat_df = df.select_dtypes(include=["category", "object", "bool"])
    feature_names = list(cat_df.columns)
    n_features = len(feature_names)

    if n_features < 2:
        raise ValueError(
            "The provided dataframe must contain at least 2 categorical columns."
        )

    # Initialize a symmetric matrix filled with 1.0 on the diagonal
    corr_matrix = pd.DataFrame(1.0, index=feature_names, columns=feature_names)

    # Automate pairwise extraction using nested loops
    for i in range(n_features - 1):
        for j in range(i + 1, n_features):
            col_i = feature_names[i]
            col_j = feature_names[j]
            association_score = compute_cramers_v(cat_df[col_i], cat_df[col_j])
            corr_matrix.loc[col_i, col_j] = association_score
            corr_matrix.loc[col_j, col_i] = association_score

    return corr_matrix


def plot_corr_matrix(df, dtype="Numeric", threshold=0.5, axis_ticks_size=14):
    """
    Plot correlation matrix heatmap (lower triangular masked equivalent).
    """
    if dtype == "Numeric":
        matrix_data = compute_num_corr_matrix(df)
        if matrix_data.empty:
            return None
        vmin, vmax = -1.0, 1.0
        from matplotlib.colors import LinearSegmentedColormap

        cmap = LinearSegmentedColormap.from_list(
            "custom_num", ["#3182bd", "white", "#de2d26"]
        )
        cbar_label = "s"
    elif dtype == "Categorical":
        matrix_data = compute_cat_corr_matrix(df)
        vmin, vmax = 0.0, 1.0
        from matplotlib.colors import LinearSegmentedColormap

        cmap = LinearSegmentedColormap.from_list(
            "custom_cat", ["#fff5f5", "#fecaca", "#ef4444", "#7f1d1d"]
        )
        cbar_label = "V"
    else:
        raise ValueError(
            "Invalid dtype parameter. Please specify either 'Numeric' or 'Categorical'."
        )

    # Mask the upper triangle and keep the lower triangle visible.
    # The diagonal is hidden so the matrix looks clean and focused on pairwise
    # relationships between distinct features.
    mask = np.triu(np.ones_like(matrix_data, dtype=bool))

    plt.figure(figsize=(10, 8))

    # Custom annotations to only show values >= threshold
    annot_mask = (np.abs(matrix_data) >= threshold) & (~mask)
    annot_labels = matrix_data.map(lambda v: f"{v:.1f}" if not pd.isna(v) else "")
    annot_labels = np.where(annot_mask, annot_labels, "")

    ax = sns.heatmap(
        matrix_data,
        mask=mask,
        vmin=vmin,
        vmax=vmax,
        cmap=cmap,
        annot=annot_labels,
        fmt="",
        annot_kws={"size": 10, "weight": "bold"},
        linewidths=0.4,
        linecolor="#e2e8f0",
        square=True,
        cbar_kws={"label": cbar_label, "shrink": 0.8},
    )

    # Text color styling: white on high values, dark blue on low values
    for text in ax.texts:
        if text.get_text():
            try:
                val = float(text.get_text())
                if abs(val) > threshold:
                    text.set_color("white")
                else:
                    text.set_color("#2c3e50")
            except ValueError:
                text.set_color("#2c3e50")

    plt.title(
        f"{dtype} Correlation Matrix",
        fontsize=16,
        fontweight="bold",
        pad=15,
        loc="left",
    )
    plt.xticks(
        rotation=45,
        ha="right",
        fontsize=axis_ticks_size,
        fontweight="bold",
        color="#34495e",
    )
    plt.yticks(
        rotation=45,
        va="top",
        fontsize=axis_ticks_size,
        fontweight="bold",
        color="#34495e",
    )
    plt.tight_layout()
    return plt.gcf()


def plot_vif(
    df,
    threshold=5.0,
    target_var="AF_recurrence",
    title="VIF Diagnostics",
    x_label="VIF / GVIF^2",
    y_label=None,
):
    """
    Computes Generalized Variance Inflation Factor (GVIF) equivalent VIF for each feature,
    and plots a professional horizontal bar chart.
    """
    temp_data = df.copy()
    if target_var in temp_data.columns:
        temp_data[target_var] = pd.to_numeric(temp_data[target_var], errors="coerce")

    complete_cases_subset = temp_data.dropna()

    # Identify invariant columns (nunique < 2) in complete cases
    single_level_features = []
    for col in complete_cases_subset.columns:
        if col != target_var and complete_cases_subset[col].nunique() < 2:
            single_level_features.append(col)

    if single_level_features:
        print(
            f"\n[VIF Diagnostics] Automatically dropping columns that become invariant within complete cases:\n{', '.join(single_level_features)}\n"
        )
        temp_data = temp_data.drop(columns=single_level_features)

    # Split predictors and dummy-encode them
    if target_var in temp_data.columns:
        X_df = temp_data.drop(columns=[target_var]).dropna()
    else:
        X_df = temp_data.dropna()

    if X_df.empty:
        print("Warning: No complete cases available for VIF calculation.")
        return None

    # Compute GVIF for each original feature
    feature_groups = {}
    encoded_dfs = []

    for col in X_df.columns:
        # Check if column is categorical/object/bool
        if X_df[col].dtype.name in ["category", "object", "bool"] or not np.issubdtype(
            X_df[col].dtype, np.number
        ):
            dummies = pd.get_dummies(
                X_df[col], prefix=col, drop_first=True, dtype=float
            )
            feature_groups[col] = list(dummies.columns)
            encoded_dfs.append(dummies)
        else:
            feature_groups[col] = [col]
            encoded_dfs.append(X_df[[col]].astype(float))

    X_encoded = pd.concat(encoded_dfs, axis=1)
    corr_all = X_encoded.corr().to_numpy()
    cols_all = list(X_encoded.columns)

    vif_dict = {}
    for col, group in feature_groups.items():
        if not group:
            continue
        idx_group = [cols_all.index(c) for c in group]
        idx_other = [i for i, c in enumerate(cols_all) if c not in group]

        if not idx_other:
            vif_dict[col] = 1.0
            continue

        if len(group) == 1:
            dep = group[0]
            indep = [c for c in cols_all if c != dep]
            try:
                X_indep = sm.add_constant(X_encoded[indep])
                model = sm.OLS(X_encoded[dep], X_indep).fit()
                r2 = model.rsquared
                vif = 1.0 / (1.0 - r2) if r2 < 1.0 else 999.0
            except Exception:
                vif = 1.0
            vif_dict[col] = vif
        else:
            try:
                R_group = X_encoded[group].corr().to_numpy()
                other_cols = [cols_all[i] for i in idx_other]
                R_other = X_encoded[other_cols].corr().to_numpy()

                det_group = np.linalg.det(R_group)
                det_other = np.linalg.det(R_other)
                det_all = np.linalg.det(corr_all)

                if det_all == 0 or np.isnan(det_all):
                    # Add ridge to diagonal if singular
                    ridge = 1e-5 * np.eye(corr_all.shape[0])
                    det_all = np.linalg.det(corr_all + ridge)
                    det_group = np.linalg.det(R_group + 1e-5 * np.eye(R_group.shape[0]))
                    det_other = np.linalg.det(R_other + 1e-5 * np.eye(R_other.shape[0]))
                gvif = (det_group * det_other) / det_all if det_all != 0 else 1.0
                vif_dict[col] = float(np.sqrt(abs(gvif)))
            except Exception:
                vif_dict[col] = 1.0

    vif_series = pd.Series(vif_dict).sort_values(ascending=False)

    plt.figure(figsize=(10, max(6, len(vif_series) * 0.35)))
    sns.barplot(x=vif_series.values, y=vif_series.index, color="#2563eb")
    plt.axvline(threshold, linestyle="--", color="#e11d48", linewidth=1.2)
    plt.title(title, fontsize=13, fontweight="bold", pad=20, loc="left")
    plt.xlabel(x_label, fontsize=11, fontweight="bold", color="#1e293b", labelpad=10)
    if y_label:
        plt.ylabel(y_label, fontsize=11, fontweight="bold", color="#1e293b")
    plt.tight_layout()
    return plt.gcf()
