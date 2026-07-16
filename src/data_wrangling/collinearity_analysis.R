# ---- Compute statistics ----
#' Compute numeric correlation matrix
#'
#' Compute a correlation matrix for all numeric columns in the dataframe.
#'
#' @param df A data frame containing numeric columns.
#'
#' @return A correlation matrix (data frame) with pairwise correlations between
#' numeric features.
compute_num_corr_matrix <- function(df) {
    numeric_df <- df |>
        dplyr::select(tidyselect::where(is.numeric))

    correlation_matrix <- stats::cor(
        numeric_df,
        method = "spearman",
        use = "pairwise.complete.obs"
    )

    return(correlation_matrix)
}

#' Compute Cramer's V
#'
#' Compute Cramer's V for two vectors.
#'
#' @param x A categorical variable (factor or character).
#' @param y Another categorical variable (factor or character).
#'
#' @return A numeric value representing Cramer's V, which ranges from 0 to 1.
compute_cramers_v <- function(x, y) {
    contingency_table <- table(x, y)
    chi2_test <- stats::chisq.test(
        contingency_table,
        correct = FALSE
    )

    n_samples <- sum(contingency_table)
    n_rows <- nrow(contingency_table)
    n_cols <- ncol(contingency_table)

    if (n_samples == 0 || min(n_rows - 1, n_cols - 1) == 0) {
        return(0)
    }

    v_value <- sqrt(
        chi2_test$statistic / (n_samples * min(n_rows - 1, n_cols - 1))
    )

    return(as.numeric(v_value))
}

#' Compute categorical correlation matrix
#'
#' Compute a correlation matrix for all the categorical columns in the data
#' frame.
#'
#' @param df A data frame containing categorical columns.
#'
#' @return A correlation matrix (data frame) with pairwise correlations between
#' categorical features.
compute_cat_corr_matrix <- function(df) {
    cat_df <- df |>
        dplyr::select(tidyselect::where(is.factor))
    feature_names <- colnames(cat_df)
    n_features <- length(feature_names)

    # Safety check if there are not enough categorical features
    if (n_features < 2) {
        stop("The provided dataframe must contain at least 2 categorical
        columns.")
    }

    # Initialize a symmetric matrix filled with 1.0 on the diagonal
    matrix <- matrix(
        1.0,
        nrow = n_features,
        ncol = n_features,
        dimnames = list(feature_names, feature_names)
    )

    # Automate pairwise extraction using nested loops
    for (i in 1:(n_features - 1)) {
        for (j in (i + 1):n_features) {
            association_score <- compute_cramers_v(cat_df[[i]], cat_df[[j]])
            matrix[i, j] <- association_score
            matrix[j, i] <- association_score
        }
    }

    return(matrix)
}

# ---- Plots ----
#' Plot correlation matrix heat map
#'
#' Automatically filters a data frame based on dtype, computes the corresponding
#' association matrix (Pearson r or Cramer's V), and renders a lower-triangular
#' heat map.
#'
#' @param df A data frame containing clinical features.
#' @param dtype Determines whether data is categorical ("cat") or numeric ("num").
#' Default is "num".
#' @param title Plot title string. Default is NULL.
#' @param threshold Absolute value threshold to display numerical labels.
#' Default is 0.4.
#'
#' @return A ggplot2 heatmap object.
plot_corr_matrix <- function(df,
                             dtype = "num",
                             title = NULL,
                             threshold = 0.5) {
    if (dtype == "num") {
        flag <- "numeric"

        # Compute numeric matrix using your helper function
        matrix_data <- compute_num_corr_matrix(df)

        # Customize column name and scale range
        coefficient <- "r"
        limits <- c(-1, 1)

        # Customize color scheme for numeric data
        fill_scale <- ggplot2::scale_fill_gradient2(
            low = "#3182bd",
            mid = "white",
            high = "#de2d26",
            midpoint = 0,
            limits = limits,
            name = coefficient
        )
    } else if (dtype == "cat") {
        flag <- "categorical"

        # Compute categorical matrix using your helper function
        matrix_data <- compute_cat_corr_matrix(df)

        # Customize column name and scale range
        coefficient <- "V"
        limits <- c(0, 1)

        # Customize color scheme for categorical data
        fill_scale <- ggplot2::scale_fill_gradientn(
            colors = c("#fff5f5", "#fecaca", "#ef4444", "#7f1d1d"),
            limits = limits,
            name = coefficient
        )
    } else {
        stop("Invalid dtype parameter. Please specify either 'num' or 'cat'.")
    }

    # Compute the coefficient's midpoint for perfect color balancing
    midpoint <- mean(limits)

    # Mask the upper triangle and diagonal to accurately preserve the LOWER
    # triangle staircase shape requested for the final report.
    matrix_data[lower.tri(matrix_data, diag = TRUE)] <- NA

    # Save the feature names in their original structural column order
    feature_order <- colnames(matrix_data)

    # Reshape the square matrix into a clean long format data frame
    df_long <- as.data.frame(matrix_data) |>
        tibble::rownames_to_column(var = "Feature_1") |>
        tidyr::pivot_longer(
            cols = -Feature_1,
            names_to = "Feature_2",
            values_to = "Value"
        ) |>
        dplyr::filter(!is.na(Value)) |>
        dplyr::mutate(
            Feature_1 = factor(Feature_1, levels = feature_order),
            Feature_2 = factor(Feature_2, levels = rev(feature_order))
        )

    # Render the triangular heat map
    p <- ggplot2::ggplot(
        df_long,
        ggplot2::aes(x = Feature_1, y = Feature_2, fill = Value)
    ) +

        # Add the tiles
        ggplot2::geom_tile(color = "#e2e8f0", linewidth = 0.4) +

        # Inject the previously defined scale
        fill_scale +

        # Add text labels
        ggplot2::geom_text(
            data = df_long |>
                dplyr::filter(abs(Value) >= threshold),
            ggplot2::aes(
                label = sprintf("%.2f", Value),
                color = abs(Value) > threshold
            ),
            size = 3.5,
            fontface = "bold",
            show.legend = FALSE
        ) +

        # Ensures text is white on dark cells and dark-blue on light cells for
        # reading safety
        ggplot2::scale_color_manual(
            values = c("TRUE" = "white", "FALSE" = "#2c3e50")
        ) +

        # Customization
        ggplot2::labs(title = title, x = NULL, y = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(
                face = "bold",
                size = 16,
                margin = ggplot2::margin(b = 10)
            ),
            axis.text.x = ggplot2::element_text(
                angle = 45,
                hjust = 1,
                face = "bold",
                color = "#34495e"
            ),
            axis.text.y = ggplot2::element_text(
                face = "bold",
                color = "#34495e"
            ),
            panel.grid = ggplot2::element_blank()
        )

    return(p)
}

#' Plot Variance Inflation Factor for each feature
#'
#' Computes the Generalized Variance Inflation Factor (GVIF) for a mixed
#' data set, safely handles invariant features within complete cases, scales it
#' back to a standard VIF equivalent, and renders a professional horizontal bar
#' chart.
#'
#' @param df A data frame containing both numeric features and categorical
#' factors.
#' @param target_var Character string specifying the dependent variable to
#' predict. Default is "AF_recurrence".
#' @param title Character string for the plot title. Default is "VIF
#' Diagnostics".
#' @param x_label Character string for the horizontal axis title. Default is
#' "VIF / GVIF^2".
#' @param y_label Character string for the vertical axis title. Defaults to NULL
#' for no label.
#'
#' @return A ggplot2 object representing the publication-ready VIF bar chart.
plot_vif <- function(df,
                     target_var = "AF_recurrence",
                     title = "VIF Diagnostics",
                     x_label = "VIF / GVIF^2",
                     y_label = NULL) {
    # 1. Clean tracking columns, drop unused factors, and force target to numeric
    temp_data <- df |>
        dplyr::mutate(
            dplyr::across(tidyselect::all_of(target_var), as.numeric)
        ) |>
        droplevels()

    # 2. Extract complete cases to replicate lm()'s internal listwise row deletion
    complete_cases_subset <- temp_data |>
        tidyr::drop_na()

    # 3. Dynamic filtration: Identify and drop invariant features within complete cases
    # Extract only factor columns first
    factor_cols <- complete_cases_subset |>
        dplyr::select(tidyselect::where(is.factor))

    # Initialize as empty vector
    single_level_features <- character(0)

    # Only attempt to pivot and summarize if there is at least ONE factor column
    if (ncol(factor_cols) > 0) {
        single_level_features <- factor_cols |>
            dplyr::summarise(
                dplyr::across(tidyselect::everything(), ~ dplyr::n_distinct(.))
            ) |>
            tidyr::pivot_longer(
                cols = tidyselect::everything(),
                names_to = "feature",
                values_to = "unique_levels"
            ) |>
            dplyr::filter(unique_levels < 2) |>
            dplyr::pull(feature)
    }

    if (length(single_level_features) > 0) {
        cat(
            "\n[VIF Diagnostics] Automatically dropping columns that become invariant within complete cases:\n",
            paste(single_level_features, collapse = ", "), "\n\n"
        )
        temp_data <- temp_data |>
            dplyr::select(-tidyselect::all_of(single_level_features))
    }

    # 4. Fit the linear regression model dynamically
    formula_string <- paste(target_var, "~ .")
    regression_model <- stats::lm(
        stats::as.formula(formula_string),
        data = temp_data
    )

    # 5. Compute VIF and process structure safely based on feature data types
    vif_raw_output <- car::vif(regression_model) |>
        as.data.frame()

    if ("GVIF^(1/(2*Df))" %in% colnames(vif_raw_output)) {
        vif_df <- vif_raw_output |>
            tibble::rownames_to_column(var = "feature") |>
            dplyr::rename(Adjusted_GVIF = `GVIF^(1/(2*Df))`) |>
            dplyr::mutate(VIF_Equivalent = Adjusted_GVIF^2) |>
            dplyr::select(feature, VIF_Equivalent)
    } else {
        vif_df <- vif_raw_output |>
            tibble::rownames_to_column(var = "feature") |>
            dplyr::rename(VIF_Equivalent = 2) |>
            dplyr::select(feature, VIF_Equivalent)
    }

    # 6. Build the professional horizontal diagnostics visualization
    p <- vif_df |>
        ggplot2::ggplot(
            ggplot2::aes(
                x = stats::reorder(feature, VIF_Equivalent),
                y = VIF_Equivalent
            )
        ) +
        ggplot2::geom_bar(
            stat = "identity",
            fill = "#3b82f6",
            alpha = 0.85,
            width = 0.7
        ) +

        # Establish clinical collinearity reference lines
        ggplot2::geom_hline(
            yintercept = 5,
            linetype = "dashed",
            color = "#ef4444",
            linewidth = 0.7
        ) +
        ggplot2::annotate(
            "text",
            x = 0.7,
            y = 5.2,
            color = "#b91c1c",
            size = 3,
            fontface = "italic",
            hjust = 0,
            label = "Severe collinearity"
        ) +

        # Horizontal orientation
        ggplot2::coord_flip(clip = "off") +
        ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.15))
        ) +

        # Titles and design setup
        ggplot2::labs(title = title, x = y_label, y = x_label) +

        # Customization
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(
                face = "bold",
                size = 14,
                margin = ggplot2::margin(b = 4)
            ),
            plot.subtitle = ggplot2::element_text(
                color = "#64748b",
                size = 9,
                margin = ggplot2::margin(b = 15)
            ),
            axis.text.y = ggplot2::element_text(
                face = "bold",
                color = "#334155"
            ),
            axis.text.x = ggplot2::element_text(color = "#475569"),
            axis.title.x = ggplot2::element_text(
                face = "bold",
                color = "#1e293b",
                margin = ggplot2::margin(t = 10)
            ),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(
                color = "#f1f5f9",
                linewidth = 0.5
            )
        )

    return(p)
}
