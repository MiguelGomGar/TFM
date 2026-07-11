# ---- Load dependencies ----
source(here::here("src", "data_wrangling", "data_analysis.R"))

# ---- Missing values ----
#' Plot row-wise missingness distribution
#'
#' Computes the total number of missing values (NAs) per row (patient) directly
#' from a raw dataframe, aggregates their frequency counts, and renders a
#' professional distribution plot.
#'
#' @param df A data frame containing raw clinical features.
#' @param title Plot title string. Default is "Distribution of Missing Values per Patient".
#' @param subtitle Plot subtitle string.
#' @param x_label Character string for the horizontal axis title. Default is "Number of missing values".
#' @param y_label Character string for the vertical axis title. Default is "Number of Records".
#'
#' @return A ggplot2 object representing the publication-ready missingness distribution.
plot_row_missingness <- function(df,
                                 title = "Distribution of Missing Values per Patient",
                                 subtitle = "Analysis of row-wise missingness patterns across the PREDIMAR cohort",
                                 x_label = "Number of missing values",
                                 y_label = "Number of Records") {
    # Safety check for completely empty dataframe
    if (nrow(df) == 0) {
        message("The dataframe has no rows. Returning NULL.")
        return(NULL)
    }

    # 1. Compute missing values per row efficiently
    # Instead of attaching the count to the massive original dataframe,
    # we isolate the calculation into a lightweight new structure.
    na_summary <- data.frame(row_na_count = rowSums(is.na(df))) |>
        dplyr::count(row_na_count, name = "n_records") |>
        dplyr::mutate(
            pct_label = scales::percent(
                n_records / sum(n_records),
                accuracy = 0.1
            )
        ) |>
        dplyr::arrange(row_na_count)

    # 2. Build the distribution chart
    p <- ggplot2::ggplot(
        na_summary,
        ggplot2::aes(x = row_na_count, y = n_records)
    ) +

        # Main vertical bar layer
        ggplot2::geom_col(
            fill = "#2563eb",
            color = "#1e3a8a",
            alpha = 0.85,
            width = 0.75,
            linewidth = 0.4
        ) +

        # Add the percentage text on top of the columns
        ggplot2::geom_text(
            ggplot2::aes(label = pct_label),
            vjust = -0.6,
            size = 3.5,
            fontface = "bold",
            color = "#1e293b"
        ) +

        # Format vertical axis: Let ggplot calculate breaks dynamically,
        # but keep the expansion to prevent text clipping at the top.
        ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.18))
        ) +

        # Format horizontal axis: Force integer ticks starting from 0
        ggplot2::scale_x_continuous(
            breaks = function(x) seq(0, max(x, na.rm = TRUE), by = 1),
            expand = ggplot2::expansion(mult = c(0.02, 0.02))
        ) +

        # Customize titles and labels
        ggplot2::labs(
            title = title,
            subtitle = subtitle,
            x = x_label,
            y = y_label
        ) +

        # Customize theme
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(
                face = "bold", size = 13, margin = ggplot2::margin(b = 4)
            ),
            plot.subtitle = ggplot2::element_text(
                color = "#64748b", size = 9, margin = ggplot2::margin(b = 15)
            ),
            axis.title = ggplot2::element_text(
                face = "bold", color = "#1e293b"
            ),
            axis.text.y = ggplot2::element_text(
                color = "#475569", face = "bold"
            ),
            axis.text.x = ggplot2::element_text(
                color = "#475569", face = "bold"
            ),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(
                color = "#f1f5f9", linewidth = 0.5
            )
        )

    return(p)
}

#' Plot column-wise missingness distribution
#'
#' Calculates and plots the column-wise missingness distribution from a raw dataframe.
#' Saves the plot to a specified file path with the given dimensions and resolution.
#'
#' @param df A data frame containing raw clinical features.
#' @param title Plot title string.
#' @param subtitle Plot subtitle string.
#' @param x_label Character string for the horizontal axis title.
#' @param y_label Character string for the vertical axis title.
#'
#' @return A ggplot2 object representing the bar plot, or NULL if no NAs are found.
plot_column_missingness <- function(df,
                                    title = "Missing Data by Feature",
                                    subtitle = "Proportion of NAs per feature in the PREDIMAR cohort",
                                    x_label = "Missing Values Rate",
                                    y_label = NULL) {
    # 1. Calculate missing rate for each column directly from the raw data frame
    df_plot <- df |>
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.)) / dplyr::n())) |>
        tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "feature",
            values_to = "missing_rate"
        ) |>
        # 2. Filter out complete columns, arrange, and create a clean percentage label
        dplyr::filter(missing_rate > 0) |>
        dplyr::arrange(missing_rate) |>
        dplyr::mutate(
            feature = factor(feature, levels = feature),
            pct_label = scales::percent(missing_rate, accuracy = 0.1)
        )

    # Safety check: if there are no missing values at all, avoid crashing ggplot
    if (nrow(df_plot) == 0) {
        message("No missing values found in the dataframe. Returning NULL.")
        return(NULL)
    }

    # 3. Build the bar plot
    p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(x = missing_rate, y = feature)
    ) +
        ggplot2::geom_col(
            fill = "#2563eb",
            color = "#1e3a8a",
            alpha = 0.85,
            width = 0.75,
            linewidth = 0.4
        ) +

        # Threshold line at 25% missingness
        ggplot2::geom_vline(
            xintercept = 0.25,
            color = "#e11d48",
            linetype = "dashed",
            linewidth = 0.8
        ) +

        # Text annotation for the threshold line
        ggplot2::annotate(
            "text",
            x = 0.25,
            y = 1,
            label = "25%",
            vjust = -1,
            hjust = -0.1,
            color = "#e11d48",
            fontface = "italic",
            size = 3.5
        ) +

        # Label formatting as percentage with safety expansion to avoid clipping
        ggplot2::scale_x_continuous(
            labels = scales::percent_format(accuracy = 1),
            expand = ggplot2::expansion(mult = c(0, 0.15))
        ) +

        # Axis labels and titles
        ggplot2::labs(
            title = title,
            subtitle = subtitle,
            x = x_label,
            y = y_label
        ) +

        # Aesthetics
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(
                face = "bold", size = 13, margin = ggplot2::margin(b = 4)
            ),
            plot.subtitle = ggplot2::element_text(
                color = "#64748b", size = 9, margin = ggplot2::margin(b = 15)
            ),
            axis.title = ggplot2::element_text(
                face = "bold", color = "#1e293b"
            ),
            axis.text.y = ggplot2::element_text(
                color = "#475569", face = "bold"
            ),
            axis.text.x = ggplot2::element_text(
                color = "#475569", face = "bold"
            ),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(
                color = "#f1f5f9", linewidth = 0.5
            )
        )

    return(p)
}
# ---- Correlations ----
#' Plot correlation matrix heat map
#'
#' Automatically filters a data frame based on dtype, computes the
#' corresponding association matrix (Pearson r or Cramer's V), and
#' renders a clean, non-redundant lower-triangular ggplot2 heat map.
#'
#' @param df A data frame containing clinical features.
#' @param dtype Determines whether data is categorical ("cat") or numeric
#' ("num"). Default is "num".
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
                color = abs(Value) > 0.6
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

#' Plot Variance Inflation Factor (VIF) Diagnostics
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
    # 1. Clean tracking columns, drop unused factors, and force target to
    # numeric
    temp_data <- df |>
        dplyr::mutate(
            dplyr::across(tidyselect::all_of(target_var), as.numeric)
        ) |>
        droplevels()

    # 2. Extract complete cases to replicate lm()'s internal listwise row
    # deletion
    complete_cases_subset <- temp_data |>
        tidyr::drop_na()

    # 3. Dynamic filtration: Identify and drop invariant features within
    # complete cases to protect the linear model from the "contrasts can be
    # applied only to factors" crash.
    single_level_features <- complete_cases_subset |>
        dplyr::select(tidyselect::where(is.factor)) |>
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

    if (length(single_level_features) > 0) {
        cat(
            "\n[VIF Diagnostics] Automatically dropping columns that become
            invariant within complete cases:\n",
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
            dplyr::rename(VIF_Equivalent = 1) |>
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
            yintercept = 10,
            linetype = "dashed",
            color = "#ef4444",
            linewidth = 0.7
        ) +
        ggplot2::annotate(
            "text",
            x = 0.7,
            y = 10.2,
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

#---- Distributions ----

#' Custom x-axis label formatting function
#'
#' Dynamically checks if the input vector contains only integers and formats
#' them without decimal places, while safely falling back to standard formatting
#' for real continuous scales to prevent errors from non-integer values.
#'
#' @param x A numeric vector representing the x-axis tick values.
#'
#' @return A character vector of formatted labels, where integers are shown
#' without decimal places and non-integers are formatted with standard numeric
#' formatting.
format_axis_labels <- function(x) {
    x_clean <- x[!is.na(x)]
    if (length(x_clean) > 0 && all(x_clean == round(x_clean))) {
        return(sprintf("%d", as.integer(x)))
    } else {
        # Safe fallback formatting for real continuous scales
        return(format(x, trim = TRUE))
    }
}

#' Plot global numeric distribution
#'
#' Renders a professional multi-panel grid of histograms and density curves
#' for all numeric variables in the data frame to inspect global distributions.
#'
#' @param df A data frame containing numeric features.
#' @param title Plot title string. Default is "Numeric features distribution".
#' @param x_label Character string for the horizontal axis title. Default is
#' NULL.
#' @param y_label Character string for the vertical axis title. Default is
#'  "Count".
#' @param bins Number of bins for the histogram layer to avoid over-smoothing.
#' Default is 20.
#'
#' @return A ggplot2 object representing the faceted grid.
plot_global_numeric_distribution <- function(df,
                                             title = "Numeric features distribution",
                                             x_label = NULL,
                                             y_label = "Count",
                                             bins = 20) {
    # 1. Isolate strictly numeric columns automatically and pivot to long format
    df_long <- df |>
        dplyr::select(tidyselect::where(is.numeric)) |>
        tidyr::pivot_longer(
            cols = tidyselect::everything(),
            names_to = "feature",
            values_to = "value"
        )

    # 2. Build the continuous histogram
    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(count)),
            bins = bins,
            color = "white",
            fill = "#16a085",
            alpha = 0.4,
            na.rm = TRUE
        ) +

        # Custom x-axis label formatting to safely handle decimals and integers
        # dynamically
        ggplot2::scale_x_continuous(labels = format_axis_labels) +

        # Multi-panel wrapping with independent free axes
        ggplot2::facet_wrap(~feature, scales = "free", ncol = 4) +

        # Prevent ggplot from cutting off text labels at the plot borders
        ggplot2::coord_cartesian(clip = "off") +

        # Clinical theme adjustments for publication-ready styling
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(
                color = "#eaeded",
                linewidth = 0.4
            ),
            strip.background = ggplot2::element_rect(
                fill = "#f8f9f9",
                color = "#d5dbdb",
                linewidth = 0.5
            ),
            strip.text = ggplot2::element_text(
                face = "bold",
                color = "#2c3e50",
                size = 10
            ),
            axis.line.x = ggplot2::element_line(
                color = "#bdc3c7",
                linewidth = 0.6
            ),
            axis.text = ggplot2::element_text(color = "#34495e"),
            axis.title = ggplot2::element_text(
                face = "bold",
                color = "#2c3e50"
            )
        ) +

        # Customize titles
        ggplot2::labs(title = title, x = x_label, y = y_label)

    return(p)
}

#' Plot stratified numeric distribution
#'
#' Renders a professional multi-panel grid of boxplots for all numeric variables
#' stratified by a categorical target class.
#'
#' @param df A data frame containing numeric features and a target column.
#' @param target_var String name of the categorical variable to stratify by.
#' Default is "AF_recurrence".
#' @param title Plot title string.
#' @param x_label Character string for the horizontal axis title. Default is
#' NULL.
#' @param y_label Character string for the vertical axis title. Default matches
#' target_var.
#'
#' @return A ggplot2 object representing the faceted grid.
plot_stratified_numeric_distribution <- function(df,
                                                 target_var = "AF_recurrence",
                                                 title = paste0(
                                                     "Numeric features distribution stratified by ",
                                                     target_var
                                                 ),
                                                 x_label = NULL,
                                                 y_label = target_var) {
    # 1. Filter out metadata (like 'code') implicitly by keeping only the target
    # and strictly numeric columns, then pivot to long format.
    df_long <- df |>
        dplyr::select(
            tidyselect::any_of(target_var),
            tidyselect::where(is.numeric)
        ) |>
        tidyr::pivot_longer(
            cols = -tidyselect::any_of(target_var),
            names_to = "feature",
            values_to = "value"
        )

    # 2. Build the stratified multi-panel boxplot grid.
    p <- ggplot2::ggplot(
        df_long,
        ggplot2::aes(
            x = value,
            y = .data[[target_var]],
            fill = .data[[target_var]]
        )
    ) +
        ggplot2::geom_boxplot(
            alpha = 0.75,
            color = "#2c3e50",
            outlier.size = 1,
            outlier.alpha = 0.4,
            linewidth = 0.6,
            na.rm = TRUE
        ) +

        # Scale fill with a high-contrast palette for clinical stratification
        ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.8) +

        # Apply the pseudo-log transformation to safely handle wide clinical
        # ranges
        ggplot2::facet_wrap(~feature, scales = "free", ncol = 3) +

        # Apply your customized medical theme configuration
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            legend.position = "none",
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(
                color = "#eaeded",
                linewidth = 0.5
            ),
            strip.background = ggplot2::element_rect(
                fill = "#f8f9f9",
                color = "#d5dbdb",
                linewidth = 0.5
            ),
            strip.text = ggplot2::element_text(
                face = "bold",
                color = "#2c3e50",
                size = 10
            ),
            axis.line.x = ggplot2::element_line(
                color = "#bdc3c7",
                linewidth = 0.6
            ),
            axis.text = ggplot2::element_text(color = "#34495e"),
            axis.title = ggplot2::element_text(
                face = "bold",
                color = "#2c3e50"
            ),
            panel.spacing = ggplot2::unit(1.2, "lines")
        ) +

        # Customize titles
        ggplot2::labs(title = title, x = x_label, y = y_label)

    return(p)
}

#' Plot single global categorical distribution
#'
#' Renders a professional horizontal bar chart for a single
#' categorical or factor column in the dataframe.
#'
#' @param df A data frame containing categorical features.
#' @param var_name Character string specifying the name of the column to plot.
#' @param title Plot title string. Defaults to a dynamic title.
#' @param x_label Character string for the horizontal axis title. Default is NULL.
#' @param y_label Character string for the vertical axis title. Default is "Count".
#'
#' @return A ggplot2 object.
plot_single_categorical_distribution <- function(df,
                                                 var_name,
                                                 title = NULL,
                                                 x_label = NULL,
                                                 y_label = "Count") {
    # Dynamic title if none is provided
    if (is.null(title)) title <- paste("Distribution of", var_name)

    # 1. Isolate the target column, handle as character, and order by frequency
    df_plot <- df |>
        dplyr::select(value = tidyselect::all_of(var_name)) |>
        dplyr::mutate(value = as.character(value)) |>
        # Order factors based on absolute frequency from highest to lowest
        dplyr::mutate(value = forcats::fct_rev(forcats::fct_infreq(value)))

    # 2. Render the distribution plot
    p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(y = value, fill = value)
    ) +
        ggplot2::geom_bar(
            color = "#2c3e50",
            alpha = 0.8,
            width = 0.7,
            linewidth = 0.5
        ) +

        # Add the exact count labels just outside the bars
        ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(after_stat(count) / sum(after_stat(count)))),
            stat = "count",
            hjust = -0.2,
            size = 3.5,
            fontface = "bold",
            color = "#2c3e50"
        ) +

        # Apply the dark medical 'mako' palette from viridis
        ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +

        # Prevent ggplot from cutting off text labels at the plot borders
        ggplot2::coord_cartesian(clip = "off") +

        # Clinical theme adjustments
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            legend.position = "none",
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(
                color = "#f2f4f4",
                linewidth = 0.5
            ),
            panel.grid.major.y = ggplot2::element_blank(),
            axis.line.y = ggplot2::element_line(
                color = "#bdc3c7",
                linewidth = 0.6
            ),
            axis.text = ggplot2::element_text(
                color = "#34495e",
                face = "bold"
            ),
            axis.title = ggplot2::element_text(
                face = "bold",
                color = "#2c3e50"
            )
        ) +

        # Customize labels
        ggplot2::labs(title = title, x = x_label, y = y_label)

    return(p)
}

#' Plot single stratified categorical distribution
#'
#' Renders a professional absolute stacked bar chart for a single
#' categorical feature, where the Y-axis displays absolute counts
#' and internal bar labels dynamically show the relative percentage.
#'
#' @param df A data frame containing categorical features and a target column.
#' @param var_name Character string specifying the name of the column to plot.
#' @param target_var String name of the categorical variable to stratify and
#' color by. Default is "AF_recurrence".
#' @param title Plot title string. Defaults to a dynamic title.
#' @param x_label Character string for the horizontal axis title. Default is NULL.
#' @param y_label Character string for the vertical axis title. Default is NULL.
#' @param legend_title Character string for the legend title. Defaults to target_var.
#'
#' @return A ggplot2 object.
plot_single_stratified_categorical <- function(df,
                                               var_name,
                                               target_var = "AF_recurrence",
                                               title = NULL,
                                               x_label = NULL,
                                               y_label = NULL,
                                               legend_title = NULL) {
    # Dynamic titles if not provided
    if (is.null(title)) {
        title <- paste("Distribution of", var_name, "stratified by", target_var)
    }
    if (is.null(legend_title)) {
        legend_title <- target_var
    }

    # 1. Isolate target and the specific factor, compute absolute counts and
    # within-bar percentages
    df_plot <- df |>
        dplyr::select(
            value = tidyselect::all_of(var_name),
            target = tidyselect::all_of(target_var)
        ) |>
        # Clean out missing feature entries to keep the bars structurally stable
        dplyr::filter(!is.na(value)) |>
        dplyr::mutate(value = as.character(value)) |>
        # Reorder predictor levels based on their overall frequency counts
        dplyr::mutate(value = forcats::fct_infreq(value)) |>
        # Pre-aggregate frequencies and compute localized percentage strings
        dplyr::group_by(value, target) |>
        dplyr::summarise(n_records = dplyr::n(), .groups = "drop_last") |>
        dplyr::mutate(
            pct = n_records / sum(n_records),
            pct_label = scales::percent(pct, accuracy = 0.1)
        ) |>
        dplyr::ungroup()

    # 2. Generate the absolute stacked bar chart with relative labels
    p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(
            x = value,
            y = n_records,
            fill = target
        )
    ) +

        # Add absolute stacked bars
        ggplot2::geom_col(
            color = "#2c3e50",
            position = "stack",
            alpha = 0.85,
            width = 0.7,
            linewidth = 0.5
        ) +

        # Add text relative labels
        ggplot2::geom_text(
            ggplot2::aes(label = pct_label),
            position = ggplot2::position_stack(vjust = 0.5),
            size = 3.5,
            fontface = "bold",
            color = "white"
        ) +

        # Expand scales to prevent label clipping and ensure readability
        ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.08))
        ) +

        # Apply professional high-contrast palette for clinical stratification
        ggplot2::scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) +

        # Flip the axes for horizontal reading
        ggplot2::coord_flip() +

        # Clinical theme adjustments
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            legend.position = "top",
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(
                color = "#eaeded",
                linewidth = 0.5
            ),
            axis.line.x = ggplot2::element_line(
                color = "#bdc3c7",
                linewidth = 0.6
            ),
            axis.text.x = ggplot2::element_text(
                color = "#34495e",
                face = "bold"
            ),
            axis.text.y = ggplot2::element_text(
                color = "#34495e",
                face = "bold"
            ),
            axis.title = ggplot2::element_text(
                face = "bold",
                color = "#2c3e50"
            )
        ) +

        # Customize labels
        ggplot2::labs(
            title = title,
            x = x_label,
            y = y_label,
            fill = legend_title
        )

    return(p)
}
