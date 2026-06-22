# ---- Save plots ----

#' Save ggplot2 object to file
#' 
#' Saves a ggplot2 object to a specified file path with the given dimensions and 
#' resolution.
#' 
#' @param plot A ggplot2 object to be saved.
#' @param file_name Name of the file to save the plot to.
#' @param width Width of the saved plot.
#' @param height Height of the saved plot.
#' @param dpi DPI of the saved plot.
#' 
#' @return NULL. The function saves the plot to the specified file path.
save_plot <- function(plot, file_name, width = 12, height = 5, dpi = 300) {
    
    file_path <- here::here("results", "eda", file_name)

    ggplot2::ggsave(
        filename = file_path,
        plot = plot,
        width = width,
        height = height,
        dpi = dpi
    )
}

# ---- Missing values ----

#' Plot stratified missingness rate
#'
#' Renders a bar chart analyzing the proportion and absolute count of missing 
#' values (NAs) stratified by a target class. Safely handles mixed data types 
#' (factors and numerics) during the pivoting phase.
#'
#' @param df A data frame containing clinical features.
#' @param target_var Character string specifying the categorical variable to 
#' stratify by. Default is "AF_recurrence".
#' @param title Character string for the plot title. Default is "Missing Data 
#' Diagnostic".
#' @param subtitle Character string for the plot subtitle.
#' @param x_label Character string for the horizontal axis title.
#' @param y_label Character string for the vertical axis title. Default matches 
#' target_var.
#'
#' @return A ggplot2 object representing the faceted missingness grid.
plot_stratified_missingness <- function(
        df, 
        target_var = "AF_recurrence",
        title = "Missing Data Diagnostic",
        subtitle = "Proportion and absolute count of missing values evaluated 
        within each patient record",
        x_label = "Missingness Rate within Stratum (%)",
        y_label = target_var) {
    
    # 1. Isolate the target column and any predictors containing at least one NA
    df_missing_analysis <- df |> 
        dplyr::select(
            tidyselect::all_of(target_var), tidyselect::where(~ any(is.na(.)))
            ) |> 
        tidyr::pivot_longer(
            cols = -tidyselect::all_of(target_var),
            names_to = "feature",
            values_to = "value",
            values_transform = list(value = as.character)
        ) |> 
        
        # Calculate the missingness rate inside each stratum dynamically
        dplyr::group_by(feature, .data[[target_var]]) |> 
        dplyr::summarise(
            na_count = sum(is.na(value)),
            group_total = dplyr::n(),
            na_rate = na_count / group_total,
            .groups = "drop"
        )
    
    # Safety check in case no columns contain missing values
    if (nrow(df_missing_analysis) == 0) {
        stop("The provided dataframe does not contain any missing values (NAs) 
        to analyze.")
    }
    
    # 2. Render the faceted bar chart
    p <- ggplot2::ggplot(
        df_missing_analysis, 
        ggplot2::aes(
            x = na_rate, 
            y = .data[[target_var]], 
            fill = .data[[target_var]]
            )
        ) +
        ggplot2::geom_col(
            color = "#2c3e50", 
            alpha = 0.85, 
            width = 0.6, 
            linewidth = 0.5
            ) +
        
        # Add text labels
        ggplot2::geom_text(
            ggplot2::aes(label = sprintf("%.1f%%", na_rate * 100)),
            hjust = -0.1,
            size = 2.8,
            fontface = "bold",
            color = "#1e293b",
            lineheight = 0.85
        ) +
        
        # Format vertical axis as standard percentage with safety overhead space
        ggplot2::scale_x_continuous(
            labels = scales::label_percent(),
            expand = ggplot2::expansion(mult = c(0, 0.25))
        ) +
        
        # High-contrast medical palette
        ggplot2::scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) +
        
        # Facet grid
        ggplot2::facet_wrap(~ feature, ncol = 3) +
        
        # Set labels
        ggplot2::labs(
            title = title,
            subtitle = paste(subtitle, target_var, "stratum"),
            x = x_label,
            y = y_label
        ) +
        
        # Customize
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
                size = 9
            ),
            axis.line.x = ggplot2::element_line(
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
            ),
            panel.spacing = ggplot2::unit(1.5, "lines")
        )

    return(p)
}

#' Plot row-wise missingness distribution
#'
#' Computes the total number of missing values (NAs) per row (patient),
#' aggregates their frequency counts, and renders a professional distribution 
#' plot.
#' Y-axis displays absolute counts for sample-size transparency, while 
#' bar labels dynamically show relative percentages for clinical impact 
#' assessment.
#'
#' @param df A data frame containing clinical features.
#' @param title Plot title string. Default is "Distribution of Missing Values 
#' per Patient".
#' @param subtitle Plot subtitle string.
#' @param x_label Character string for the horizontal axis title. Default is 
#' "Number of missing values".
#' @param y_label Character string for the vertical axis title. Default is 
#' "Number of Records".
#'
#' @return A ggplot2 object representing the publication-ready missingness 
#' distribution.
plot_row_missingness <- function(
        df,
        title = "Distribution of Missing Values per Patient",
        subtitle = "Analysis of row-wise missingness patterns across the 
        PREDIMAR cohort",
        x_label = "Number of missing values",
        y_label = "Number of Records"
) {
    
    # 1. Compute missing values per row and aggregate metrics cleanly
    na_summary <- df |> 
        dplyr::mutate(row_na_count = rowSums(is.na(df))) |> 
        dplyr::count(row_na_count, name = "n_records") |>
        dplyr::mutate(
            pct_label = scales::percent(
                n_records / sum(n_records), accuracy = 0.1
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
            size = 3.0,         
            fontface = "bold",  
            color = "#1e293b"   
        ) +
        
        # Format vertical axis to eliminate lower floating and add safety room 
        # for text strings
        ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.18))
            ) +
        ggplot2::scale_x_continuous(
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
                face = "bold", 
                size = 13, 
                margin = ggplot2::margin(b = 4)
                ),
            plot.subtitle = ggplot2::element_text(
                color = "#64748b", 
                size = 9, 
                margin = margin(b = 15)
                ),
            axis.title = ggplot2::element_text(
                face = "bold", 
                color = "#1e293b"
                ),
            axis.text.y = ggplot2::element_text(
                color = "#475569", 
                face = "bold"
                ),
            axis.text.x = ggplot2::element_text(
                color = "#475569", 
                face = "bold", 
                hjust = 1
                ),
            axis.line.x = ggplot2::element_line(
                color = "#bdc3c7", 
                linewidth = 0.6
                ),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(), 
            panel.grid.major.y = ggplot2::element_line(
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
plot_global_numeric_distribution <- function(
        df, 
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
        ggplot2::facet_wrap(~ feature, scales = "free", ncol = 4) +
        
        # Prevent ggplot from cutting off text labels at the plot borders
        ggplot2::coord_cartesian(clip = "off") +
        
        # Clinical theme adjustments for publication-ready styling
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(
                color = "#eaeded", 
                linewidth = 0.4),
            strip.background = ggplot2::element_rect(
                fill = "#f8f9f9", 
                color = "#d5dbdb", 
                linewidth = 0.5),
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
plot_stratified_numeric_distribution <- function(
        df, 
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
            )) +
        ggplot2::geom_boxplot(
            alpha = 0.75, 
            color = "#2c3e50", 
            outlier.size = 1, 
            outlier.alpha = 0.4,
            linewidth = 0.6,
            na.rm = TRUE
        ) +
        
        # Scale fill with a high-contrast palette for clinical stratification
        ggplot2::scale_fill_manual(values = c("#16A085", "#2C3E50")) +
        
        # Apply the pseudo-log transformation to safely handle wide clinical 
        # ranges
        ggplot2::facet_wrap(~ feature, scales = "free", ncol = 3) +
        
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

#' Plot global categorical distribution
#'
#' Renders a professional multi-panel grid of horizontal bar charts 
#' for all categorical and factor columns in the dataframe.
#'
#' @param df A data frame containing categorical features.
#' @param title Plot title string. Default is "Categorical features 
#' distribution".
#' @param x_label Character string for the horizontal axis title. Default is 
#' NULL.
#' @param y_label Character string for the vertical axis title. Default is 
#' "Count".
#'
#' @return A ggplot2 object representing the faceted grid.
plot_global_categorical_distribution <- function(
        df, 
        title = "Categorical features distribution", 
        x_label = NULL, 
        y_label = "Count") {
    
    # 1. Isolate factor columns, exclude metadata, and pivot to long format
    df_long <- df |> 
        dplyr::select(tidyselect::where(is.factor)) |> 
        tidyr::pivot_longer(
            cols = tidyselect::everything(),
            names_to = "feature",
            values_to = "value",
            values_transform = list(value = as.character)
        ) |> 
        # Order factors based on absolute frequency from highest to lowest
        dplyr::mutate(value = forcats::fct_rev(forcats::fct_infreq(value)))
    
    # Safety check if there are no categorical features to display
    if (ncol(df_long) == 0) {
        stop("The provided dataframe does not contain any factor or character 
        columns.")
    }
    
    # 2. Render the faceted categorical distribution plot
    p <- ggplot2::ggplot(
        df_long, 
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
            ggplot2::aes(label = ggplot2::after_stat(count)),
            stat = "count",
            hjust = -0.2,            
            size = 3.2,
            fontface = "bold",
            color = "#2c3e50"
        ) +
        
        # Apply the dark medical 'mako' palette from viridis
        ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
        
        # Multi-panel grid wrapper with independent vertical axis scales
        ggplot2::facet_wrap(~ feature, scales = "free_y", ncol = 3) +
        
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
            ),
            panel.spacing = ggplot2::unit(1.5, "lines")
        ) + 
        
        # Customize labels
        ggplot2::labs(title = title, x = x_label, y = y_label)

    return(p)
}

#' Plot stratified categorical distribution
#'
#' Renders a professional multi-panel grid of absolute stacked bar charts
#' for all categorical features, where the Y-axis displays absolute counts
#' and internal bar labels dynamically show the relative percentage.
#'
#' @param df A data frame containing categorical features and a target column.
#' @param target_var String name of the categorical variable to stratify and 
#' color by. Default is "AF_recurrence".
#' @param title Plot title string.
#' @param x_label Character string for the horizontal axis title. Default is 
#' NULL.
#' @param y_label Character string for the vertical axis title. Default is NULL.
#' @param legend_title Character string for the legend title. Defaults to 
#' target_var if NULL.
#'
#' @return A ggplot2 object representing the faceted grid.
plot_stratified_categorical_distribution <- function(
        df, 
        target_var = "AF_recurrence", 
        title = paste0(
            "Categorical features distribution stratified by ",
            target_var
        ),
        x_label = NULL, 
        y_label = NULL,
        legend_title = NULL) {
    
    # Set the legend title to match the variable name if no custom label is 
    # provided
    if (is.null(legend_title)) {legend_title <- target_var}
    
    # 1. Isolate target and factors, compute absolute counts and within-bar 
    # percentages
    df_long <- df |> 
        dplyr::select(
            tidyselect::any_of(target_var), 
            tidyselect::where(is.factor)
            ) |> 
        
        # Pivot all columns to long format EXCEPT the target stratification 
        # variable
        tidyr::pivot_longer(
            cols = -tidyselect::any_of(target_var),
            names_to = "feature",
            values_to = "value",
            values_transform = list(value = as.character)
        ) |> 
        
        # Clean out missing feature entries to keep the bars structurally stable
        dplyr::filter(!is.na(value)) |> 
        
        # Reorder predictor levels based on their overall frequency counts
        dplyr::mutate(value = forcats::fct_infreq(value)) |> 
        
        # Pre-aggregate frequencies and compute localized percentage strings
        dplyr::group_by(feature, value, .data[[target_var]]) |> 
        dplyr::summarise(n_records = dplyr::n(), .groups = "drop_last") |> 
        dplyr::mutate(
            pct = n_records / sum(n_records),
            pct_label = scales::percent(pct, accuracy = 0.1)
        ) |> 
        dplyr::ungroup()
    
    # Safety check if there are no features left to plot
    if (ncol(df_long) <= 1) {
        stop("The provided dataframe does not contain enough categorical columns 
        besides the target.")
    }
    
    # 2. Generate the absolute stacked bar chart grid with relative labels
    p <- ggplot2::ggplot(
        df_long, 
        ggplot2::aes(
            x = value, 
            y = n_records, 
            fill = .data[[target_var]]
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
            size = 3.0,
            fontface = "bold",
            color = "white"
        ) +
        
        # Expand scales to prevent label clipping and ensure readability
        ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.08))
        ) +
        
        # Apply professional high-contrast palette for clinical stratification
        ggplot2::scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) +
        
        # Facet grid
        ggplot2::facet_wrap(~ feature, scales = "free", ncol = 3) +
        
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
            axis.text.x = ggplot2::element_text(
                color = "#34495e", 
                face = "bold", 
                angle = 45, 
                hjust = 1
            ),
            axis.text.y = ggplot2::element_text(color = "#34495e"),
            axis.title = ggplot2::element_text(
                face = "bold", 
                color = "#2c3e50"
            ),
            panel.spacing = ggplot2::unit(1.5, "lines")
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

# ---- Multicollinearity ----

#' Compute numeric correlation matrix
#'
#' Compute a correlation matrix for all numeric columns in the dataframe.
#'
#' @param df A dataframe containing numeric columns.
#'
#' @return A correlation matrix (data frame) with pairwise correlations between 
#' numeric features.
compute_num_corr_matrix <- function(df) {
    numeric_df <- df |> 
    dplyr::select(tidyselect::where(is.numeric))

    correlation_matrix <- stats::cor(
        numeric_df, 
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
    
    if (n_samples == 0 || min(n_rows - 1, n_cols - 1) == 0) {return(0)}
    
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
compute_cat_corr_matrix <- function(df){
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

#' Plot correlation matrix heatmap
#'
#' Automatically filters a data frame based on dtype, computes the 
#' corresponding association matrix (Pearson r or Cramer's V), and 
#' renders a clean, non-redundant lower-triangular ggplot2 heatmap.
#'
#' @param df A data frame containing clinical features.
#' @param dtype Determines whether data is categorical ("cat") or numeric 
#' ("num"). Default is "num".
#' @param title Plot title string. Default is NULL.
#' @param threshold Absolute value threshold to display numerical labels. 
#' Default is 0.4.
#'
#' @return A ggplot2 heatmap object.
plot_corr_matrix <- function(
        df, 
        dtype = "num",
        title = NULL,
        threshold = 0.4) {
    
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
        ggplot2::geom_tile(color = "#e2e8f0" , linewidth = 0.4) +
        
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
#' dataset, safely handles invariant features within complete cases, scales it 
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
plot_vif <- function(
        df, 
        target_var="AF_recurrence",
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
        
        # Add exact text labels to the tip of each bar
        ggplot2::geom_text(
            ggplot2::aes(
                label = sprintf("%.2f", VIF_Equivalent)), 
                hjust = -0.2,
                size = 3.2,
                fontface = "bold",
                color = "#1e293b"
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
            label = "Severe collinearity") +
        
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