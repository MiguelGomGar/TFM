library(tidyverse)

#---- Distributions ----
format_axis_labels = function(x) {
    
    # Custom x-axis label formatting function that dynamically checks if the 
    # input vector contains only integers and formats them without decimal 
    # places, while safely falling back to standard formatting for real 
    # continuous scales (like BMI) to prevent errors from non-integer values.
    # 
    # Parameters:
    # - x: A numeric vector representing the x-axis tick values.
    # 
    # Returns:
    # - A character vector of formatted labels, where integers are shown without
    #   decimal places and non-integers are formatted with standard numeric 
    #   formatting.

    x_clean <- x[!is.na(x)]
    if (length(x_clean) > 0 && all(x_clean == round(x_clean))) {
        return(sprintf("%d", as.integer(x)))
    } else {
        # Safe fallback formatting for real continuous scales (e.g., BMI)
        return(format(x, trim = TRUE)) 
    }
}

plot_global_numeric_distribution <- function(
        df, 
        output,
        title = "Numeric features distribution", 
        x_label = NULL, 
        y_label = "Count",
        bins = 20) {
    
    # Renders a professional multi-panel grid of histograms and density curves 
    # for all numeric variables in the data frame to inspect global distributions.
    #
    # Parameters:
    #
    # - df: a data frame containing numeric features.
    # - output: a string path to save the resulting plot.
    # - title: plot title string.
    # - x_label: character string for the horizontal axis title.
    # - y_label: character string for the vertical axis title.
    # - bins: number of bins for the histogram layer to avoid over-smoothing.
    #
    # Returns:
    # 
    # - A ggplot2 object representing the faceted grid.
    
    # 1. Isolate strictly numeric columns automatically and pivot to long format
    df_long <- df |> 
        select(where(is.numeric)) |> 
        pivot_longer(
            cols = everything(),
            names_to = "feature",
            values_to = "value"
        )
    
    # 2. Build the continuous histogram
    p <- ggplot(df_long, aes(x = value)) +
        geom_histogram(
            aes(y = after_stat(count)),
            bins = bins,
            color = "white",
            fill = "#16a085", 
            alpha = 0.4,
            na.rm = TRUE
        ) +
        
        # Custom x-axis label formatting to safely handle decimals and integers dynamically
        scale_x_continuous(format_axis_labels) +
        
        # Multi-panel wrapping with independent free axes
        facet_wrap(~ feature, scales = "free", ncol = 4) +
        
        # Prevent ggplot from cutting off text labels at the plot borders
        coord_cartesian(clip = "off") +
        
        # Clinical theme adjustments for publication-ready styling
        theme_minimal(base_size = 11) +
        theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#eaeded", linewidth = 0.4),
            strip.background = element_rect(fill = "#f8f9f9", color = "#d5dbdb", linewidth = 0.5),
            strip.text = element_text(face = "bold", color = "#2c3e50", size = 10),
            axis.line.x = element_line(color = "#bdc3c7", linewidth = 0.6),
            axis.text = element_text(color = "#34495e"),
            axis.title = element_text(face = "bold", color = "#2c3e50")
        ) +
        
        # Customize titles
        labs(
            title = title,
            x = x_label,
            y = y_label
        )
    
    # Save the results
    ggsave(
        filename = paste0(
            output,
            "numeric_distribution.png"
        ), 
        plot = p, width = 12, height = 8, dpi = 300
        )
    
    return(p)
}

plot_stratified_numeric_distribution <- function(
        df, 
        output,
        target_var = "AF_recurrence", 
        title = paste0(
            "Numeric features distribution stratified by ", 
            target_var
            ),
        x_label = NULL, 
        y_label = target_var) {
    
    # Renders a professional multi-panel grid of boxplots for all numeric variables 
    # stratified by a categorical target class.
    #
    # Parameters:
    # 
    # - df: a data frame containing numeric features and a target column.
    # - output: a string path to save the resulting plot.
    # - target_var: string name of the categorical variable to stratify by.
    # - title: plot title string.
    # - x_label: character string for the horizontal axis title.
    # - y_label: character string for the vertical axis title.
    #
    # Returns:
    # 
    # - A ggplot2 object representing the faceted grid.
    
    # 1. Filter out metadata (like 'code') implicitly by keeping only the target 
    # and strictly numeric columns, then pivot to long format.
    df_long <- df |> 
        select(any_of(target_var), where(is.numeric)) |> 
        pivot_longer(
            cols = -any_of(target_var),
            names_to = "feature",
            values_to = "value"
        )
    
    # 2. Build the stratified multi-panel boxplot grid.
    p <- ggplot(df_long, aes(
        x = value, y = .data[[target_var]], 
        fill = .data[[target_var]])) +
        geom_boxplot(
            alpha = 0.75, 
            color = "#2c3e50", 
            outlier.size = 1, 
            outlier.alpha = 0.4,
            linewidth = 0.6,
            na.rm = TRUE
        ) +
        
        # Scale fill with a high-contrast palette for clinical stratification
        scale_fill_manual(values = c("#16A085", "#2C3E50")) +
        
        # Apply the pseudo-log transformation to safely handle wide clinical ranges
        facet_wrap(~ feature, scales = "free", ncol = 4) +
        
        # Apply your customized medical theme configuration
        theme_minimal(base_size = 11) +
        theme(
            legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#eaeded", linewidth = 0.5),
            strip.background = element_rect(fill = "#f8f9f9", color = "#d5dbdb", linewidth = 0.5),
            strip.text = element_text(face = "bold", color = "#2c3e50", size = 10),
            axis.line.x = element_line(color = "#bdc3c7", linewidth = 0.6),
            axis.text = element_text(color = "#34495e"),
            axis.title = element_text(face = "bold", color = "#2c3e50"),
            panel.spacing = unit(1.2, "lines")
        ) +
        
        # Customize titles
        labs(
            title = title,
            x = x_label,
            y = y_label
        )
    
    # Save the results
    ggsave(
        filename = paste0(
            output,
            "numeric_distribution_stratified_by_",
            target_var,
            ".png"
        ),
        plot = p, width = 12, height = 8, dpi = 300
    )
    
    return(p)
}

plot_global_categorical_distribution <- function(
        df, 
        output,
        title = "Categorical features distribution", 
        x_label = NULL, 
        y_label = "Count") {
    
    # Renders a professional multi-panel grid of horizontal bar charts 
    # for all categorical and factor columns in the dataframe.
    #
    # Parameters:
    # 
    # - df: a data frame containing categorical features.
    # - output: a string path to save the resulting plot.
    # - title: plot title string.
    # - x_label: character string for the horizontal axis title.
    # - y_label: character string for the vertical axis title.
    #
    # Returns:
    # 
    # - A ggplot2 object representing the faceted grid.
    
    # 1. Isolate factor columns, exclude metadata, and pivot to long format
    df_long <- df |> 
        select(where(is.factor)) |> 
        pivot_longer(
            cols = everything(),
            names_to = "feature",
            values_to = "value",
            values_transform = list(value = as.character)
        ) |> 
        # Order factors based on absolute frequency from highest to lowest
        mutate(value = fct_rev(fct_infreq(value)))
    
    # Safety check if there are no categorical features to display
    if (ncol(df_long) == 0) {
        stop("The provided dataframe does not contain any factor or character columns.")
    }
    
    # 2. Render the faceted categorical distribution plot
    p <- ggplot(df_long, aes(y = value, fill = value)) + 
        geom_bar(
            color = "#2c3e50", 
            alpha = 0.8, 
            width = 0.7,
            linewidth = 0.5
        ) +
        
        # Add the exact count labels just outside the bars
        geom_text(
            aes(label = after_stat(count)),
            stat = "count",
            hjust = -0.2,            
            size = 3.2,
            fontface = "bold",
            color = "#2c3e50"
        ) +
        
        # Apply the dark medical 'mako' palette from viridis
        scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
        
        # Multi-panel grid wrapper with independent vertical axis scales
        facet_wrap(~ feature, scales = "free_y", ncol = 3) +
        
        # Prevent ggplot from cutting off text labels at the plot borders
        coord_cartesian(clip = "off") +
        
        # Clinical theme adjustments
        theme_minimal(base_size = 11) +
        theme(
            legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(color = "#f2f4f4", linewidth = 0.5),
            panel.grid.major.y = element_blank(),
            strip.background = element_rect(fill = "#f8f9f9", color = "#d5dbdb", linewidth = 0.5),
            strip.text = element_text(face = "bold", color = "#2c3e50", size = 10),
            axis.line.y = element_line(color = "#bdc3c7", linewidth = 0.6),
            axis.text = element_text(color = "#34495e", face = "bold"),
            axis.title = element_text(face = "bold", color = "#2c3e50"),
            panel.spacing = unit(1.5, "lines")
        ) + 
        
        # Customize labels
        labs(
            title = title,
            x = x_label,
            y = y_label
        )
    
    # Save the results
    ggsave(
        filename = paste0(
            output,
            "categorical_distribution.png"
        ), 
        plot = p, width = 12, height = 8, dpi = 300
    )
    
    return(p)
}

plot_stratified_categorical_distribution <- function(
        df, 
        output,
        target_var = "AF_recurrence", 
        title = paste0(
            "Categorical features distribution stratified by ",
            target_var
        ),
        x_label = NULL, 
        y_label = NULL,
        legend_title = NULL) {
    
    # Renders a professional multi-panel grid of absolute stacked bar charts
    # for all categorical features, where the Y-axis displays absolute counts
    # and internal bar labels dynamically show the relative percentage.
    #
    # Parameters:
    # 
    # - df: a data frame containing categorical features and a target column.
    # - output: a string path to save the resulting plot.
    # - target_var: string name of the categorical variable to stratify and color by.
    # - title: plot title string.
    # - x_label: character string for the horizontal axis title.
    # - y_label: character string for the vertical axis title.
    # - legend_title: character string for the legend title. Defaults to target_var if NULL.
    #
    # Returns:
    # 
    # - A ggplot2 object representing the faceted grid.
    
    # Set the legend title to match the variable name if no custom label is provided
    if (is.null(legend_title)) {
        legend_title <- target_var
    }
    
    # 1. Isolate target and factors, compute absolute counts and within-bar percentages
    df_long <- df |> 
        select(any_of(target_var), where(is.factor)) |> 
        
        # Pivot all columns to long format EXCEPT the target stratification variable
        pivot_longer(
            cols = -any_of(target_var),
            names_to = "feature",
            values_to = "value",
            values_transform = list(value = as.character)
        ) |> 
        
        # Clean out missing feature entries to keep the bars structurally stable
        filter(!is.na(value)) |> 
        
        # Reorder predictor levels based on their overall frequency counts
        mutate(value = fct_infreq(value)) |> 
        
        # NEW PATTERN: Pre-aggregate frequencies and compute localized percentage strings
        group_by(feature, value, .data[[target_var]]) |> 
        summarise(n_records = n(), .groups = "drop_last") |> 
        mutate(
            pct = n_records / sum(n_records),
            pct_label = scales::percent(pct, accuracy = 0.1)
        ) |> 
        ungroup()
    
    # Safety check if there are no features left to plot
    if (ncol(df_long) <= 1) {
        stop("The provided dataframe does not contain enough categorical columns besides the target.")
    }
    
    # 2. Generate the absolute stacked bar chart grid with relative labels
    # FIX: Mapped 'y' to the pre-aggregated absolute 'n_records' count column
    p <- ggplot(df_long, aes(x = value, y = n_records, fill = .data[[target_var]])) +
        
        # FIX: Swapped geom_bar(position="fill") for geom_col(position="stack") for absolute scaling
        geom_col(
            color = "#2c3e50", 
            position = "stack",
            alpha = 0.85, 
            width = 0.7,
            linewidth = 0.5
        ) + 
        
        # Add text relative labels
        geom_text(
            aes(label = pct_label),
            position = position_stack(vjust = 0.5),
            size = 3.0,
            fontface = "bold",
            color = "white"
        ) +
        
        # Expand scales to prevent label clipping and ensure readability
        scale_y_continuous(
            expand = expansion(mult = c(0, 0.08))
        ) +
        
        # Apply professional high-contrast palette for clinical stratification
        scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) +
        
        # Facet grid
        facet_wrap(~ feature, scales = "free", ncol = 3) +
        
        # Clinical theme adjustments
        theme_minimal(base_size = 11) +
        theme(
            legend.position = "top",
            
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_line(color = "#eaeded", linewidth = 0.5), 
            
            strip.background = element_rect(fill = "#f8f9f9", color = "#d5dbdb", linewidth = 0.5),
            strip.text = element_text(face = "bold", color = "#2c3e50", size = 10),
            
            axis.line.x = element_line(color = "#bdc3c7", linewidth = 0.6),
            axis.text.x = element_text(color = "#34495e", face = "bold", angle = 45, hjust = 1),
            axis.text.y = element_text(color = "#34495e"),
            axis.title = element_text(face = "bold", color = "#2c3e50"),
            
            panel.spacing = unit(1.5, "lines")
        ) +
        
        # Customize labels
        labs(
            title = title,
            x = x_label,
            y = y_label,
            fill = legend_title
        )
    
    # Save the results
    ggsave(
        filename = paste0(
            output,
            "categorical_distribution_stratified_by_",
            target_var,
            ".png"
        ),
        plot = p, width = 12, height = 16, dpi = 300
    )
    
    return(p)
}

# ---- Multicollinearity ----

compute_num_corr_matrix <- function(df) {
    # Compute a correlation matrix for all numeric columns in the dataframe.
    # Parameters:
    # 
    # - df: A dataframe containing numeric columns.
    #
    # Returns:
    # 
    # - A correlation matrix (data frame) with pairwise correlations between 
    # numeric features.

    numeric_df <- df |> select(where(is.numeric))
    correlation_matrix <- cor(numeric_df, use = "pairwise.complete.obs")
    return(correlation_matrix)
}

compute_cramers_v <- function(x, y) {
    # Compute Cramer's V for two vectors. 
    # 
    # Parameters:
    # 
    # - x: A categorical variable (factor or character).
    # - y: Another categorical variable (factor or character).
    #
    # Returns:
    # 
    # - A numeric value representing Cramer's V, which ranges from 0 to 1

        contingency_table <- table(x, y)
        chi2_test <- chisq.test(contingency_table, correct = FALSE)
        
        n_samples <- sum(contingency_table)
        n_rows <- nrow(contingency_table)
        n_cols <- ncol(contingency_table)
        
        if (n_samples == 0 || min(n_rows - 1, n_cols - 1) == 0) {
            return(0)
        }
        
        v_value <- sqrt(chi2_test$statistic / (n_samples * min(n_rows - 1, n_cols - 1)))
        return(as.numeric(v_value))
        }

compute_cat_corr_matrix <- function(df){
    # Compute a correlation matrix for all the categorical columns in the data 
    # frame.
    # 
    # Parameters:
    # 
    # - df: a data frame containing categorical columns
    # Returns:
    # - A correlation matrix (data frame) with pairwise correlations between 
    # categorical features.
    
    # Select only categorical features
    cat_df <- df |> dplyr::select(where(is.factor))
    feature_names <- colnames(cat_df)
    n_features <- length(feature_names)
    
    # Safety check if there are not enough categorical features
    if (n_features < 2) {
        stop("The provided dataframe must contain at least 2 categorical columns.")
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

plot_corr_matrix <- function(
        df, 
        output,
        dtype = "num",
        title = NULL,
        threshold = 0.4) {
    
    # Automatically filters a data frame based on dtype, computes the 
    # corresponding association matrix (Pearson r or Cramer's V), and 
    # renders a clean, non-redundant lower-triangular ggplot2 heatmap.
    #
    # Parameters:
    # 
    # - df: a data frame containing clinical features.
    # - output: a string path to save the resulting plot.
    # - dtype: determines whether data is categorical ("cat") or numeric ("num").
    # - title: plot title string.
    # - threshold: absolute value threshold to display numerical labels.
    # 
    # Returns:
    # 
    # - A ggplot2 heatmap object.
    
    if (dtype == "num") {
        
        flag <- "numeric"
        
        # Compute numeric matrix using your helper function
        matrix_data <- compute_num_corr_matrix(df)
        
        # Customize column name and scale range
        coefficient <- "r"
        limits <- c(-1, 1)
        
        # Customize color scheme for numeric data
        fill_scale <- scale_fill_gradient2(
            low = "#3182bd", 
            mid = "white", 
            high = "#de2d26", 
            midpoint = 0, 
            limits = limits,
            name = coefficient
        )
    } else if (dtype == "cat") {
        flag <- "categorical"
        
        # Compute numeric matrix using your helper function
        matrix_data <- compute_cat_corr_matrix(df)
        
        # Customize column name and scale range
        coefficient <- "V"
        limits <- c(0, 1)
        
        # Customize color scheme for numeric data
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
        rownames_to_column(var = "Feature_1") |>
        pivot_longer(
            cols = -Feature_1, 
            names_to = "Feature_2", 
            values_to = "Value"
        ) |> 
        filter(!is.na(Value)) |> 
        mutate(
            Feature_1 = factor(Feature_1, levels = feature_order),
            Feature_2 = factor(Feature_2, levels = rev(feature_order))
        )
    
    # Render the triangular heat map
    p <- ggplot(df_long, aes(x = Feature_1, y = Feature_2, fill = Value)) + 
        geom_tile(color = "#e2e8f0" , linewidth = 0.4) +
        
        # Inject the previously defined scale
        fill_scale +
        
        # Add text labels
        geom_text(
            data = df_long |> filter(abs(Value) >= threshold),
            aes(label = sprintf("%.2f", Value), color = abs(Value) > 0.6), 
            size = 3.5, 
            fontface = "bold",
            show.legend = FALSE
        ) +
        
        # Ensures text is white on dark cells and dark-blue on light cells for reading safety
        scale_color_manual(values = c("TRUE" = "white", "FALSE" = "#2c3e50")) +
        
        # Customization
        labs(
            title = title,
            x = NULL, y = NULL
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
            axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "#34495e"),
            axis.text.y = element_text(face = "bold", color = "#34495e"),
            panel.grid = element_blank()
        )
    
    # Save the results
    ggsave(
        filename = paste0(
            output,
            flag,
            "_correlation_matrix",
            ".png"
            ),
        plot = p, width = 12, height = 8, dpi = 300
    )
    
    return(p)
}

plot_vif <- function(
        df, 
        output,
        target_var="AF_recurrence",
        title = "VIF Diagnostics",
        x_label = "VIF / GVIF^2",
        y_label = NULL) {
    # Computes the Generalized Variance Inflation Factor (GVIF) for a mixed dataset,
    # safely handles invariant features within complete cases, scales it back to 
    # a standard VIF equivalent, and renders a professional horizontal bar chart.
    #
    # Parameters:
    # 
    # - df: A data frame containing both numeric features and categorical factors.
    # - output: A string path to save the resulting plot.
    # - target_var: Character string specifying the dependent variable to predict.
    # - title: Character string for the plot title.
    # - x_label: Character string for the horizontal axis title.
    # - y_label: Character string for the vertical axis title. Defaults to NULL for no label.
    #
    # Returns:
    # 
    # - A ggplot2 object representing the publication-ready VIF bar chart.
    
    # 1. Clean tracking columns, drop unused factors, and force target to numeric
    temp_data <- df |> 
        mutate(across(all_of(target_var), as.numeric)) |> 
        droplevels()
    
    # 2. Extract complete cases to replicate lm()'s internal listwise row deletion
    complete_cases_subset <- temp_data |> 
        drop_na()
    
    # 3. Dynamic filtration: Identify and drop invariant features within complete cases
    # to protect the linear model from the "contrasts can be applied only to factors" crash.
    single_level_features <- complete_cases_subset |>
        select(where(is.factor)) |>
        summarise(across(everything(), ~ n_distinct(.))) |>
        pivot_longer(
            cols = everything(), 
            names_to = "feature", 
            values_to = "unique_levels"
            ) |>
        filter(unique_levels < 2) |>
        pull(feature)
    
    if (length(single_level_features) > 0) {
        cat("\n[VIF Diagnostics] Automatically dropping columns that become invariant within complete cases:\n", 
            paste(single_level_features, collapse = ", "), "\n\n")
        temp_data <- temp_data |> 
            select(-all_of(single_level_features))
    }
    
    # 4. Fit the linear regression model dynamically
    formula_string <- paste(target_var, "~ .")
    regression_model <- lm(as.formula(formula_string), data = temp_data)
    
    # 5. Compute VIF and process structure safely based on feature data types
    vif_raw_output <- car::vif(regression_model) |> 
        as.data.frame()
    
    if ("GVIF^(1/(2*Df))" %in% colnames(vif_raw_output)) {
        vif_df <- vif_raw_output |> 
            rownames_to_column(var = "feature") |> 
            rename(Adjusted_GVIF = `GVIF^(1/(2*Df))`) |> 
            mutate(VIF_Equivalent = Adjusted_GVIF^2) |> 
            select(feature, VIF_Equivalent)
    } else {
        vif_df <- vif_raw_output |> 
            rownames_to_column(var = "feature") |> 
            rename(VIF_Equivalent = 1) |> 
            select(feature, VIF_Equivalent)
    }
    
    # 6. Build the professional horizontal diagnostics visualization
    p <- vif_df |> 
        ggplot(aes(x = reorder(feature, VIF_Equivalent), y = VIF_Equivalent)) + 
        geom_bar(stat = "identity", fill = "#3b82f6", alpha = 0.85, width = 0.7) +
        
        # Add exact text labels to the tip of each bar
        geom_text(
            aes(label = sprintf("%.2f", VIF_Equivalent)),
            hjust = -0.2,
            size = 3.2,
            fontface = "bold",
            color = "#1e293b"
        ) +
        
        # Establish clinical collinearity reference lines
        geom_hline(yintercept = 10, linetype = "dashed", color = "#ef4444", linewidth = 0.7) +
        annotate("text", x = 0.7, y = 10.2, 
                 color = "#b91c1c", size = 3, fontface = "italic", hjust = 0,
                 label = "Severe collinearity") +
        
        # Horizontal orientation
        coord_flip(clip = "off") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        
        # Titles and design setup
        labs(
            title = title,
            x = y_label, 
            y = x_label
        ) +
        
        # Customization
        theme_minimal(base_size = 11) +
        theme(
            plot.title = element_text(face = "bold", size = 14, margin = margin(b = 4)),
            plot.subtitle = element_text(color = "#64748b", size = 9, margin = margin(b = 15)),
            axis.text.y = element_text(face = "bold", color = "#334155"),
            axis.text.x = element_text(color = "#475569"),
            axis.title.x = element_text(face = "bold", color = "#1e293b", margin = margin(t = 10)),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#f1f5f9", linewidth = 0.5)
        )
    
    # Save the results
    ggsave(
        filename = paste0(
            output,
            "VIF.png"
            
            ),
        plot = p, width = 12, height = 8, dpi = 300
    )
    
    return(p)
}

# ---- Missing values ----
plot_stratified_missingness <- function(
        df, 
        output,
        target_var = "AF_recurrence",
        title = "Missing Data Diagnostic",
        subtitle = "Proportion and absolute count of missing values evaluated 
        within each ",
        x_label = "Missingness Rate within Stratum (%)",
        y_label = target_var) {
    # Renders a bar chart analyzing the proportion and absolute count of missing 
    # values (NAs) stratified by a target class. Safely handles mixed data types 
    # (factors and numerics) during the pivoting phase.
    #
    # Parameters:
    #
    # - df: A data frame containing clinical features.
    # - output: A string path to save the resulting plot.
    # - target_var: Character string specifying the categorical variable to 
    # stratify by.
    # - title: Character string for the plot title.
    # - subtitle: Character string for the plot subtitle.
    # - x_label: Character string for the horizontal axis title.
    # - y_label: Character string for the vertical axis title.
    #
    # Returns:
    #
    # - A ggplot2 object representing the faceted missingness grid.
    
    # 1. Isolate the target column and any predictors containing at least one NA
    df_missing_analysis <- df |> 
        select(all_of(target_var), where(~ any(is.na(.)))) |> 
        pivot_longer(
            cols = -all_of(target_var),
            names_to = "feature",
            values_to = "value",
            values_transform = list(value = as.character)
        ) |> 
        
        # Calculate the missingness rate inside each stratum dynamically
        group_by(feature, .data[[target_var]]) |> 
        summarise(
            na_count = sum(is.na(value)),
            group_total = n(),
            na_rate = na_count / group_total,
            .groups = "drop"
        )
    
    # Safety check in case no columns contain missing values
    if (nrow(df_missing_analysis) == 0) {
        stop("The provided dataframe does not contain any missing values (NAs) to analyze.")
    }
    
    # 2. Render the faceted bar chart
    p <- ggplot(df_missing_analysis, aes(
        x = na_rate, 
        y = .data[[target_var]], 
        fill = .data[[target_var]])
        ) +
        geom_col(color = "#2c3e50", alpha = 0.85, width = 0.6, linewidth = 0.5) +
        
        # Add text labels
        geom_text(
            aes(label = sprintf("%.1f%%", na_rate * 100)),
            hjust = -0.1,
            size = 2.8,
            fontface = "bold",
            color = "#1e293b",
            lineheight = 0.85
        ) +
        
        # Format vertical axis as standard percentage with safety overhead space
        scale_x_continuous(
            labels = label_percent(),
            expand = expansion(mult = c(0, 0.25))
        ) +
        
        # High-contrast medical palette
        scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) +
        
        # Facet grid
        facet_wrap(~ feature, ncol = 3) +
        
        # Set labels
        labs(
            title = title,
            subtitle = paste(subtitle, target_var, "stratum"),
            x = x_label,
            y = y_label
        ) +
        
        # Customize
        theme_minimal(base_size = 11) +
        theme(
            legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#eaeded", linewidth = 0.5),
            strip.background = element_rect(fill = "#f8f9f9", color = "#d5dbdb", linewidth = 0.5),
            strip.text = element_text(face = "bold", color = "#2c3e50", size = 9),
            axis.line.x = element_line(color = "#bdc3c7", linewidth = 0.6),
            axis.text = element_text(color = "#34495e", face = "bold"),
            axis.title = element_text(face = "bold", color = "#2c3e50"),
            panel.spacing = unit(1.5, "lines")
        )
    
    # Save the results
    ggsave(
        filename = paste0(
            output,
            "NAs_stratified_by_",
            target_var,
            ".png"
        ),
        plot = p, width = 12, height = 8, dpi = 300
    )
    
    return(p)
}

plot_row_missingness <- function(
        df,
        output,
        title = "Distribution of Missing Values per Patient",
        subtitle = "Analysis of row-wise missingness patterns across the PREDIMAR cohort",
        x_label = "Number of missing values",
        y_label = "Number of Rrcords"
) {
    # Computes the total number of missing values (NAs) per row (patient),
    # aggregates their frequency counts, and renders a professional distribution plot.
    # Y-axis displays absolute counts for sample-size transparency, while 
    # bar labels dynamically show relative percentages for clinical impact assessment.
    #
    # Parameters:
    # 
    # - df: A data frame containing clinical features.
    # - output: A string path to save the resulting plot.
    # - title: plot title string.
    # - subtitle: plot subtitle string.
    # - x_label: character string for the horizontal axis title.
    # - y_label: character string for the vertical axis title.
    #
    # Returns:
    # 
    # - A ggplot2 object representing the publication-ready missingness distribution.
    
    # 1. Compute missing values per row and aggregate metrics cleanly
    na_summary <- df |> 
        mutate(row_na_count = rowSums(is.na(df))) |> 
        count(row_na_count, name = "n_records") |>
        mutate(pct_label = scales::percent(n_records / sum(n_records), accuracy = 0.1)) |> 
        arrange(row_na_count)
    
    # 2. Build the distribution chart
    p <- ggplot(na_summary, aes(x = row_na_count, y = n_records)) +
        
        # Main vertical bar layer
        geom_col(
            fill = "#2563eb", 
            color = "#1e3a8a", 
            alpha = 0.85, 
            width = 0.75, 
            linewidth = 0.4
            ) +
        
        # Add the percentage text on top of the columns
        geom_text(
            aes(label = pct_label),
            vjust = -0.6,
            size = 3.0,         
            fontface = "bold",  
            color = "#1e293b"   
        ) +
        
        # Format vertical axis to eliminate lower floating and add safety room for text strings
        scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
        
        # Customize titles and labels
        labs(
            title = title,
            subtitle = subtitle,
            x = x_label,
            y = y_label
        ) +
        
        # Customize theme
        theme_minimal(base_size = 11) +
        theme(
            plot.title = element_text(face = "bold", size = 13, margin = margin(b = 4)),
            plot.subtitle = element_text(color = "#64748b", size = 9, margin = margin(b = 15)),
            axis.title = element_text(face = "bold", color = "#1e293b"),
            axis.text.y = element_text(color = "#475569", face = "bold"),
            axis.text.x = element_text(color = "#475569", face = "bold", hjust = 1),
            axis.line.x = element_line(color = "#bdc3c7", linewidth = 0.6),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_line(color = "#f1f5f9", linewidth = 0.5)
        )
    
    # Save the results
    ggsave(
        filename = paste0(output, "NAs_distributed_by_row.png"),
        plot = p, width = 12, height = 8, dpi = 300
    )
    
    return(p)
}