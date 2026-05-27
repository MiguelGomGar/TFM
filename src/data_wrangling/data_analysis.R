compute_num_corr_matrix <- function(df) {
    # Compute a correlation matrix for all numeric columns in the dataframe.
    # Parameters:
    # - df: A dataframe containing numeric columns.
    #
    # Returns:
    # - A correlation matrix (data frame) with pairwise correlations between 
    # numeric features.

    numeric_df <- df |> dplyr::select(where(is.numeric))
    correlation_matrix <- stats::cor(numeric_df, use = "pairwise.complete.obs")
    return(correlation_matrix)
}

compute_cramers_v <- function(x, y) {
    # Calculate Cramer's V for two vectors. Parameters:
    # - x: A categorical variable (factor or character).
    # - y: Another categorical variable (factor or character).
    #
    # Returns:
    # - A numeric value representing Cramer's V, which ranges from 0 to 1

        contingency_table <- table(x, y)
        chi2_test <- stats::chisq.test(contingency_table, correct = FALSE)
        
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
    # Parameters:
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
        dtype = "num",
        title = NULL,
        threshold = 0.4) {
    
    # Automatically filters a data frame based on dtype, computes the 
    # corresponding association matrix (Pearson r or Cramer's V), and 
    # renders a clean, non-redundant lower-triangular ggplot2 heatmap.
    #
    # Parameters:
    # ----------
    # - df: a data frame containing clinical features.
    # - dtype: determines whether data is categorical ("cat") or numeric ("num").
    # - title: plot title string.
    # - threshold: absolute value threshold to display numerical labels.
    # 
    # Returns:
    # --------
    # - A ggplot2 heatmap object.
    
    if (dtype == "num") {
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
        # Compute numeric matrix using your helper function
        matrix_data <- compute_cat_corr_matrix(df)
        
        # Customize column name and scale range
        coefficient <- "V"
        limits <- c(0, 1)
        
        # Customize color scheme for numeric data
        fill_scale <- ggplot2::scale_fill_gradientn(
            colors = c("#f8fafc", "#bae6fd", "#0284c7", "#1e3a8a"),
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
    ggplot2::ggplot(df_long, aes(x = Feature_1, y = Feature_2, fill = Value)) +
        ggplot2::geom_tile(color = "#e2e8f0" , linewidth = 0.4) +
        
        # Inject the previously defined scale
        fill_scale +
        
        # Add text labels
        ggplot2::geom_text(
            data = df_long |> filter(abs(Value) >= threshold),
            aes(label = sprintf("%.2f", Value), color = abs(Value) > 0.6), 
            size = 3.5, 
            fontface = "bold",
            show.legend = FALSE
        ) +
        
        # Ensures text is white on dark cells and dark-blue on light cells for reading safety
        ggplot2::scale_color_manual(values = c("TRUE" = "white", "FALSE" = "#2c3e50")) +
        
        # Customization
        ggplot2::labs(
            title = title,
            x = NULL, y = NULL
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
            axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "#34495e"),
            axis.text.y = element_text(face = "bold", color = "#34495e"),
            panel.grid = element_blank()
        )
}

plot_global_numeric_distribution <- function(
        df, 
        title = NULL, 
        x_label = NULL, 
        y_label = NULL,
        bins = 20) {
    
    # Renders a professional multi-panel grid of histograms and density curves 
    # for all numeric variables in the data frame to inspect global distributions.
    #
    # Parameters:
    # ----------
    # - df: a data frame containing numeric features.
    # - title: plot title string.
    # - x_label: character string for the horizontal axis title.
    # - y_label: character string for the vertical axis title.
    # - bins: number of bins for the histogram layer to avoid over-smoothing.
    #
    # Returns:
    # --------
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
    ggplot(df_long, aes(x = value)) +
        # Subdued background histogram layer synchronized with the density scale
        geom_histogram(
            aes(y = after_stat(density)),
            bins = bins,
            color = "white",
            fill = "#16a085", 
            alpha = 0.4,
            na.rm = TRUE
        ) +
        
        # Custom x-axis label formatting to safely handle decimals and integers dynamically
        scale_x_continuous(
            labels = function(x) {
                x_clean <- x[!is.na(x)]
                if (length(x_clean) > 0 && all(x_clean == round(x_clean))) {
                    return(sprintf("%d", as.integer(x)))
                } else {
                    # Safe fallback formatting for real continuous scales (e.g., BMI)
                    return(format(x, trim = TRUE)) 
                }
            }
        ) +
        # Multi-panel wrapping with independent free axes
        facet_wrap(~ feature, scales = "free", ncol = 4) +
        
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
        labs(
            title = title,
            x = x_label,
            y = y_label
        )
}

plot_stratified_numeric_distribution <- function(
        df, 
        target_var = "AF_recurrence", 
        title = NULL, 
        x_label = NULL, 
        y_label = NULL) {
    
    # Renders a professional multi-panel grid of boxplots for all numeric variables 
    # stratified by a categorical target class.
    #
    # Parameters:
    # ----------
    # - df: a data frame containing numeric features and a target column.
    # - target_var: string name of the categorical variable to stratify by.
    # - title: plot title string.
    # - x_label: character string for the horizontal axis title.
    # - y_label: character string for the vertical axis title.
    #
    # Returns:
    # --------
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
    # We use .data[[target_var]] to dynamically evaluate the string parameter inside aes()
    ggplot(df_long, aes(x = .data[[target_var]], y = value, fill = .data[[target_var]])) +
        geom_boxplot(
            alpha = 0.75, 
            color = "#2c3e50", 
            outlier.size = 1, 
            outlier.alpha = 0.4,
            linewidth = 0.6,
            na.rm = TRUE
        ) +
        # Apply the pseudo-log transformation to safely handle wide clinical ranges
        scale_y_continuous(trans = scales::pseudo_log_trans(base = 10)) +
        scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) +
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
        labs(
            title = title,
            x = x_label,
            y = y_label
        )
}

plot_global_categorical_distribution <- function(
        df, 
        title = NULL, 
        x_label = NULL, 
        y_label = NULL) {
    
    # Renders a professional multi-panel grid of horizontal bar charts 
    # for all categorical and factor columns in the dataframe.
    #
    # Parameters:
    # ----------
    # - df: a data frame containing categorical features.
    # - title: plot title string.
    # - x_label: character string for the horizontal axis title.
    # - y_label: character string for the vertical axis title.
    #
    # Returns:
    # --------
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
    ggplot(df_long, aes(y = value, fill = value)) + 
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
        labs(
            title = title,
            x = x_label,
            y = y_label
        )
}

plot_stratified_categorical_distribution <- function(
        df, 
        target_var = "AF_recurrence", 
        title = NULL, 
        x_label = NULL, 
        y_label = NULL,
        legend_title = NULL) {
    
    # Renders a professional multi-panel grid of proportional stacked bar charts
    # for all categorical features, stratified by a chosen target class variable.
    #
    # Parameters:
    # ----------
    # - df: a data frame containing categorical features and a target column.
    # - target_var: string name of the categorical variable to stratify and color by.
    # - title: plot title string.
    # - x_label: character string for the horizontal axis title.
    # - y_label: character string for the vertical axis title.
    # - legend_title: character string for the legend title. Defaults to target_var if NULL.
    #
    # Returns:
    # --------
    # - A ggplot2 object representing the faceted grid.
    
    # Set the legend title to match the variable name if no custom label is provided
    if (is.null(legend_title)) {
        legend_title <- target_var
    }
    
    # 1. Isolate the target column along with all categorical predictors
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
        mutate(value = fct_infreq(value))
    
    # Safety check if there are no features left to plot
    if (ncol(df_long) <= 1) {
        stop("The provided dataframe does not contain enough categorical columns 
             besides the target.")
    }
    
    # 2. Generate the proportional stacked bar chart grid
    ggplot(df_long, aes(x = value, fill = .data[[target_var]])) +
        
        # Proportional bar layer
        geom_bar(
            color = "#2c3e50", 
            position = "fill",
            alpha = 0.85, 
            width = 0.7,
            linewidth = 0.5
        ) + 
        
        # Add absolute count labels centered within each filled segment
        geom_text(
            aes(label = after_stat(count)),
            stat = "count",
            position = position_fill(vjust = 0.5),
            size = 3.0,
            fontface = "bold",
            color = "white"
        ) +
        
        # Explicitly call the 'scales' namespace to format the vertical axis as %
        scale_y_continuous(
            labels = scales::label_percent(),
            expand = expansion(mult = c(0, 0.05))
        ) +
        
        # Apply professional high-contrast palette for clinical stratification
        scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) +
        
        # Multi-panel grid with free horizontal scales per clinical factor
        facet_wrap(~ feature, scales = "free_x", ncol = 3) +
        
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
        labs(
            title = title,
            x = x_label,
            y = y_label,
            fill = legend_title
        )
}