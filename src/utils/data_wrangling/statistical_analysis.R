# ---- Single Variable Distribution Plots ----

#' Plot single numeric distribution
#'
#' Renders a professional histogram for a single continuous numeric variable.
#'
#' @param df A data frame containing the data.
#' @param col_name A string specifying the name of the numeric column.
#'
#' @return A ggplot2 object representing the histogram.
plot_single_numeric_distribution <- function(df, col_name) {
    # Check if the column is numeric to prevent ggplot errors
    if (!is.numeric(df[[col_name]])) {
        stop(paste("The column", col_name, "must be numeric."))
    }

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[col_name]])) +
        ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(count)),
            bins = 30,
            color = "white",
            fill = "#16a085",
            alpha = 0.7,
            na.rm = TRUE
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(color = "#eaeded", linewidth = 0.4),
            axis.line.x = ggplot2::element_line(color = "#bdc3c7", linewidth = 0.6),
            axis.text = ggplot2::element_text(color = "#34495e", face = "bold"),
            axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5, hjust = 0.5),
            axis.title = ggplot2::element_text(face = "bold", color = "#2c3e50"),
            plot.title = ggplot2::element_text(face = "bold", color = "#2c3e50", margin = ggplot2::margin(b = 10)),
            plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
        ) +
        ggplot2::labs(
            title = paste("Distribution of", col_name),
            x = col_name,
            y = "n"
        )

    return(p)
}

#' Plot single categorical distribution
#'
#' Renders a professional bar chart for a single categorical variable,
#' displaying dynamic percentage labels on top of each bar.
#'
#' @param df A data frame containing the data.
#' @param col_name A string specifying the name of the categorical column.
#'
#' @return A ggplot2 object representing the bar chart.
plot_single_categorical_distribution <- function(df, col_name) {
    # 1. Summarize data to calculate counts and percentages dynamically
    df_summary <- df |>
        dplyr::filter(!is.na(.data[[col_name]])) |>
        dplyr::count(.data[[col_name]]) |>
        dplyr::mutate(
            pct = n / sum(n),
            # Format as percentage
            pct_label = scales::percent(pct, accuracy = 0.1)
        )

    # 2. Build the bar plot with text labels (Mapped natively to horizontal)
    p <- ggplot2::ggplot(
        df_summary,
        ggplot2::aes(y = .data[[col_name]], x = n, fill = .data[[col_name]])
    ) +
        ggplot2::geom_col(
            color = "#2c3e50",
            alpha = 0.8,
            width = 0.6,
            linewidth = 0.5
        ) +
        ggplot2::geom_text(
            ggplot2::aes(label = pct_label, x = n),
            hjust = -0.1, # Pushes the label to the right of the horizontal bar
            size = 3.5,
            fontface = "bold",
            color = "#2c3e50"
        ) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            legend.position = "none",
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(color = "#eaeded", linewidth = 0.4),
            axis.line.y = ggplot2::element_line(color = "#bdc3c7", linewidth = 0.6),
            axis.text = ggplot2::element_text(color = "#34495e", face = "bold"),
            axis.title = ggplot2::element_text(face = "bold", color = "#2c3e50"),
            plot.title = ggplot2::element_text(face = "bold", color = "#2c3e50", margin = ggplot2::margin(b = 10)),
            plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
        ) +
        ggplot2::labs(
            title = paste("Distribution of", col_name),
            y = NULL,
            x = "n"
        )

    return(p)
}

#' Plot Stratified Numeric Distribution
#'
#' Renders a professional boxplot for a continuous numeric variable,
#' stratified by a target categorical variable.
#'
#' @param df A data frame containing the data.
#' @param col_name A string specifying the name of the numeric column.
#' @param target_var A string specifying the name of the stratifying categorical column.
#'
#' @return A ggplot2 object representing the boxplot.
plot_stratified_numeric_distribution <- function(df, col_name, target_var) {
    # 1. Validation checks
    if (!is.numeric(df[[col_name]])) {
        stop(paste("The column", col_name, "must be numeric."))
    }
    if (!is.factor(df[[target_var]]) && !is.character(df[[target_var]])) {
        stop(paste("The target variable", target_var, "must be categorical/factor."))
    }

    # 2. Build the stratified boxplot
    p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
            y = .data[[target_var]],
            x = .data[[col_name]],
            fill = .data[[target_var]]
        )
    ) +
        ggplot2::geom_boxplot(
            color = "#2c3e50",
            alpha = 0.8,
            outlier.color = "#c0392b", # Outliers in dark red to spot extreme patients
            outlier.size = 1.5,
            width = 0.5,
            na.rm = TRUE
        ) +
        # Use Viridis 'mako' palette to stay consistent with your categorical plots
        ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.7) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            legend.position = "none", # Hide legend since the y-axis already labels the groups
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(
                color = "#eaeded",
                linewidth = 0.4
            ),
            axis.line.x = ggplot2::element_line(color = "#bdc3c7", linewidth = 0.6),
            axis.line.y = ggplot2::element_line(color = "#bdc3c7", linewidth = 0.6),
            axis.text = ggplot2::element_text(color = "#34495e", face = "bold"),
            axis.title = ggplot2::element_text(face = "bold", color = "#2c3e50"),
            plot.title = ggplot2::element_text(
                face = "bold",
                color = "#2c3e50",
                margin = ggplot2::margin(b = 10)
            )
        ) +
        ggplot2::labs(
            title = paste("Distribution of", col_name, "stratified by", target_var),
            y = NULL,
            x = col_name
        )

    return(p)
}

#' Plot Stratified Categorical Distribution
#'
#' Renders a horizontal 100% stacked bar chart with internal percentage labels.
#'
#' @param df A data frame.
#' @param col_name Name of the categorical column to plot.
#' @param target_var Name of the stratifying variable.
#'
#' @return A ggplot2 object.
plot_stratified_categorical_distribution <- function(df, col_name, target_var) {
    # 1. Validation checks: a feature can't be stratified by itself
    if (col_name == target_var) {
        return(NULL)
    }

    # 2. Calculate proportions for the stacked bar chart
    df_summary <- df |>
        dplyr::select(dplyr::all_of(c(col_name, target_var))) |>
        tidyr::drop_na() |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
        dplyr::group_by(.data[[col_name]], .data[[target_var]]) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |>
        dplyr::mutate(prop = n / sum(n)) |>
        dplyr::ungroup()

    # 3. Build the plot
    p <- ggplot2::ggplot(
        df_summary,
        ggplot2::aes(y = .data[[col_name]], x = n, fill = .data[[target_var]])
    ) +
        ggplot2::geom_col(
            position = "fill",
            color = "#2c3e50",
            alpha = 0.85,
            linewidth = 0.4
        ) +
        ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(prop, accuracy = 1)),
            position = ggplot2::position_fill(vjust = 0.5),
            color = "black",
            fontface = "bold",
            size = 4
        ) +
        ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
        ggplot2::scale_x_continuous(labels = scales::percent) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            legend.position = "bottom",
            panel.grid.major.y = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = "#34495e", face = "bold"),
            axis.title = ggplot2::element_text(face = "bold", color = "#2c3e50"),
            plot.title = ggplot2::element_text(face = "bold", color = "#2c3e50", margin = ggplot2::margin(b = 10))
        ) +
        ggplot2::labs(
            title = paste("Distribution of", col_name, "stratified by", target_var),
            x = NULL,
            y = NULL,
            fill = target_var
        )

    return(p)
}

# ---- Multiple Variable Distribution Plots ----
#' Plot Multiple Global Numeric Distributions (Paginated)
#'
#' @param df Data frame with the data.
#' @param nrow Number of rows per page.
#' @param ncol Number of columns per page.
#' @return List of ggplot2 objects.
plot_global_numeric_paginated <- function(df, nrow = 2, ncol = 3) {
    # 1. Select all numeric variables automatically
    num_cols <- df |>
        dplyr::select(where(is.numeric)) |>
        names()

    # 2. Pivot to long format
    df_long <- df |>
        tidyr::pivot_longer(cols = dplyr::all_of(num_cols), names_to = "Variable", values_to = "Value") |>
        tidyr::drop_na(Value)

    # 3. Pagination logic
    plots_per_page <- nrow * ncol
    n_pages <- ceiling(length(num_cols) / plots_per_page)

    plot_list <- purrr::map(1:n_pages, ~ {
        ggplot2::ggplot(df_long, ggplot2::aes(x = Value)) +
            ggplot2::geom_histogram(bins = 30, fill = "#16a085", color = "white", alpha = 0.7) +
            ggforce::facet_wrap_paginate(~Variable, scales = "free", nrow = nrow, ncol = ncol, page = .x) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = "#2c3e50", margin = ggplot2::margin(b = 10), )
            ) +
            ggplot2::labs(
                title = paste("Global Numeric Features Distribution - Page", .x),
                x = NULL, y = NULL
            )
    })

    return(plot_list)
}

#' Plot Multiple Global Categorical Distributions (Paginated)
#'
#' @param df Data frame with the data.
#' @param nrow Number of rows per page.
#' @param ncol Number of columns per page.
#' @return List of ggplot2 objects.
plot_global_categorical_paginated <- function(df, nrow = 2, ncol = 3) {
    # 1. Automatically identify categorical variables (factors or characters)
    cat_cols <- df |>
        dplyr::select(where(~ is.factor(.x))) |>
        names()

    # 2. Pivot to long format and prepare summary data for each variable
    df_long <- df |>
        dplyr::select(dplyr::all_of(cat_cols)) |>
        tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "Variable",
            values_to = "Category",
            values_transform = list(Category = as.character)
        ) |>
        tidyr::drop_na(Category) |>
        dplyr::group_by(Variable, Category) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::group_by(Variable) |>
        dplyr::mutate(
            pct = n / sum(n),
            pct_label = scales::percent(pct, accuracy = 0.1)
        ) |>
        dplyr::ungroup()

    # 3. Calculate pages
    plots_per_page <- nrow * ncol
    n_pages <- ceiling(length(cat_cols) / plots_per_page)

    # 4. Generate paginated plots
    plot_list <- purrr::map(1:n_pages, ~ {
        ggplot2::ggplot(df_long, ggplot2::aes(x = n, y = Category, fill = Category)) +
            ggplot2::geom_col(color = "#2c3e50", alpha = 0.8) +
            scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
            ggplot2::geom_text(
                ggplot2::aes(label = pct_label),
                hjust = -0.5, size = 3, fontface = "bold"
            ) +
            coord_cartesian(clip = "off") +
            ggforce::facet_wrap_paginate(~Variable, scales = "free", nrow = nrow, ncol = ncol, page = .x) +
            ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                legend.position = "none",
                plot.title = ggplot2::element_text(face = "bold", color = "#2c3e50", margin = ggplot2::margin(b = 10)),
                plot.margin = ggplot2::margin(r = 20, l = 30, t = 20, b = 10, unit = "pt")
            ) +
            ggplot2::labs(
                title = paste("Global Categorical Features Distribution - Page", .x),
                y = NULL, x = "n"
            )
    })

    return(plot_list)
}

#' Plot Stratified Numeric Distributions (Paginated)
#'
#' Renders a professional boxplot for each numeric variable,
#' stratified by a target categorical variable and paginated.
#'
#' @param df A data frame containing the data.
#' @param target_var A string specifying the name of the stratifying categorical column.
#' @param nrow Number of rows per page in the grid.
#' @param ncol Number of columns per page in the grid.
#'
#' @return A list of ggplot2 objects, where each element is a page.
plot_stratified_numeric_paginated <- function(df, target_var, nrow = 2, ncol = 3) {
    # 1. Identify numeric variables (excluding the target variable)
    num_cols <- df |>
        dplyr::select(where(is.numeric), -dplyr::all_of(target_var)) |>
        names()

    # 2. Pivot data to long format for faceting
    df_long <- df |>
        tidyr::pivot_longer(cols = dplyr::all_of(num_cols), names_to = "Variable", values_to = "Value") |>
        tidyr::drop_na(Value)

    # 3. Calculate pages required
    plots_per_page <- nrow * ncol
    n_pages <- ceiling(length(num_cols) / plots_per_page)

    # 4. Generate the list of plots (Mapped natively to horizontal)
    plot_list <- purrr::map(1:n_pages, ~ {
        ggplot2::ggplot(df_long, ggplot2::aes(y = .data[[target_var]], x = Value, fill = .data[[target_var]])) +
            ggplot2::geom_boxplot(
                color = "#2c3e50",
                alpha = 0.8,
                outlier.size = 1.0
            ) +
            ggforce::facet_wrap_paginate(~Variable, scales = "free_x", nrow = nrow, ncol = ncol, page = .x) +
            ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.7) +
            ggplot2::theme_minimal(base_size = 11) +
            ggplot2::theme(
                legend.position = "none",
                panel.grid.minor = ggplot2::element_blank(),
                strip.text = ggplot2::element_text(face = "bold", size = 11, color = "#2c3e50"),
                plot.title = ggplot2::element_text(face = "bold", color = "#2c3e50", margin = ggplot2::margin(b = 10))
            ) +
            ggplot2::labs(
                title = paste("Numeric Distributions stratified by", target_var, "- Page", .x),
                y = target_var,
                x = "Value"
            )
    })

    return(plot_list)
}

#' Plot Stratified Categorical Distributions (Paginated)
#'
#' Renders horizontal 100% stacked bar charts for each categorical variable,
#' stratified by a target variable, and paginated.
#'
#' @param df A data frame containing the data.
#' @param target_var A string specifying the name of the stratifying categorical column.
#' @param nrow Number of rows per page in the grid.
#' @param ncol Number of columns per page in the grid.
#'
#' @return A list of ggplot2 objects, where each element is a page.
plot_stratified_categorical_paginated <- function(df, target_var, nrow = 2, ncol = 3) {
    # 1. Identify categorical variables (factors or characters), excluding target
    cat_cols <- df |>
        dplyr::select(where(~ is.factor(.x) || is.character(.x)), -dplyr::all_of(target_var)) |>
        names()

    # 2. Pivot to long format and calculate proportions
    df_long <- df |>
        dplyr::select(dplyr::all_of(cat_cols), dplyr::all_of(target_var)) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
        tidyr::pivot_longer(cols = dplyr::all_of(cat_cols), names_to = "Variable", values_to = "Category") |>
        tidyr::drop_na(Category) |>
        dplyr::group_by(Variable, Category, .data[[target_var]]) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |>
        dplyr::mutate(prop = n / sum(n)) |>
        dplyr::ungroup()

    # 3. Calculate pages
    plots_per_page <- nrow * ncol
    n_pages <- ceiling(length(cat_cols) / plots_per_page)

    # 4. Generate the list of plots (Mapped natively to horizontal)
    plot_list <- purrr::map(1:n_pages, ~ {
        ggplot2::ggplot(df_long, ggplot2::aes(y = Category, x = n, fill = .data[[target_var]])) +
            ggplot2::geom_col(position = "fill", color = "#2c3e50", alpha = 0.85, linewidth = 0.4) +
            ggforce::facet_wrap_paginate(~Variable, scales = "free_y", nrow = nrow, ncol = ncol, page = .x) +
            ggplot2::geom_text(
                ggplot2::aes(label = scales::percent(prop, accuracy = 1)),
                position = ggplot2::position_fill(vjust = 0.5),
                color = "black",
                fontface = "bold",
                size = 3.5
            ) +
            ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
            ggplot2::scale_x_continuous(labels = scales::percent) +
            ggplot2::theme_minimal(base_size = 11) +
            ggplot2::theme(
                legend.position = "bottom",
                strip.text = ggplot2::element_text(face = "bold", size = 11, color = "#2c3e50"),
                plot.title = ggplot2::element_text(face = "bold", color = "#2c3e50", margin = ggplot2::margin(b = 10)),
                plot.margin = ggplot2::margin(r = 20, l = 30, t = 20, b = 10, unit = "pt")
            ) +
            ggplot2::labs(
                title = paste("Categorical Distributions stratified by", target_var, "- Page", .x),
                x = NULL,
                y = NULL
            )
    })

    return(plot_list)
}

# ---- Normality Tests ----
#' Single Variable Q-Q Plot with Confidence Bands
#'
#' @param df A data frame containing clinical data.
#' @param feature A string specifying the name of the numeric variable to plot.
#' @param ci_level Desired confidence level (e.g., 0.95 for 95%).
#'
#' @return A ggplot object containing the single Q-Q plot.
plot_single_qq <- function(df, feature, ci_level = 0.95) {
    # 1. Filter out missing values only for the selected variable
    clean_data <- df |>
        tidyr::drop_na(dplyr::all_of(feature))

    # 2. Generate the single Q-Q plot
    p <- ggpubr::ggqqplot(
        data = clean_data,
        x = feature,
        conf.int = TRUE, # Enable confidence interval shading
        conf.int.level = ci_level, # Apply the threshold (0.95 by default)
        color = "#16a085", # Professional dark slate color for points
        ggtheme = ggplot2::theme_minimal()
    ) +
        # 3. Customize titles and labels dynamically
        ggplot2::labs(
            title = paste("Q-Q Plot:", feature),
            subtitle = paste0("Asymptotic confidence bands at ", ci_level * 100, "%"),
            x = "Theoretical Quantiles (Standard Normal Distribution)",
            y = paste("Observed Values for", feature)
        ) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = 14)
        )

    return(p)
}

#' Plot Multiple Global Q-Q Plots (Paginated)
#'
#' @param df Data frame with the data.
#' @param nrow Number of rows per page.
#' @param ncol Number of columns per page.
#' @return List of ggplot2 objects.
plot_global_qq_paginated <- function(df, nrow = 2, ncol = 3) {
    # 1. Select numeric variables
    num_cols <- df |>
        dplyr::select(where(is.numeric)) |>
        names()

    # 2. Pivot to long format
    df_long <- df |>
        tidyr::pivot_longer(cols = dplyr::all_of(num_cols), names_to = "Variable", values_to = "Value") |>
        tidyr::drop_na(Value)

    # 3. Pagination logic
    plots_per_page <- nrow * ncol
    n_pages <- ceiling(length(num_cols) / plots_per_page)

    plot_list <- purrr::map(1:n_pages, ~ {
        ggpubr::ggqqplot(
            data = df_long,
            x = "Value",
            conf.int = TRUE,
            color = "#16a085"
        ) +
            ggforce::facet_wrap_paginate(~Variable, scales = "free", nrow = nrow, ncol = ncol, page = .x) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", size = 14),
                strip.text = ggplot2::element_text(face = "bold", size = 11, color = "#2c3e50")
            ) +
            ggplot2::labs(
                title = paste("Q-Q Plots - Page", .x),
                x = "Theoretical Quantiles", y = "Sample Quantiles"
            )
    })

    return(plot_list)
}

# ---- Summary tables ----
#' Generate table 1
#'
#' Create the table 1 and perform statistical tests.
#'
#' @param data A data frame containing the data to be analyzed.
#' @param strat_var A string specifying the name of the stratification variable.
#' @param cat_vars A vector of strings specifying the names of categorical variables.
#' @param nonnormal_vars A vector of strings specifying the names of non-normal
#' continuous variables.
#' @param exact_vars A vector of strings specifying the names of categorical
#' variables for Fisher's Exact Test.
#' @param output_dir A string path to save the resulting plot.
#' @param file_name A string for the name of the csv file.
#'
#' @return A CSV file containing the results of table 1, saved in the specified
#' output directory.
create_table1 <- function(data,
                          strat_var,
                          cat_vars = NULL,
                          nonnormal_vars = NULL,
                          exact_vars = NULL) {
    vars <- data |> names()

    # Create table 1 object
    table1_object <- tableone::CreateTableOne(
        vars = vars,
        data = data,
        factorVars = cat_vars,
        strata = strat_var,
        addOverall = TRUE
    )

    # Perform statistical tests using direct argument passing
    table_1 <- print(
        table1_object,
        nonnormal = nonnormal_vars,
        exact = exact_vars,
        showAllLevels = TRUE,
        quote = FALSE,
        noSpaces = TRUE,
        printToggle = FALSE
    )

    return(table_1)
}
