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

 # ---- Histograms ----
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

 #' Plot global categorical distribution (Paginated)
 #'
 #' @param df A data frame containing categorical features.
 #' @param title Plot title string.
 #' @param x_label Character string for the horizontal axis title.
 #' @param y_label Character string for the vertical axis title.
 #' @param ncol Number of columns per page. Default is 3.
 #' @param nrow Number of rows per page. Default is 3.
 #' @param page The specific page to render. Default is 1.
 #'
 #' @return A ggplot2 object representing one page of the faceted grid.
 plot_global_categorical_distribution <- function(df,
                                                  title = "Categorical features distribution",
                                                  x_label = NULL,
                                                  y_label = "Count",
                                                  ncol = 2,
                                                  nrow = 2) {
     # 1. Isolate factor columns and pivot to long format
     df_long <- df |>
         dplyr::select(tidyselect::where(is.factor)) |>
         tidyr::pivot_longer(
             cols = tidyselect::everything(),
             names_to = "feature",
             values_to = "value",
             values_transform = list(value = as.character)
         ) |>
         dplyr::mutate(value = forcats::fct_rev(forcats::fct_infreq(value)))

     if (ncol(df_long) == 0) stop("No factor columns found.")

     # 2. Render the plot with pagination
     p <- ggplot2::ggplot(df_long, ggplot2::aes(y = value, fill = value)) +
         ggplot2::geom_bar(color = "#2c3e50", alpha = 0.8, width = 0.7, linewidth = 0.5) +
         ggplot2::geom_text(
             ggplot2::aes(label = ggplot2::after_stat(count)),
             stat = "count", hjust = -0.2, size = 3.2, fontface = "bold", color = "#2c3e50"
         ) +
         ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +

         # Set up pagination
         cols <- ceiling(sqrt(length(unique(df_long$feature))))
     rows <- cols
     pages <- ceiling(length(unique(df_long$feature)) / (cols * rows))


     ggforce::facet_wrap_paginate(
         ~feature,
         scales = "free_y",
         ncol = cols,
         nrow = rows,
         page = page
     ) +
         ggplot2::coord_cartesian(clip = "off") +
         ggplot2::theme_minimal(base_size = 11) +
         ggplot2::theme(
             legend.position = "none",
             panel.grid.minor = ggplot2::element_blank(),
             panel.grid.major.x = ggplot2::element_line(color = "#f2f4f4", linewidth = 0.5),
             panel.grid.major.y = ggplot2::element_blank(),
             strip.background = ggplot2::element_rect(fill = "#f8f9f9", color = "#d5dbdb", linewidth = 0.5),
             strip.text = ggplot2::element_text(face = "bold", color = "#2c3e50", size = 10),
             axis.line.y = ggplot2::element_line(color = "#bdc3c7", linewidth = 0.6),
             axis.text = ggplot2::element_text(color = "#34495e", face = "bold"),
             axis.title = ggplot2::element_text(face = "bold", color = "#2c3e50"),
             panel.spacing = ggplot2::unit(1.5, "lines")
         ) +
         ggplot2::labs(title = paste0(title, " (Page ", page, ")"), x = x_label, y = y_label)

     return(p)
 }
