#' Plot row-wise missingness distribution
#'
#' Computes the total number of missing values (NAs) per row (patient) directly
#' from a raw dataframe, aggregates their frequency counts, and renders a
#' professional distribution plot.
#'
#' @param df A data frame containing raw clinical features.
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
#' Calculates and plots the column-wise missingness distribution from a raw data frame.
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
        # 2. Filter out complete columns, arrange, and create a clean percentage
        # label
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
