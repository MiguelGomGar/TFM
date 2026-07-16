# ---- Configuration ----
# Load packages
suppressPackageStartupMessages({
    library(here)
    library(arrow)
    library(tidyverse)
})
source(here("src", "data_wrangling", "statistical_analysis.R"))

# Set paths
input_file <- here("data", "clean", "06clinical_data_selected.parquet")
output_dir <- here("results", "eda", "clinical_features", "distributions")

# ---- Main function ----
main <- function() {
    # Load data
    print(paste0("Loading data from ", input_file, "..."))
    df <- read_parquet(input_file) |>
        select(-c(code, starts_with("score")))

    # Identifying numeric and categorical features
    print("Identifying numeric and categorical features...")
    numeric_features <- df |>
        select(where(is.numeric)) |>
        names()
    categorical_features <- df |>
        select(where(is.factor)) |>
        names()

    # Numeric features global distribution plots
    print("Creating individualized global distribution plots for numeric features...")
    for (feature in numeric_features) {
        plot <- plot_single_numeric_distribution(df, feature)
        ggsave(
            filename = here(output_dir, paste0("distribution_", feature, ".png")),
            plot = plot,
            width = 8,
            height = 6
        )
    }

    print("Creating grouped global distribution plots for numeric features...")
    grouped_num_global <- plot_global_numeric_paginated(df)
    for (i in seq_along(grouped_num_global)) {
        ggsave(
            filename = here(
                output_dir,
                paste0("grouped_numeric_distribution_page_", i, ".png")
            ),
            plot = grouped_num_global[[i]],
            width = 12,
            height = 8
        )
    }

    # Numeric features QQ plots
    print("Creating individualized QQ plots for numeric features...")
    for (feature in numeric_features) {
        qqplot <- plot_single_qq(df, feature)
        ggsave(
            filename = here(output_dir, paste0("qq_plot_", feature, ".png")),
            plot = qqplot,
            width = 8,
            height = 6
        )
    }

    print("Creating grouped QQ plots for numeric features...")
    grouped_num_qq <- plot_global_qq_paginated(df)
    for (i in seq_along(grouped_num_qq)) {
        ggsave(
            filename = here(
                output_dir,
                paste0("grouped_numeric_qq_plot_page_", i, ".png")
            ),
            plot = grouped_num_qq[[i]],
            width = 12,
            height = 8
        )
    }

    # Numeric features distribution plots stratified by AF recurrence
    print("Creating individualized distribution plots  stratified by AF recurrence for numeric features...")
    for (feature in numeric_features) {
        plot <- plot_stratified_numeric_distribution(
            df,
            col_name = feature,
            target_var = "AF_recurrence"
        )
        ggsave(
            filename = here(output_dir, paste0("distribution_", feature, "_by_AF_recurrence.png")),
            plot = plot,
            width = 8,
            height = 6
        )
    }

    print("Creating grouped distribution plots stratified by AF recurrence for numeric features...")
    grouped_num_af <- plot_stratified_numeric_paginated(df, target_var = "AF_recurrence")
    for (i in seq_along(grouped_num_af)) {
        ggsave(
            filename = here(
                output_dir,
                paste0("grouped_numeric_stratified_by_AF_recurrence_page_", i, ".png")
            ),
            plot = grouped_num_af[[i]],
            width = 12,
            height = 8
        )
    }

    # Numeric features distribution plots stratified by intervention
    print("Creating individualized distribution plots stratified by intervention for numeric features...")
    for (feature in numeric_features) {
        plot <- plot_stratified_numeric_distribution(
            df,
            col_name = feature,
            target_var = "intervention"
        )
        ggsave(
            filename = here(output_dir, paste0("distribution_", feature, "_by_intervention.png")),
            plot = plot,
            width = 8,
            height = 6
        )
    }

    print("Creating grouped distribution plots stratified by intervention for numeric features...")
    grouped_num_intervention <- plot_stratified_numeric_paginated(df, target_var = "intervention")
    for (i in seq_along(grouped_num_intervention)) {
        ggsave(
            filename = here(
                output_dir,
                paste0("grouped_numeric_stratified_by_intervention_page_", i, ".png")
            ),
            plot = grouped_num_intervention[[i]],
            width = 12,
            height = 8
        )
    }

    # Categorical features global distribution plots
    print("Creating individualized global distribution plots for categorical features...")
    for (feature in categorical_features) {
        plot <- plot_single_categorical_distribution(df, feature)
        ggsave(
            filename = here(output_dir, paste0("distribution_", feature, ".png")),
            plot = plot,
            width = 8,
            height = 6
        )
    }

    print("Creating grouped global distribution plots for categorical features...")
    grouped_cat_global <- plot_global_categorical_paginated(df, nrow = 2, ncol = 2)
    for (i in seq_along(grouped_cat_global)) {
        ggsave(
            filename = here(
                output_dir,
                paste0("grouped_categorical_distribution_page_", i, ".png")
            ),
            plot = grouped_cat_global[[i]],
            width = 12,
            height = 8
        )
    }

    # Categorical features distribution plots stratified by AF recurrence
    print("Creating individualized distribution plots stratified by AF recurrence for categorical features...")
    for (feature in categorical_features) {
        plot <- plot_stratified_categorical_distribution(
            df,
            col_name = feature,
            target_var = "AF_recurrence"
        )
        ggsave(
            filename = here(output_dir, paste0("distribution_", feature, "_by_AF_recurrence.png")),
            plot = plot,
            width = 8,
            height = 6
        )
    }

    print("Creating grouped distribution plots stratified by AF recurrence for categorical features...")
    grouped_cat_af <- plot_stratified_categorical_paginated(
        df,
        target_var = "AF_recurrence",
        nrow = 2, ncol = 2
    )
    for (i in seq_along(grouped_cat_af)) {
        ggsave(
            filename = here(
                output_dir,
                paste0("grouped_categorical_stratified_by_AF_recurrence_page_", i, ".png")
            ),
            plot = grouped_cat_af[[i]],
            width = 12,
            height = 8
        )
    }

    # Categorical features distribution plots stratified by intervention
    print("Creating individualized distribution plots stratified by intervention for categorical features...")
    for (feature in categorical_features) {
        plot <- plot_stratified_categorical_distribution(
            df,
            col_name = feature,
            target_var = "intervention"
        )
        ggsave(
            filename = here(
                output_dir,
                paste0("distribution_", feature, "_by_intervention.png")
            ),
            plot = plot,
            width = 8,
            height = 6
        )
    }

    print("Creating grouped distribution plots stratified by intervention for categorical features...")
    grouped_cat_intervention <- plot_stratified_categorical_paginated(
        df,
        target_var = "intervention",
        nrow = 2, ncol = 2
    )
    for (i in seq_along(grouped_cat_intervention)) {
        ggsave(
            filename = here(
                output_dir,
                paste0("grouped_categorical_stratified_by_intervention_page_", i, ".png")
            ),
            plot = grouped_cat_intervention[[i]],
            width = 12,
            height = 8
        )
    }
}

main()
