# ---- Configuration ----
# Load packages
suppressPackageStartupMessages({
    library(here)
    library(arrow)
    library(tidyverse)
})
source(here("src", "utils", "data_wrangling", "statistical_analysis.R"))

# Set paths
input_file <- here("data", "clean", "09proteomic_data_with_target.parquet")
output_dir <- here("results", "eda", "proteomic_features")

# ---- Main function ----
main <- function() {
    # Load data
    print(paste0("Loading data from ", input_file, "..."))
    df <- read_parquet(input_file) |>
        select(-code)

    # Identifying proteomic features
    print("Identifying proteomic features...")
    proteomic_features <- df |>
        select(where(is.numeric)) |>
        names()

    # Global distribution plots
    print("Creating individualized global distribution plots for proteomic features...")
    for (feature in proteomic_features) {
        plot <- plot_single_numeric_distribution(df, feature)
        ggsave(
            filename = here(output_dir, paste0(feature, "_global_distribution.png")),
            plot = plot,
            width = 8,
            height = 6
        )
    }

    # QQ plots
    print("Creating individualized QQ plots for proteomic features...")
    for (feature in proteomic_features) {
        qqplot <- plot_single_qq(df, feature)
        ggsave(
            filename = here(output_dir, paste0(feature, "_qq_plot.png")),
            plot = qqplot,
            width = 8,
            height = 6
        )
    }

    # Distribution plots stratified by AF recurrence
    print("Creating individualized distribution plots for proteomic features stratified by AF recurrence...")
    for (feature in proteomic_features) {
        plot <- plot_stratified_numeric_distribution(
            df,
            col_name = feature,
            target_var = "AF_recurrence"
        )
        ggsave(
            filename = here(output_dir, paste0(feature, "_distribution_by_AF_recurrence.png")),
            plot = plot,
            width = 8,
            height = 6
        )
    }

    # Distribution plots stratified by intervention
    print("Creating individualized distribution plots for proteomic features stratified by intervention...")
    for (feature in proteomic_features) {
        plot <- plot_stratified_numeric_distribution(
            df,
            col_name = feature,
            target_var = "intervention"
        )
        ggsave(
            filename = here(output_dir, paste0(feature, "_distribution_by_intervention.png")),
            plot = plot,
            width = 8,
            height = 6
        )
    }
}

main()
