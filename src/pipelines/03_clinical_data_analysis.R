# ---- Configuration
suppressPackageStartupMessages({
    library(here)
    library(tidyverse)
    library(arrow)
    library(skimr)
    library(scales)
    library(car)
})

# ---- Main
main <- function() {
    # 0. Define paths and load functions
    interm_data_dir <- here("data", "intermediate", "whole_clinical_data.parquet")
    clean_data_dir <- here("data", "clean")

    source(here("src", "data_wrangling", "data_analysis.R"))

    # 1. Load the data
    id <- cli::cli_process_start("Loading data from {.path interm_data_dir}")
    clinical_data <- read_parquet(interm_data_dir)
    cli::cli_process_done(id)

    # 2. Global summary
    n_col <- ncol(clinical_data)
    n_row <- nrow(clinical_data)
    cli::cli_alert_info("The dataset has {n_col} columns and {n_row} rows.")

    # 3. Numeric features statistical analysis
    id <- cli::cli_process_start(
        "Generating global distribution plots for numeric features"
        )

    # Global distribution
    global_numeric_distribution <- clinical_data |> 
    select(-code) |> 
    plot_global_numeric_distribution()

    # Save plot
    save_plot(
        plot = global_numeric_distribution,
        file_name = "numeric_distribution.png", 
        width = 8,
        height = 5
        )
    cli::cli_process_done(id)
    
    # Stratified distributions
    target_features <- clinical_data |> select(c(where(is.factor))) |> names()

    id <- cli::cli_process_start(
        "Generating stratified distribution plots for numeric features"
        )

    for (feature in target_features) {
        stratified_distribution <- clinical_data |> 
        select(-code) |>
        plot_stratified_numeric_distribution(target_var = feature)

        # Save the plot
        save_plot(
            plot = stratified_distribution, 
            file_name = paste0(
                "numeric_distribution_stratified_by_", 
                feature, 
                ".png"
                ),
            width = 8, 
            height = 5
            )
    }
    cli::cli_process_done(id)

    # 4. Categorical features statistical analysis
    id <- cli::cli_process_start(
        "Generating global distribution plots for categorical features"
        )

    # Global distribution
    global_categorical_distribution <- clinical_data |> 
    select(-code) |> 
    plot_global_categorical_distribution()

    # Save plot
    save_plot(
        plot = global_categorical_distribution,
        file_name = "categorical_distribution.png", 
        width = 12,
        height = 5
        )
    cli::cli_process_done(id)
    
    # Stratified distributions
    id <- cli::cli_process_start(
        "Generating stratified distribution plots for categorical features"
        )

    for (feature in target_features) {
        stratified_distribution <- clinical_data |> 
        select(-code) |>
        plot_stratified_categorical_distribution(target_var = feature)

        # Save the plot
        save_plot(
            plot = stratified_distribution, 
            file_name = paste0(
                "categorical_distribution_stratified_by_", 
                feature, 
                ".png"
                ),
            width = 12, 
            height = 8
            )
    }

    cli::cli_process_done(id)
    
} 

main()