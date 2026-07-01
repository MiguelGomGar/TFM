# ---- Configuration
suppressPackageStartupMessages({
    library(here)
    library(tidyverse)
    library(arrow)
    library(skimr)
    library(scales)
    library(car)
})

# ---- Main Function ----
main <- function() {
    # 0. Define paths and load functions
    interm_data_dir <- here("data", "intermediate", "whole_clinical_data.parquet")
    results_dir <- here("results", "eda")
    source(here("src", "data_wrangling", "data_analysis.R"))

    # 1. Load the data
    id <- cli::cli_process_start("Loading data from {.path interm_data_dir}")
    clinical_data <- read_parquet(interm_data_dir)
    cli::cli_process_done(id)

    # 2. Global summary
    n_col <- ncol(clinical_data)
    n_row <- nrow(clinical_data)
    cli::cli_alert_info("The dataset has {n_col} columns and {n_row} rows.")

    target_features <- clinical_data |> select(where(is.factor)) |> names()

    # 3. Missing values analysis
    id <- cli::cli_process_start(
        "Generating missing values plots"
        )
    
    # Missing values per row
    missing_values_per_row <- clinical_data |> plot_row_missingness()

    # Save the plot
    save_plot(
        plot = missing_values_per_row,
        file_name = "NAs_by_row.png",
        width = 8,
        height = 5
        )

    # Missing values per feature
    missing_values_per_feature_df <- clinical_data |> 
    select(-code) |> 
    skim() |> 
    select(c(skim_variable, complete_rate)) |> 
    rename(feature = skim_variable) |>
    mutate(missing_rate = 1 - complete_rate, complete_rate = NULL)

    # Create the plot
    missing_values_per_feature_plot <- missing_values_per_feature_df |> 
    plot_column_missingness()

    # Save the plot
    save_plot(
        plot = missing_values_per_feature_plot,
        file_name = "NAs_by_feature.png",
        width = 8,
        height = 5
        )

    # Missing values stratified by feature
    for (feature in target_features) {
        missing_values_stratified <- clinical_data |> 
        select(-code) |>
        plot_stratified_missingness(target_var = feature)

        # Save the plot
        save_plot(
            plot = missing_values_stratified, 
            file_name = paste0(
                "NAs_stratified_by_", 
                feature, 
                ".png"
                ),
            width = 8, 
            height = 5
            )
    }

    cli::cli_process_done(id)

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
        height = 10
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
            height = 10
            )
    }

    cli::cli_process_done(id)

    # 6. Multicolinearity analysis
    id <- cli::cli_process_start(
        "Generating correlation matrices and VIF plots"
        )
    # Numeric features correlation matrix
    numeric_correlation_matrix <- clinical_data |> 
    select(-code) |> 
    plot_corr_matrix(threshold = 0.5)

    # Save the plot
    save_plot(
    plot = numeric_correlation_matrix,
    file_name = "numeric_correlation_matrix.png",
    width = 8,
    height = 5
    )

    # Categorical features correlation matrix
    categorical_correlation_matrix <- clinical_data |> 
    plot_corr_matrix(dtype = "cat", threshold = 0.5) |> 
    suppressWarnings()

    # Save the plot
    save_plot(
    plot = categorical_correlation_matrix,
    file_name = "categorical_correlation_matrix.png",
    width = 8, 
    height = 5
    )

    # Variance Inflation Factor analysis
    VIF <- clinical_data |> 
        select(-c(
            code, LAD, LAV, LAA, heart_failure, hypolipidemic_meds)) |>
        plot_vif()

    # Save the plot
    save_plot(
        plot = VIF,
        file_name = "VIF.png",
        width = 8,
        height = 5
        )

    cli::cli_process_done(id)
    
    # 7. Save summary tables
    id <- cli::cli_process_start(
        "Generating summary tables for all features"
        )

    # Numeric features
    output_file <- here(results_dir, "numeric_features_summary.csv")
    num_features_summary <- summarize_numeric_features(clinical_data)
    write.csv(num_features_summary, output_file, row.names = FALSE)

    # Categorical features
    output_file <- here(results_dir, "categorical_features_summary.csv")
    cat_features_summary <- summarize_categorical_features(clinical_data)
    write.csv(cat_features_summary, output_file, row.names = FALSE)

    cli::cli_process_done(id)
}

main()