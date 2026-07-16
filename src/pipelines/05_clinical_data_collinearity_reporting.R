# ---- Configuration ----
# Load packages
suppressPackageStartupMessages({
    library(here)
    library(arrow)
    library(tidyverse)
})
source(here("src", "utils", "data_wrangling", "collinearity_analysis.R"))

# Set paths
input_file <- here("data", "intermediate", "03clinical_data_engineered.parquet")
output_dir <- here("results", "eda", "clinical_features", "collinearity")

# ---- Main function ----
main <- function() {
    # Load data
    print("Loading data (excluding features with high missing rates)...")
    df <- input_file |>
        read_parquet() |>
        select(-c(eGFR_CG, eGFR_MDRD, ischemic_cardiomyopathy, LAV, LAA, LAD))

    # Numeric correlation matrix
    print("Generating correlation matrix of numeric features...")
    num_cor_matrix <- plot_corr_matrix(df, dtype = "num")
    ggsave(
        filename = here(output_dir, "correlation_matrix_num.png"),
        plot = num_cor_matrix,
        width = 8,
        height = 6
    )

    # Categorical correlation matrix
    print("Generating correlation matrix of categorical features...")
    cat_cor_matrix <- df |>
        plot_corr_matrix(dtype = "cat")
    ggsave(
        filename = here(output_dir, "correlation_matrix_cat.png"),
        plot = cat_cor_matrix,
        width = 8,
        height = 6
    )

    # VIF
    vif_plot <- df |>
        # drop highly binary correlated features to avoid R engine crashing
        # during VIF calculation
        select(-c(code, heart_failure, hypolipidemic_meds, starts_with("score"))) |>
        plot_vif()
    ggsave(
        filename = here(output_dir, "vif.png"),
        plot = vif_plot,
        width = 9,
        height = 6
    )
}

main()
