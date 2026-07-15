# ---- Configuration ----
# Load packages
suppressPackageStartupMessages({
    library(here)
    library(arrow)
    library(tidyverse)
})
source(here("src", "data_wrangling", "NAs_analysis.R"))

# Set paths
input_file <- here("data", "intermediate", "03clinical_data_engineered.parquet")
output_dir <- here("results", "eda", "clinical_features", "missing_values")

# ---- Main function ----
main <- function() {
    # Load data
    print("Loading data...")
    df <- read_parquet(input_file)

    # NAs by feature
    print("Generating missing values by feature plot...")
    col_NAs_plot <- plot_column_missingness(df)
    ggsave(
        filename = here(output_dir, "missing_values_by_features.png"),
        plot = col_NAs_plot,
        width = 8,
        height = 6
    )

    # NAs by row
    print("Generating missing values by row plot (all features included)...")
    row_NAs_plot <- plot_row_missingness(df)
    ggsave(
        filename = here(output_dir, "missing_values_by_row.png"),
        plot = row_NAs_plot,
        width = 8,
        height = 6
    )

    print("Generating missing values by row plot (features with high missing rates excluded)...")
    row_NAs_plot2 <- df |>
        select(-c(eGFR_CG, eGFR_MDRD, ischemic_cardiomyopathy, LAV, LAA, LAD)) |>
        plot_row_missingness()
    ggsave(
        filename = here(output_dir, "missing_values_by_row2.png"),
        plot = row_NAs_plot2,
        width = 8,
        height = 6
    )
}

main()
