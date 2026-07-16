# ---- Setup ----
# Load packages
suppressPackageStartupMessages({
    library(here)
    library(tidyverse)
    library(arrow)
})
source(here("src", "utils", "data_wrangling", "feature_engineering.R"))

# Set paths
input_file <- here("data", "intermediate", "02clinical_data_cleaned.parquet")
output_file <- here("data", "intermediate", "03clinical_data_engineered.parquet")

# ---- Main ----
main <- function() {
    # Load data
    print("Loading data...")
    df <- read_parquet(input_file)

    # Compute risk scores
    print("Computing available risk scores...")
    df <- df |>
        compute_hatch_score() |>
        compute_chads2_score() |>
        compute_base_af2_score() |>
        compute_mb_later_score()

    # Save data
    print(paste0("Saving data to ", output_file, "..."))
    write_parquet(df, output_file)
}

main()
