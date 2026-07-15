# ---- Configuration
suppressPackageStartupMessages({
    library(here)
    library(tidyverse)
    library(arrow)
})

# ---- Main ----
main <- function() {
    # Define paths and load functions
    print("Setting up paths and loading functions...")

    input_file <- here("data", "intermediate", "clinical_data1_cleaned.parquet")
    output_file <- here("data", "intermediate", "clinical_data2_engineered.parquet")

    source(here("src", "data_wrangling", "feature_engineering.R"))

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
