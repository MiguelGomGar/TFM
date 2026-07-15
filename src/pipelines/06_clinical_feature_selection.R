# ---- Configuration ----
# Load packages
suppressPackageStartupMessages({
    library(here)
    library(arrow)
    library(tidyverse)
})

# Set paths
input_file <- here("data", "intermediate", "03clinical_data_engineered.parquet")
output_dir <- here("data", "clean")

# ---- Main ----
main <- function() {
    # Load data
    print("Loading clinical data...")
    clinical_data <- read_parquet(input_file)

    # Identify features with high missing rates
    threshold <- 0.25
    na_rates <- colMeans(is.na(clinical_data))
    drop_cols_na <- names(na_rates[na_rates > threshold])

    # Identify highly correlated features
    drop_cols_collinearity <- c(
        "hypolipidemic_meds",
        "heart_failure"
    )

    # Save subsets for risk scores
    print("Saving subset data for HATCH score...")
    clinical_data |>
        select(code, hypertension, COPD, stroke, age, heart_failure) |>
        write_parquet(here(output_dir, "hatch_data.parquet"))

    print("Saving subset data for CHADS2 score...")
    clinical_data |>
        select(code, hypertension, diabetes, stroke, age, heart_failure) |>
        write_parquet(here(output_dir, "chads2_data.parquet"))

    print("Saving subset data for MB-LATER score...")
    clinical_data |>
        select(code, sex, bundle_branch_R, bundle_branch_L, AF_type, LA_enlargment, ERAF) |>
        write_parquet(here(output_dir, "mb_later_data.parquet"))

    print("Saving subset data for BASE-AF2 score...")
    clinical_data |>
        select(code, BMI, LA_enlargment, smoking_status, ERAF, AF_duration, AF_type) |>
        write_parquet(here(output_dir, "base_af2_data.parquet"))

    # Save clean data set
    print(paste0(
        "Dropping features with missing rate > ",
        as.character(threshold),
        " and highly correlated features..."
    ))
    clinical_data |>
        select(-all_of(c(drop_cols_na, drop_cols_collinearity))) |>
        write_parquet(here(output_dir, "06clinical_data_selected.parquet"))
}

main()
