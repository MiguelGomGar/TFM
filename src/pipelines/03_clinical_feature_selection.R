# ---- Configuration ----
suppressPackageStartupMessages({
    library(here)
    library(arrow)
    library(tidyverse)
})

# ---- Main ----
main <- function() {
    # Define paths
    input_file <- here("data", "intermediate", "clinical_data.parquet")
    output_dir <- here("data", "clean")

    # Load data
    print("Loading clinical data...")

    clinical_data <- read_parquet(input_file)

    # Dropping records with missing rate > threshold dynamically
    threshold <- 0.25
    print(paste0(
        "Dropping records with missing rate > ", as.character(threshold)
    ))
    na_rates <- colMeans(is.na(clinical_data))
    drop_cols_na <- names(na_rates[na_rates > threshold])

    # Dropping highly correlated features manually
    print("Dropping highly correlated features...")
    drop_cols_collinearity <- c(
        "hypolipidemic_meds",
        "heart_failure"
    )

    # Saving subsets for risk scores
    print("Saving subset data for HATCH score...")
    hatch_data <- clinical_data |>
        select(code, hypertension, COPD, stroke, age, heart_failure)
    write_parquet(hatch_data, here(output_dir, "hatch_data.parquet"))

    print("Saving subset data for CHADS2 score...")
    chads2_data <- clinical_data |>
        select(code, hypertension, diabetes, stroke, age, heart_failure)
    write_parquet(chads2_data, here(output_dir, "chads2_data.parquet"))

    print("Saving subset data for MB-LATER score...")
    mb_later_data <- clinical_data |>
        select(code, sex, bundle_branch_R, bundle_branch_L, AF_type, LA_enlargment, ERAF)
    write_parquet(mb_later_data, here(output_dir, "mb_later_data.parquet"))

    print("Saving subset data for BASE-AF2 score...")
    base_af2_data <- clinical_data |>
        select(code, BMI, LA_enlargment, smoking_status, ERAF, AF_duration, AF_type)
    write_parquet(base_af2_data, here(output_dir, "base_af2_data.parquet"))
    
    # Saving clean data
    print("Saving cleaned data...")
    clinical_data_clean <- clinical_data |>
        select(-all_of(c(drop_cols_na, drop_cols_collinearity)))
    
    write_parquet(clinical_data_clean, here(output_dir, "clinical_data.parquet"))}

main()
