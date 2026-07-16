# ---- Configuration ----
# Load packages
suppressPackageStartupMessages({
    library(here)
    library(tidyverse)
    library(arrow)
})

# Set paths
clinical_input <- here("data", "clean", "06clinical_data_selected.parquet")
proteomic_input <- here("data", "raw", "olink_baseline_wide.csv")
output_dir <- here("data", "clean")

# ---- Main function ----
main <- function() {
    print(paste0("Loading data from ", clinical_input, " and ", proteomic_input, "..."))
    df_clinical <- read_parquet(clinical_input)
    df_proteomic <- read.csv(proteomic_input)

    # Save a data subset with the proteomic features, the target class and the trial features
    print("Inserting the target class into the proteomic dataset joining by the code feature...")
    proteomic_data <- df_proteomic |>
        inner_join(df_clinical |> 
                       select(code, intervention, AF_recurrence), 
                   by = "code")
    write_parquet(proteomic_data, here(output_dir, "09proteomic_data_with_target.parquet"))

    # Save the full data set with clinical and proteomic features
    print("Merging both datasets...")
    proteomic_data <- df_proteomic |>
        inner_join(df_clinical, by = "code")
    write_parquet(proteomic_data, here(output_dir, "09full_dataset.parquet"))
}

main()
