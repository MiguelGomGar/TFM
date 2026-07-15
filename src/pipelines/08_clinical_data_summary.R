# ---- Configuration ----
# Load packages
suppressPackageStartupMessages({
    library(here)
    library(arrow)
    library(tidyverse)
    library(tableone)
})
source(here("src", "data_wrangling", "distributions_analysis.R"))

# Set paths
input_file <- here("data", "clean", "06clinical_data_selected.parquet")
output_dir <- here("results", "eda", "clinical_features", "summary")

# ---- Main function ----
main <- function() {
    # Load data
    print(paste0("Loading data from ", input_file, "..."))
    df <- read_parquet(input_file)

    # TABLE 1
    print("creating Table 1...")

    # Define variables to stratify by
    strat_vars <- c("AF_recurrence", "intervention")

    # Define categorical variables
    cat_vars <- df |>
        select(where(is.factor)) |>
        names()

    # Define non-normal continuous variables
    non_normal_vars <- df |>
        select(c("AF_duration", "triglycerides", starts_with("scores"))) |>
        names()

    # Create table 1 stratifying features by the target variable
    table_1 <- create_table1(
        data = df,
        strat_var = strat_vars[1],
        cat_vars = cat_vars,
        nonnormal_vars = non_normal_vars
    )

    # Save results
    print(paste0("Saving Table 1 to ", here(output_dir, "table1.csv"), "..."))
    write.csv(
        table_1,
        here(output_dir, "table1.csv"),
        row.names = FALSE
    )

    # Create table 2 stratifying features by the intervention variable
    table_2 <- create_table1(
        data = df,
        strat_var = strat_vars[2],
        cat_vars = cat_vars,
        nonnormal_vars = non_normal_vars
    )

    # Save results
    print(paste0("Saving Table 2 to ", here(output_dir, "table2.csv"), "..."))
    write.csv(
        table_2,
        here(output_dir, "table2.csv"),
        row.names = FALSE
    )
}

main()
