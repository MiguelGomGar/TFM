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
}