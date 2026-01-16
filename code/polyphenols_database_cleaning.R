# Packages ----
library(tidyverse)
library(readxl)
library(stringr)

# Data loading ----
original_dataset <- read_xlsx(
    path = "data/raw/PREDIMAR_POLYPHENOLS (AOVE derivatives)_original.xlsx",
    sheet = 2,
    range = "A1:M1353"
    )

# Format fixing ----
polyphenols_long <- original_dataset |> 
    rename(
        "id" = ...1,
        "visit" = ...2
        ) |> 
    select(-c(Compound, 12)) |> 
    slice(-1) |> 
    mutate(
        id = as.integer(id),
        visit = (gsub("V", "", visit)),
        visit = case_match(visit,
                           "00"~0,
                           "01"~1,
                           "02"~2,
                           "03"~3),
        visit = as_factor(visit)
        ) |> 
    pivot_longer(
        cols = `2-(Phenyl)ethanol-4'-glucuronide`:last_col(),
        names_to = "compound",
        values_to = "concentration_umol_molcreat"
    )

# Data saving ----
write.csv(
    polyphenols_long,
    file = "data/processed/polyphenols_long.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
)