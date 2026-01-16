# Packages ----
library(tidyverse)

# Data loading ----
predimar_db <- read.csv("data/processed/predimar_original.csv")
polyphenols_db <- read.csv("data/processed/polyphenols_long.csv")

# Databases crossing ----
predimar_unique_patients <- predimar_db |> select(code) |> pull() |> unique()
polyphenols_unique_patients <- polyphenols_db |> select(id) |> pull() |> unique()

# patients recorded in PREDIMAR but not in polyphenols
idx_polyphenols <- polyphenols_unique_patients %in% predimar_unique_patients
polyphenols_patients_excluded <- polyphenols_unique_patients[!idx_polyphenols]

# patients recorded in POLYPHENOLS but not in predimar
idx_predimar <- predimar_unique_patients %in% polyphenols_unique_patients
predimar_patients_excluded <- predimar_unique_patients[!idx_predimar]

# Data saving ----
save(
    polyphenols_patients_excluded, 
     file = "data/processed/patients_excluded.RData"
    )