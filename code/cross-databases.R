# Packages ----
library(tidyverse)

# Data loading ----
predimar_db <- read.csv("data/processed/predimar_long.csv")
polyphenols_db <- read.csv("data/processed/polyphenols_long.csv")

# Databases crossing ----
predimar_unique_patients <- predimar_db |> select(code) |> pull() |> unique()
polyphenols_unique_patients <- polyphenols_db |> select(id) |> pull() |> unique()

idx_polyphenols <- polyphenols_unique_patients %in% predimar_unique_patients
polyphenols_patients_excluded <- polyphenols_unique_patients[!idx_polyphenols]

idx_predimar <- predimar_unique_patients %in% polyphenols_unique_patients
predimar_patients_excluded <- predimar_unique_patients[!idx_predimar]

# Output ----
print("Patients from POLYPHENOLS that are not in PREDIMAR:")
print(polyphenols_patients_excluded)

print("Patients from PREDIMAR that are not in POLYPHENOLS:")
print(predimar_patients_excluded)