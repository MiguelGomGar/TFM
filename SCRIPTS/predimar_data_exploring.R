library(tidyverse)
library(readxl)
library(haven)

predimar <- read_dta(path = "DATA/RAW/predimar_miguelgomez.dta")
polyphenols <- read_excel(
    path = "DATA/RAW/PREDIMAR_POLYPHENOLS (AOVE derivatives)_original.xlsx",
    sheet = "umol_mol creatinina",
    range = "A1:M1353")

etiquetas <- sapply(predimar, function(predimar) attr(predimar, "label")) # ver 
## etiquetas de variable 
print(etiquetas)

# seleccionar a partir de phenetanol4glucur0
predimar_filtrado <- predimar |> 
  mutate(across(2:5, as_factor)) |> # transforma los datos en factores
  select(2:5, starts_with("conc")) |> # selecciona las columnas relevantes
  pivot_longer(cols = starts_with("conc"),
               values_to = "conc_ngml",
               names_to = "sustancia_y_visita") |> 
    mutate(sustancia = as.factor(str_extract(sustancia_y_visita,
                                   "Tyrosol|Hydroxytyrosol")),
           etapa = as.factor(str_extract(sustancia_y_visita,
                                         "0|1")),
           sustancia_y_etapa = NULL) |> # divide la columna sustancia y etapa en 2
    drop_na()
    
saveRDS(predimar_filtrado, file = "DATA/PROCESSED/conc_polyphen.Rdata")

# análisis inicial: guardar todas las variables por visitas (6m, 12m, 0, 1...)