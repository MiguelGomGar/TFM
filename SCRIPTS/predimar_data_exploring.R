library(tidyverse)
library(haven)

predimar <- read_dta("DATA/RAW/predimar_miguelgomez.dta")

etiquetas <- sapply(predimar, function(predimar) attr(predimar, "label")) # ver etiquetas de variable 
print(etiquetas)

predimar_filtrado <- predimar |> 
  mutate(across(2:5, as_factor)) |> # transforma los datos de las columnas 2 a 5 en factores
  select(2:5, starts_with("conc")) |> # selecciona las columnas relevantes
  pivot_longer(cols = starts_with("conc"),
               values_to = "conc_ngml",
               names_to = "sustancia_y_etapa") |> 
    mutate(sustancia = as.factor(str_extract(sustancia_y_etapa,
                                   "Tyrosol|Hydroxytyrosol")),
           etapa = as.factor(str_extract(sustancia_y_etapa,
                                         "0|1")),
           sustancia_y_etapa = NULL) |> # divide la columna sustancia y etapa en 2
    drop_na()
    
saveRDS(predimar_filtrado, file = "DATA/PROCESSED/conc_polyphen.Rdata")