library(tidyverse)
library(haven)

predimar <- read_dta("DATA/RAW/predimar_miguelgomez.dta")

etiquetas <- sapply(predimar, function(predimar) attr(predimar, "label")) # ver etiquetas de variable 
print(etiquetas)

predimar_filtrado <- predimar |> 
  mutate(across(2:5, as_factor)) |> # transforma los datos de las columnas 2 a 5 en factores
  select(2:5, # selecciona las primeras columnas con datos relevantes
         starts_with("conc")) |> # selecciona las columnas de concentración de hidroxitirosol y tirosol 
  pivot_longer(cols = starts_with("conc"),
               values_to = "conc_ngml",
               names_to = "sustancia_y_etapa") |> 
    mutate(sustancia = str)
    
    
saveRDS(predimar_filtrado, file = "DATA/PROCESSED/conc_polyphen.Rdata")