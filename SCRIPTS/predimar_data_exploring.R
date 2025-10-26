library(tidyverse)
library(haven)

predimar <- read_dta("DATA/RAW/predimar_miguelgomez.dta")

etiquetas <- sapply(predimar, function(predimar) attr(predimar, "label")) # ver etiquetas de variable 
print(etiquetas)

predimar_filtrado <- predimar |> 
  mutate(across(2:5, as_factor)) |> # transforma los datos de las columnas 2 a 5 en factores
  select(2:5, # selecciona las columnas 2 a 5
         starts_with("conchydroxy")) |>  # selecciona las columnas de concentración de hydroxytyrosol
  pivot_longer(cols = starts_with("Conc"), 
               names_to = "etapa", 
               names_prefix = "ConcHydroxytyrosolngmL",
               values_to = "Conc_Hydroxytyrosol_ngml") |> 
  mutate(etapa = paste0("etapa ", etapa),
         etapa = as.factor(etapa)) 

predimar_filtrado |> # gráfico de cajas y bigotes por sedes comparando por sexo
  ggplot(aes(x = interv, y = Conc_Hydroxytyrosol_ngml))+
  geom_boxplot(outliers = TRUE, outlier.color = "red", na.rm = TRUE)+
  geom_jitter(colour = "blue", na.rm = TRUE)+
  xlab("")+
  ylab("Hidroxitirosol (ng/mL)")+
  facet_wrap(~etapa)