library(tidyverse)

datos <- readRDS("DATA/PROCESSED/conc_polyphen.Rdata")

datos |> # gráfico de cajas y bigotes por sedes comparando por sexo
    ggplot(aes(x = interv, y = Conc_Hydroxytyrosol_ngml))+
    geom_boxplot(outliers = TRUE, outlier.color = "red", na.rm = TRUE)+
    geom_jitter(colour = "blue", na.rm = TRUE)+
    xlab("")+
    ylab("Hidroxitirosol (ng/mL)")+
    facet_wrap(~etapa)