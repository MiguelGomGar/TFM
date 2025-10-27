#---- PAQUETES Y DATOS----
library(tidyverse) 
datos <- readRDS("DATA/PROCESSED/conc_polyphen.Rdata")

hydroxytyrosol <- datos |> 
    filter(sustancia == "Hydroxytyrosol")
tyrosol <- datos |> 
    filter(sustancia == "Tyrosol")

#----HYDROXYTYROSOL-----
hydroxytyrosol|>
    ggplot(aes(x = interv, y = conc_ngml))+
    geom_boxplot(outliers = TRUE, 
                 outlier.shape = "o", 
                 outlier.color = "red",
                 outlier.size = 2)+
    xlab("")+
    ylab("Concentración (ng/mL)")+
    ggtitle("Hydroxytyrosol")+
    facet_wrap(~etapa)

#----TYROSOL-----
tyrosol|>
    ggplot(aes(x = interv, y = conc_ngml))+
    geom_boxplot(outliers = TRUE, 
                 outlier.shape = "o", 
                 outlier.color = "red",
                 outlier.size = 2)+
    xlab("")+
    ylab("Concentración (ng/mL)")+
    ggtitle("Tyrosol")+
    facet_wrap(~etapa)