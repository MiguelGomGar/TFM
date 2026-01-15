1# Packages ----
library(tidyverse)
library(latex2exp)

#---- Data loading ----
data <- read.csv("data/processed/predimar_long.csv") |> 
    filter(visit == 0, conc_umol_molcreat < 1500)

compounds <- unique(data$compound)
nodes <- unique(data$node)

# ---- Wrapped box plot ----
wrapped_box_plot <-  data |> 
    ggplot(
        aes(
            x = node, 
            y = conc_umol_molcreat,
            color = compound
            )
        )+
    geom_boxplot(
        #outlier.fill = "red", 
        #outlier.shape = "o", 
        #outlier.size = 3,
        outliers = as.logical(1)
    )+
    facet_wrap(~compound, scales = "free")+
    theme_minimal() +
    theme(
        axis.line = element_line(linewidth = 1.1),
        axis.ticks = element_line(linewidth = 1)
    )

ggsave(
    filename = "results/conc_stratification_by_node/wrapped_box_plot.pdf",
    plot = wrapped_box_plot,
    device = "pdf",
    width = 14,
    height = 7
)

#---- Box plot by compound ----

for (i in seq_along(compounds)){
    
    df <- data |> 
        filter(compound == compounds[i])
    
    p <- df |> 
        ggplot(
            aes(
                x = node, 
                y = conc_umol_molcreat,
            )
        )+
        geom_boxplot(
            #outlier.fill = "red", 
            #outlier.shape = "o", 
            #outlier.size = 3,
            outliers = as.logical(0)
        )+
        geom_jitter(alpha = 0.3)+
        labs(
            x = "Node",
            y = "concentration (umol/mol_creat)"
        )+
        theme_minimal() +
        theme(
            axis.line = element_line(linewidth = 1.1),
            axis.ticks = element_line(linewidth = 1)
        )
    
    ggsave(
        filename = paste0(
            "results/conc_stratification_by_node/stratification_by_node_",
            as.character(compounds[i]),
            ".pdf"
        ),
        plot = p,
        device = "pdf",
        width = 7,
        height = 5
    )
}

# Summary ----
stratification_summary <- data |> 
    group_by(node) |> 
    summarise(
        mean_conc = mean(conc_umol_molcreat, na.rm = TRUE),
        std_conc = sd(conc_umol_molcreat, na.rm = TRUE)
        )