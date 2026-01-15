#---- Packages ----
library(tidyverse)
library(latex2exp)

#---- Data ----
data <-  read.csv("data/processed/predimar_long.csv") |> 
    filter(visit == 0) |> 
    drop_na()

compounds <- unique(data$compound)

#---- Wrapped scatter plot ----
g1 <-  data |> 
    ggplot(
        aes(
            x = p14, 
            y = conc_umol_molcreat,
            color = compound
        )
    )+
    geom_point(alpha = 0.2)+
    geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = "grey30")+
    facet_wrap(~compound, scales = "free")+
    scale_x_continuous(
        limits = c(0, 14),
        breaks = 1:14
        )+
    theme_minimal()+
    theme(
        axis.line = element_line(linewidth = 1.1),
        axis.ticks = element_line(linewidth = 1)
    ) 

ggsave(
    filename = "results/conc_vs_p14&aove_correlation_baseline/wrapped_scatter_plot.pdf",
    plot = g1,
    device = "pdf",
    width = 18,
    height = 12
)

#---- Linear regression ----

## we want to check whether the polyphenol's concentrations vary with the p14
## variable before the trial begins

r <- rep(0, length(compounds))
r2 <- rep(0, length(compounds))
p_val <- rep(0, length(compounds))

for (i in seq_along(compounds)) {
    
    df <- data |> 
        filter(compound == compounds[i])
    
    test <- cor.test(x = df$p14, y = df$conc_umol_molcreat)
    
    r[i] <- test$estimate
    r2[i]  <- r[i]^2
    p_val[i] <- test$p.value
    
    g2 <- ggplot(
        df,
        aes(
            x = p14,
            y = conc_umol_molcreat,
            color = compound
        )
    ) +
        geom_point(alpha = 0.2, size = 2) +
        geom_smooth(
            method = "lm",
            se = FALSE,
            formula = y ~ x,
            color = "blue"
        ) +
        labs(
            x = "P14",
            y = "concentration (umol/mol_creat)"
        ) +
        scale_x_continuous(
            limits = c(1, 14),
            breaks = 0:14
            )+
        theme_minimal()+
        theme(
            axis.line = element_line(linewidth = 1.1),
            axis.ticks = element_line(linewidth = 1)
        ) 
    
    ggsave(
        filename = paste0(
            "results/conc_vs_p14&aove_correlation_baseline/concen_vs_p14_",
            as.character(compounds[i]),
            "_visit0.pdf"
        ),
        plot = g2,
        device = "pdf",
        width = 7,
        height = 5
    )
}

# Summary statistics ----
regression_summary <- tibble(
    compound = compounds,
    correlation_coef = r,
    determination_coef = r2,
    concen_pvalue = p_val
)

g3 <- regression_summary |> 
    mutate(
        compound = fct_reorder(compound, determination_coef)
    ) |> 
    ggplot(aes(x = compound, y = determination_coef)) +
    geom_col(fill = "blue", color = "black") +
    coord_flip() +
    labs(
        y = TeX("$R^2$"),
        x = NULL
    )+
    theme_minimal() +
    theme(
        axis.line = element_line(linewidth = 1.1),
        axis.ticks = element_line(linewidth = 1)
    )

ggsave(
    filename = "results/conc_vs_p14&aove_correlation_baseline/r2_compounds.pdf",
    plot = g3,
    device = "pdf",
    width = 7,
    height = 5
)