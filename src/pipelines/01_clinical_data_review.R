# ---- Configuration
suppressPackageStartupMessages({
    library(here)
    library(tidyverse)
    library(readxl)
})

# ---- Main Function
main <- function() {
    
    # Define paths
    input_data_dir <- here("data", "intermediate", "clinical_variables_review.xlsx")
    output_dir <- here("results", "data_collection")
    
    # Load selected data
    clinical_variables <- read_excel(
        path = input_data_dir,
        range = "A1:C32"
        )
        
    # Preprocess factor levels
    prepared_variables <- clinical_variables |> 
        mutate(
            
            # Helper factor to handle the technical bottom-to-top plotting order
            Predimar_sort = factor(
                Predimar, 
                levels = c("no", "categorized", "related", "yes")
                ),
            
            # Original factor preservation
            Predimar = factor(
                Predimar, 
                levels = c("yes", "related", "categorized", "no")
                )
        ) |> 
        
        # Places "no" values with small scores first, rising up to "yes"  
        # values with high scores
        arrange(Predimar_sort, Scores) |> 
        
        # Freeze this specific physical row order into the 'Variable' factor 
        # levels
        mutate(Variable = fct_inorder(Variable))

    # Build the bar plot
    p <- ggplot(
        prepared_variables, 
        aes(x = Variable, y = Scores, fill = Predimar)
        ) +
        
        # Grid bars
        geom_col(
            color = "#2c3e50", 
            width = 0.75, 
            linewidth = 0.4, 
            alpha = 0.85
            ) +
        
        # Invert coordinates
        coord_flip(clip = "off") +
        
        # Force integer ticks on the score axis and leave room for text labels
        scale_y_continuous(
            breaks = seq(0, max(prepared_variables$Scores), by = 1),
            expand = expansion(mult = c(0, 0.08))
        ) +
        
        # Palette mapped to clinical categories
        scale_fill_manual(
            values = c(
                "yes"         = "#10b981",
                "related"     = "#3b82f6",
                "categorized" = "#f59e0b",
                "no"          = "#ef4444"
            )
        ) +
        
        # Title, subtitle, axis labels and legend
        labs(
            title = "Risk Score Variables Selection Review",
            subtitle = "Predictors grouped by PREDIMAR cohort feasibility and ranked by literary frequency within each stratum",
            x = NULL,
            y = "Frequency Count Across Reviewed Risk Scores",
            fill = "PREDIMAR Cohort Feasibility"
        ) +
        
        # Theme customization 
        theme_minimal(base_size = 11) +
        theme(
            plot.title = element_text(
                face = "bold", 
                size = 14, 
                margin = margin(b = 4)
                ),
            plot.subtitle = element_text(
                color = "#64748b", 
                size = 9, 
                margin = margin(b = 15)
                ),
            axis.text.y = element_text(
                face = "bold", 
                color = "#334155", 
                size = 9.5
                ),
            axis.text.x = element_text(color = "#475569"),
            axis.title.x = element_text(
                face = "bold", 
                color = "#1e293b", 
                margin = margin(t = 10)
                ),
            legend.position = "top",
            legend.title = element_text(
                face = "bold", 
                color = "#1e293b", 
                size = 9.5
                ),
            legend.text = element_text(color = "#334155", size = 9),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(), 
            panel.grid.major.x = element_line(
                color = "#e2e8f0", 
                linewidth = 0.5
                )
        )
        
    # Save the plot
    file_path <- here(output_dir, "clinical_variables_review_plot.png")
    ggsave(filename = file_path, plot = p, width = 9, height = 7, dpi = 300)
}

main()