# ---- Load dependencies ----
suppressPackageStartupMessages({
    library(here)
    library(arrow)
    library(tidyverse)
})

source(here("src", "data_wrangling", "data_analysis.R"))
source(here("src", "data_wrangling", "plots.R"))

# ---- Main ----
main <- function() {
    # Define paths
    raw_data_dir <- here(
        "data",
        "intermediate",
        "clinical_data.parquet"
    )
    clean_data_dir <- here(
        "data",
        "clean",
        "clinical_data.parquet"
    )
    tables_output_dir <- here("results", "eda", "tables")
    plots_output_dir <- here("results", "eda", "plots")

    # Load clean_data
    print("Loading data...")
    raw_data <- read_parquet(raw_data_dir) |> select(-code)
    clean_data <- read_parquet(clean_data_dir) |> select(-code)

    # MISSING VALUES
    print("Generating missing values plots...")

    ## Missing values by feature
    col_NAs_plot <- plot_column_missingness(raw_data)
    ggsave(
        filename = here(plots_output_dir, "missing_values_by_features.png"),
        plot = col_NAs_plot,
        width = 8,
        height = 6
    )

    ## Missing values per row
    row_NAs_plot <- plot_row_missingness(clean_data)
    ggsave(
        filename = here(plots_output_dir, "missing_values_by_row.png"),
        plot = row_NAs_plot,
        width = 8,
        height = 6
    )

    # COLLINEARITY
    print("Generating correlation matrices...")

    num_cor_matrix <- plot_corr_matrix(clean_data)
    ggsave(
        filename = here(plots_output_dir, "correlation_matrix_num.png"),
        plot = num_cor_matrix,
        width = 8,
        height = 6
    )

    cat_cor_matrix <- plot_corr_matrix(clean_data, dtype = "cat")
    ggsave(
        filename = here(plots_output_dir, "correlation_matrix_cat.png"),
        plot = cat_cor_matrix,
        width = 8,
        height = 6
    )

    # DISTRIBUTIONS
    strat_vars <- c("AF_recurrence", "intervention")

    ## numeric features distributions
    print("Generating numeric features distribution plot...")
    num_dist <- plot_global_numeric_distribution(clean_data)
    ggsave(
        filename = here(plots_output_dir, "numeric_features_distribution.png"),
        plot = num_dist,
        width = 8,
        height = 6
    )

    ### stratified by AF recurrence and intervention
    for (feature in strat_vars) {
        print(paste("Generating numeric features distribution stratified by", feature, "..."))
        num_dist_strat <- plot_numeric_distribution_by_feature(clean_data, feature)
        ggsave(
            filename = here(plots_output_dir, paste0("numeric_features_distribution_stratified_by_", feature, ".png")),
            plot = num_dist_strat,
            width = 8,
            height = 6
        )
    }

    ## categorical features distributions
    ### stratified by AF recurrence and intervention

    # TABLE 1
    print("Generating table 1...")
    cat_vars <- clean_data |>
        select(where(is.factor)) |>
        names()
    table_1 <- create_table1(
        data = clean_data,
        strat_var = strat_vars[1],
        cat_vars = cat_vars
    )
    write.csv(
        table_1,
        here(tables_output_dir, "table1.csv"),
        row.names = FALSE
    )
}

main()
