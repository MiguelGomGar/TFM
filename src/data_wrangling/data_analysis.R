# ---- Multicollinearity ----

#' Compute numeric correlation matrix
#'
#' Compute a correlation matrix for all numeric columns in the dataframe.
#'
#' @param df A data frame containing numeric columns.
#'
#' @return A correlation matrix (data frame) with pairwise correlations between
#' numeric features.
compute_num_corr_matrix <- function(df) {
    numeric_df <- df |>
        dplyr::select(tidyselect::where(is.numeric))

    correlation_matrix <- stats::cor(
        numeric_df,
        use = "pairwise.complete.obs"
    )

    return(correlation_matrix)
}

#' Compute Cramer's V
#'
#' Compute Cramer's V for two vectors.
#'
#' @param x A categorical variable (factor or character).
#' @param y Another categorical variable (factor or character).
#'
#' @return A numeric value representing Cramer's V, which ranges from 0 to 1.
compute_cramers_v <- function(x, y) {
    contingency_table <- table(x, y)
    chi2_test <- stats::chisq.test(
        contingency_table,
        correct = FALSE
    )

    n_samples <- sum(contingency_table)
    n_rows <- nrow(contingency_table)
    n_cols <- ncol(contingency_table)

    if (n_samples == 0 || min(n_rows - 1, n_cols - 1) == 0) {
        return(0)
    }

    v_value <- sqrt(
        chi2_test$statistic / (n_samples * min(n_rows - 1, n_cols - 1))
    )

    return(as.numeric(v_value))
}

#' Compute categorical correlation matrix
#'
#' Compute a correlation matrix for all the categorical columns in the data
#' frame.
#'
#' @param df A data frame containing categorical columns.
#'
#' @return A correlation matrix (data frame) with pairwise correlations between
#' categorical features.
compute_cat_corr_matrix <- function(df) {
    cat_df <- df |>
        dplyr::select(tidyselect::where(is.factor))
    feature_names <- colnames(cat_df)
    n_features <- length(feature_names)

    # Safety check if there are not enough categorical features
    if (n_features < 2) {
        stop("The provided dataframe must contain at least 2 categorical
        columns.")
    }

    # Initialize a symmetric matrix filled with 1.0 on the diagonal
    matrix <- matrix(
        1.0,
        nrow = n_features,
        ncol = n_features,
        dimnames = list(feature_names, feature_names)
    )

    # Automate pairwise extraction using nested loops
    for (i in 1:(n_features - 1)) {
        for (j in (i + 1):n_features) {
            association_score <- compute_cramers_v(cat_df[[i]], cat_df[[j]])
            matrix[i, j] <- association_score
            matrix[j, i] <- association_score
        }
    }

    return(matrix)
}

#' Generate table 1
#'
#' Create the table 1 and perform statistical tests.
#'
#' @param data A data frame containing the data to be analyzed.
#' @param strat_var A string specifying the name of the stratification variable.
#' @param cat_vars A vector of strings specifying the names of categorical variables.
#' @param nonnormal_vars A vector of strings specifying the names of non-normal continuous variables.
#' @param exact_vars A vector of strings specifying the names of categorical variables for Fisher's Exact Test.
#' @param output_dir A string path to save the resulting plot.
#' @param file_name A string for the name of the csv file.
#'
#' @return A CSV file containing the results of table 1, saved in the specified output directory.
create_table1 <- function(data,
                          strat_var,
                          cat_vars = NULL,
                          nonnormal_vars = NULL,
                          exact_vars = NULL) {
    vars <- data |> names()

    # Create table 1 object
    table1_object <- tableone::CreateTableOne(
        vars = vars,
        data = data,
        factorVars = cat_vars,
        strata = strat_var,
        addOverall = TRUE
    )

    # Perform statistical tests using direct argument passing
    table_1 <- print(
        table1_object,
        nonnormal = nonnormal_vars,
        exact = exact_vars,
        showAllLevels = TRUE,
        quote = FALSE,
        noSpaces = TRUE,
        printToggle = FALSE
    )

    return(table_1)
}
