#' Reformat visit indicator in column names
#' 
#' Replaces the visit number at the end of a column name with the corresponding 
#' month in a more readable format.
#' 
#' @param old_column_name A string representing the old column name with a visit 
#' number at the end.
#' 
#' @return A string representing the new column name with the visit number 
#' replaced by the corresponding
reformat_visit <- function(old_column_name){
    
    map_visit <- c(
        "0" =  "0",
        "1" = "12",
        "2" = "24",
        "3" = "36",
        "4" = "48"
    )
    
    visit_number <- stringr::str_extract(old_column_name, "\\d+$")
    visit_month <- map_visit[visit_number]
    
    new_column_name <- paste0(
        stringr::str_remove(old_column_name, "\\d+$"), 
        "_", 
        visit_month, 
        "m")

    return(new_column_name)
}

#' Compute HATCH score
#' 
#' Computes the HATCH score for each patient based on their age and 
#' comorbidities.
#' 
#' @param df A data frame containing patient data with columns for age,
#' hypertension, OSA, COPD, stroke, and heart failure.
#' 
#' @return A data frame with an additional column for the HATCH score.
compute_hatch_score <- function(df){
    # Computes the HATCH score for each patient
    
    df <- df |> 
        dplyr::mutate(
            hatch_score = 
                dplyr::if_else(age > 75, 1, 0) +
                dplyr::if_else(hypertension == "yes", 1, 0) +
                dplyr::if_else(OSA == "yes", 1, 0) +
                dplyr::if_else(COPD == "yes", 1, 0) +
                dplyr::if_else(stroke == "yes", 2, 0) +
                dplyr::if_else(heart_failure == "yes", 2, 0)
        )

    return(df)
}
