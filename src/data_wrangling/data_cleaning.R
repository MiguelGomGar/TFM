reformat_visit <- function(old_column_name){
    # Replaces de visit number at the end of a column name with the 
    # corresponding month in a more readable format.
    
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

compute_hatch_score <- function(df){
    # Computes the HATCH score for each patient
    
    df <- df |> 
        mutate(
            hatch_score = 
                (if_else(age > 75, 1, 0)) +
                (if_else(hypertension == "yes", 1, 0)) +
                if_else(OSA == "yes", 1, 0) +
                (if_else(COPD == "yes", 1, 0)) +
                (if_else(stroke == "yes", 2, 0)) +
                (if_else(heart_failure == "yes", 2, 0))
        )
    
    return(df)
}