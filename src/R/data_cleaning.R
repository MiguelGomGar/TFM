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