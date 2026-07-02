# ---- Risk scores ----
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
    df_result <- df |> 
        dplyr::mutate(
            hatch_score = 
                dplyr::if_else(age > 75, 1, 0) +
                dplyr::if_else(hypertension == "yes", 1, 0) +
                dplyr::if_else(COPD == "yes", 1, 0) +
                dplyr::if_else(stroke == "yes", 2, 0) +
                dplyr::if_else(heart_failure == "yes", 2, 0)
        )
    
    return(df_result)
}

#' Compute CHADS2 score
#' 
#' Computes the CHADS2 score for each patient based on their age and 
#' comorbidities.
#' 
#' @param df A data frame containing patient data with columns for age,
#' hypertension, diabetes, stroke, stroke, and heart failure.
#' 
#' @return A data frame with an additional column for the CHADS2 score.
compute_chads2_score <- function(df){
    df_result <- df |> 
        dplyr::mutate(
            chads2_score = 
                dplyr::if_else(age >= 75, 1, 0) +
                dplyr::if_else(hypertension == "yes", 1, 0) +
                dplyr::if_else(diabetes == "yes", 1, 0) +
                dplyr::if_else(stroke == "yes", 2, 0) +
                dplyr::if_else(heart_failure == "yes", 1, 0)
        )
    
    return(df_result)
}

# ---- New features ----
#' Compute metabolic syndrome
#' 
#' Computes the metabolic syndrome for each patient based on their 
#' comorbidities.
#' 
#' @param df A data frame containing patient data with columns for central 
#' BMI, triglycerides, HDL, hypertension and glucose.
#' 
#' @return A data frame with an additional column for the metabolic syndrome.
compute_metabolic_syndrome <- function(df) {
    df_result <- df |>
        dplyr::mutate(
            mets_obesity = dplyr::if_else(BMI >= 30, 1, 0, missing = 0),
            mets_triglycerides = dplyr::if_else(
                triglycerides >= 150 | hypolipidemic_meds == "yes", 
                1, 0, missing = 0
                ),
            mets_hdl = dplyr::if_else(
                (sex == "male" & HDL < 40) | 
                    (sex == "female" & HDL < 50), 
                1, 0, missing = 0
                ),
            mets_hypertension = dplyr::if_else(
                hypertension == "yes", 1, 0, missing = 0
                ),
            mets_glucose = dplyr::if_else(
                glucose >= 100 | diabetes == "yes", 1, 0, missing = 0
                ),
            
            # Sum up comorbidities
            comorbidities = mets_obesity + mets_triglycerides + mets_hdl + mets_hypertension + mets_glucose,
            metabolic_syndrome = dplyr::if_else(
                comorbidities >= 3, "yes", "no", missing = "no"
                )
        ) |> 
        
        # Drop intermediate features calculated to compute metabolic syndrome
        dplyr::select(-dplyr::starts_with("mets_"), -comorbidities)
    
    return(df_result)
}