# ---- Configuration
suppressPackageStartupMessages({
    library(here)
    library(tidyverse)
    library(haven)
    library(labelled)
    library(arrow) 
    library(data.table)
})

# ---- Main
main <- function() {
    # 0. Define paths and load functions
    raw_data_dir <- here("data", "raw", "predimar_miguelgomez.dta")
    interm_data_dir <- here("data", "intermediate")

    source(here("src", "data_wrangling", "data_cleaning.R"))

    # 1. Load raw data and remove labels
    id <- cli::cli_process_start("Collecting data from {.path raw_data_dir}")
    original_clinical_data <- read_dta(raw_data_dir) |> remove_labels()
    Sys.sleep(2)
    cli::cli_process_done(id)

    # 2. Select relevant variables
    id <- cli::cli_process_start("Selecting relevant variables")
    clinical_data_selected <- original_clinical_data |> 
    select(
        #=======================================================================
        # SOCIODEMOGRAPHIC DATA
        #=======================================================================
        sexo,
        edad,
        fum_0m,
        
        #=======================================================================
        # CLINICAL DATA
        #=======================================================================
        # Tracking & Trial Context
        code,
        nodo,
        interv,
        
        # Lifestyle & General Comorbidities
        bmi_autoref_0m,
        cintura_0m,
        altura_0m,
        mettotal_0m,
        glu_new0m,
        diabetest1_0m,
        diabetest2_0m,
        hdl_new0m,
        trig_new0m,
        hipercol_0m,
        hipolipemiantes_0m,
        saos_0m,
        insurenal_0m,
        hta_0m,
        epoc_0m,
        ictus_0m,
        
        # Specific Cardiac Conditions & Echo Structural Measurements
        ic_v00,
        miocardiopat_0m_imp,
        faa_0m_new,
        dilat_tot4_imp,
        eco_diamai_0m,
        eco_areaai_0m,
        eco_volumenai_0m,
        eco_fe_0m,
        
        # Atrial Fibrillation History & Dynamics
        tipofa,
        tiempo_fa_ablac,
        ablacion_previa,
        event_blank,
        
        # Target class
        event18m_conkardia
    )
    Sys.sleep(2)
    cli::cli_process_done(id)

    # 3. Relabel categorical variables using factors
    id <- cli::cli_process_start("Reformatting variables")
    clinical_data_modified <- clinical_data_selected |> 
    mutate(
        # ======================================================================
        # SOCIODEMOGRAPHIC DATA
        # ======================================================================
        sexo = factor(
            sexo, 
            levels = c(0, 1), 
            labels = c("male", "female"),
            ordered = FALSE
        ),
        
        fum_0m = factor(
            fum_0m,
            levels = c(0, 2, 1),
            labels = c("never", "current", "former"),
            ordered = TRUE
        ),
        
        # ======================================================================
        # CLINICAL DATA
        # ======================================================================
        # Tracking & Trial Context
        nodo = factor(
            nodo,
            levels = c(1, 2, 3, 4),
            labels = c("Pamplona", "Madrid", "Granada", "Alicante"),
            ordered = FALSE
        ),
        
        interv = factor(
            interv,
            levels = c(0, 1),
            labels = c("control", "intervention"),
            ordered  = FALSE
        ),

        # Lifestyle & General Comorbidities
        diabetest1_0m = factor(
            diabetest1_0m,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        diabetest2_0m = factor(
            diabetest2_0m,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        hipercol_0m = factor(
            hipercol_0m,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        hipolipemiantes_0m = factor(
            hipolipemiantes_0m,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        saos_0m = factor(
            saos_0m,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        insurenal_0m = factor(
            insurenal_0m,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        hta_0m = factor(
            hta_0m,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        epoc_0m = factor(
            epoc_0m,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        ictus_0m = factor(
            ictus_0m,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        # Specific Cardiac Conditions & Echo Measurements
        ic_v00 = factor(
            ic_v00,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        miocardiopat_0m_imp = factor(
            miocardiopat_0m_imp,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        faa_0m_new = factor(
            faa_0m_new,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        dilat_tot4_imp = factor(
            dilat_tot4_imp,
            levels = c(0, 1, 2, 3),
            labels = c("normal", "mild", "moderate", "severe"),
            ordered = TRUE
        ),

        eco_fe_0m = factor(
            eco_fe_0m,
            levels = c(0, 1, 2, 3),
            labels = c(
                "normal", 
                "slightly abnormal", 
                "moderately abnormal", 
                "severely abnormal"
                ),
            ordered = TRUE
        ),
        
        # ======================================================================
        # CLINICAL DATA: Atrial Fibrillation History & Dynamics
        # ======================================================================
        tipofa = factor(
            tipofa,
            levels = c(0, 1),
            labels = c("persistent", "paroxysmal"),
            ordered = FALSE
        ),
        
        ablacion_previa = factor(
            ablacion_previa,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        event_blank = factor(
            event_blank,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        ),
        
        # TARGET CLASS
        event18m_conkardia = factor(
            event18m_conkardia,
            levels = c(0, 1),
            labels = c("no", "yes"),
            ordered = FALSE
        )
    )

    # 4. Rename columns
    new_names <- c(
    "sex", "age", "smoking_status", "code", "center", "intervention", "BMI", 
    "waist_circumference", "height", "Met", "glucose", "type1_diabetes", 
    "type2_diabetes", "HDL", "triglicerides", "hypercholesterolemia", 
    "hypolipidemic_meds", "OSA", "renal_insuf", "hypertension", "COPD", 
    "stroke", "heart_failure", "cardiomyopathy", "antirrythmic_meds", 
    "LA_enlargment", "LAD", "LAA", "LAV", "LVEF", "AF_type", 
    "AF_ablation_time", "previous_ablation", "ERAF", 
    "AF_recurrence"
    )
    clinical_data_processed <- clinical_data_modified |> 
    setnames(old = names(clinical_data_modified), new = new_names)

    Sys.sleep(2)
    cli::cli_process_done(id)

    # 5. Feature engineering
    id <- cli::cli_process_start("Engineering features")
    clinical_data_processed <- clinical_data_processed |> 
    mutate(
        # waist-to-height ratio might be a better predictor than BMI
        WHR = waist_circumference / height,
        .after = BMI
    ) |> 
    select(-c(waist_circumference, height)) |> 
    compute_hatch_score()
    Sys.sleep(2)
    cli::cli_process_done(id)

    # 6. Save the data
    id <- cli::cli_process_start("Saving processed data")

    # Save the entire dataset
    whole_data_output_file <- here(interm_data_dir, "whole_clinical_data.parquet")
    write_parquet(clinical_data_processed, whole_data_output_file)

    # Save a subset with the hatch score features
    hatch_data_output_file <- here("data", "clean", "hatch_data.parquet")
    hatch_data <- clinical_data_processed |> 
        select(
            code, 
            age, 
            hypertension, 
            COPD, 
            stroke, 
            heart_failure, 
            AF_type, 
            hatch_score, 
            AF_recurrence
            )
    write_parquet(hatch_data, hatch_data_output_file)
    Sys.sleep(2)
    cli::cli_process_done(id)

    cli::cli_alert_success(
        "Full dataset saved to: {.path {whole_data_output_file}}"
        )

    cli::cli_alert_success(
        "Hatch subset saved to: {.path {hatch_data_output_file}}"
        )
}

main()