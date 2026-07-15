# ---- Setup ----
# Load packages
suppressPackageStartupMessages({
    library(here)
    library(tidyverse)
    library(haven)
    library(labelled)
    library(arrow)
    library(data.table)
})

# Set paths
input_file <- here("data", "raw", "predimar_miguelgomez.dta")
output_file <- here("data", "intermediate", "02clinical_data_cleaned.parquet")

# ---- Main ----
main <- function() {
    # Load raw data and remove labels
    print("Loading raw data and removing STATA labels...")

    original_clinical_data <- read_dta(input_file) |> remove_labels()

    # Select features
    print("Selecting relevant features...")

    clinical_data_selected <- original_clinical_data |>
        select(
            # SOCIODEMOGRAPHIC DATA
            sexo,
            edad,
            fum_0m,

            # CLINICAL DATA
            # Trial Context
            code,
            interv,

            # General Comorbidities
            bmi_autoref_0m,
            glu_new0m,
            diabetest2_0m,
            hdl_new0m,
            trig_new0m,
            hipercol_0m,
            hipolipemiantes_0m,
            saos_0m,
            insurenal_0m,
            fglopre_MDRD_cal_0m,
            fglopre_CockroftGault_cal_0m,
            hta_0m,
            epoc_0m,
            ictus_0m,

            # Specific Cardiac Conditions
            bri_0m,
            brd_0m,
            ic_v00,
            mio_isq_0m,
            miocardiopat_0m_imp,
            faa_0m_new,
            dilat_tot4_imp,
            eco_diamai_0m,
            eco_areaai_0m,
            eco_volumenai_0m,
            eco_fe_0m,

            # Specific Atrial Fibrillation Conditions
            tipofa,
            tiempo_fa_ablac,
            ablacion_previa,
            event_blank,

            # Target class
            event18m_conkardia,

            # Risk scores related to AF
            chad2ds2vasc_v00
        )

    # Transform categorical variables
    print("Converting categorical variables to factors and labelling them...")

    clinical_data_modified <- clinical_data_selected |>
        mutate(
            # SOCIODEMOGRAPHIC DATA
            sexo = factor(
                sexo,
                levels = c(0, 1),
                labels = c("male", "female"),
                ordered = FALSE
            ),
            fum_0m = factor(
                fum_0m,
                levels = c(0, 2, 1),
                labels = c("never", "former", "current"),
                ordered = TRUE
            ),

            # CLINICAL DATA
            # Trial Context
            interv = factor(
                interv,
                levels = c(0, 1),
                labels = c("control", "intervention"),
                ordered = FALSE
            ),

            # General Comorbidities
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
            fglopre_MDRD_cal_0m = factor(
                fglopre_MDRD_cal_0m,
                levels = c(0, 1),
                labels = c("no", "yes"),
                ordered = FALSE
            ),
            fglopre_CockroftGault_cal_0m = factor(
                fglopre_CockroftGault_cal_0m,
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

            # Specific Cardiac Conditions
            bri_0m = factor(
                bri_0m,
                levels = c(0, 1),
                labels = c("no", "yes"),
                ordered = FALSE
            ),
            brd_0m = factor(
                brd_0m,
                levels = c(0, 1),
                labels = c("no", "yes"),
                ordered = FALSE
            ),
            ic_v00 = factor(
                ic_v00,
                levels = c(0, 1),
                labels = c("no", "yes"),
                ordered = FALSE
            ),
            mio_isq_0m = factor(
                mio_isq_0m,
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
                labels = c("normal", "mild", "moderate", "severe"),
                ordered = TRUE
            ),

            # Specific Atrial Fibrillation Conditions
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

    # Rename columns
    print("Renaming features...")

    new_names <- c(
        "sex",
        "age",
        "smoking_status",
        "code",
        "intervention",
        "BMI",
        "glucose",
        "diabetes",
        "HDL",
        "triglycerides",
        "hypercholesterolemia",
        "hypolipidemic_meds",
        "OSA",
        "renal_insuf",
        "eGFR_MDRD",
        "eGFR_CG",
        "hypertension",
        "COPD",
        "stroke",
        "bundle_branch_R",
        "bundle_branch_L",
        "heart_failure",
        "ischemic_cardiomyopathy",
        "cardiomyopathy",
        "antirrythmic_meds",
        "LA_enlargment",
        "LAD",
        "LAA",
        "LAV",
        "LVEF",
        "AF_type",
        "AF_duration",
        "previous_ablation",
        "ERAF",
        "AF_recurrence",
        "score_chad2ds2_vasc"
    )

    clinical_data_processed <- clinical_data_modified |>
        setnames(old = names(clinical_data_modified), new = new_names)

    # Save data
    print(paste0("Saving data to ", output_file, "..."))
    write_parquet(clinical_data_processed, output_file)
}

main()
