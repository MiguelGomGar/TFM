# Packages ----
library(tidyverse)
library(haven)
library(labelled)
library(stringr)

# Data loading ----
original_dataset <- read_dta(file = "data/raw/predimar_miguelgomez.dta")

predimar_unlabelled <- original_dataset |> 
    remove_labels() 

excluded_patients <- load("data/processed/patients_excluded.RData")

write.csv(
    predimar_unlabelled,
    file = "data/processed/predimar_original.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
)

# Selecting relevant variables and filtering excluded records ----
predimar_selected <- predimar_unlabelled |> 
    select(
        1:6,
        event18m_conkardia,
        
        starts_with("p14_tot"),
        starts_with("ac_oliva"),
        starts_with("ac_olivavir"),
        starts_with("olivatot"),
        starts_with("ac_orujo"),
        starts_with("ac_maiz"),
        starts_with("ac_girasol"),
        starts_with("ac_soja"),
        starts_with("ac_mezcla"),
        starts_with("v_blanco"),
        starts_with("v_tinto"),
        starts_with("v_rosado"),
        starts_with("v_moscatel"),
        starts_with("cervezas"),
        starts_with("alcoholg"),
        
        phenetanol4glucur0:ConcTyrosolngmL1,
        -starts_with("Conc")
    ) |> 
    filter(!(code %in% excluded_patients))

# Factors encoding ----
predimar_modified <- predimar_selected |> 
    mutate(
        tipofa = factor(
            tipofa,
            levels = c(0, 1),
            labels = c("persistent", "paroxysmal")
        ),
        interv = factor(
            interv,
            levels = c(0, 1),
            labels = c("control", "intervention")
        ),
        nodo = factor(
            nodo,
            levels = c(1, 2, 3, 4),
            labels = c("Pamplona", "Madrid", "Granada", "Alicante")
        ),
        sexo = factor(
            sexo,
            levels = c(0, 1),
            labels = c("male", "female")     
        ),
        event18m_conkardia = factor(
            event18m_conkardia,
            levels = c(0, 1),
            labels = c("yes", "no")
        )
    )

# Long pivoting (p14) ----
predimar_p14_long <- predimar_modified |> 
    select(
        code:edad,
        starts_with("p14")
        ) |> 
    pivot_longer(
        cols          = starts_with("p14"),
        names_to      = "visit",
        values_to     = "p14",
        names_pattern = "p14_tot_(\\d+)m"
    ) |> 
    mutate(
        visit = as_factor(visit)
    )

# Long pivoting (metabolites concentrations) ----
predimar_polyphenols_long <- predimar_modified |> 
    select(-starts_with("p14")) |>
    mutate(across(phenetanol4glucur0:last_col(), as.numeric)) |> 
    pivot_longer(
        cols      = phenetanol4glucur0:last_col(),
        names_to  = "compound_visit",
        values_to = "conc_umol_molcreat"
    )|>
    mutate(
        visit = str_extract(compound_visit, "\\d+$"),
        visit = as_factor(case_match(
            visit,
            "0" ~ "0",
            "1" ~ "12",
            "2" ~ "24",
            "3" ~ "36",
            "4" ~ "48"
        )),
        compound = str_replace(compound_visit, "\\d+$", ""),
        compound = case_match(
            compound,
            "phenetanol4glucur"           ~ "Tyrosol-glucuronide",
            "metphenethanol4sulfate"      ~ "Homovanillyl_alcohol-sulfate",
            "hydrphenethanol4sulfate"     ~ "Hydroxytyrosol-4-sulfate",
            "hydrphenethanol3sulfate"     ~ "Hydroxytyrosol-3-sulfate",
            "phelethanol4sulfate"         ~ "Tyrosol-sulfate",
            "dihydrphenethanol"           ~ "Hydroxytyrosol",
            "hydrphenethanol3glucuronide" ~ "Hydroxytyrosol-3-glucuronide",
            "hydrphenethanol4glucuronide" ~ "Hydroxytyrosol-4-glucuronide",
            .default = compound
        ),
        compound = as_factor(compound)
    ) |> 
    relocate(visit, .after = sexo) |> 
    select(-compound_visit)

# Long pivoting (cardiac event) ----
predimar_event_long <- predimar_modified |> 
    select(code:event18m_conkardia) |> 
    pivot_longer(
        cols      = event18m_conkardia,
        names_to  = "visit",
        values_to = "event_conkardia"
    ) |> 
    mutate(visit = "18")

# Inner join p14 & metabolites ----
keys_long <- c("code", "tipofa", "interv", "nodo", "sexo", "visit", "edad")

predimar_long <- predimar_p14_long |> 
    inner_join(
        predimar_polyphenols_long, 
        by = keys_long
        ) |> 
    rename(
        "af_type" = tipofa,
        "group"   = interv,
        "node"    = nodo,
        "sex"     = sexo,
        "age"     = edad
    )

write.csv(
    x    = predimar_long,
    file = "data/processed/predimar_long.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
    )

# Wide pivoting (metabolites) ----
predimar_polyphenols_wide <- predimar_long |> 
    select(-p14) |> 
    mutate(
        compound_visit = paste0(
            as.character(compound),
            "_",
            as.character(visit),
            "m"
            ),
        compound = NULL,
        visit    = NULL,
        ) |> 
    pivot_wider(
        names_from  = compound_visit,
        values_from = conc_umol_molcreat
    )

# Wide pivoting (p14) ----
predimar_p14_wide <- predimar_long |> 
    select(code, af_type, group, node, sex, edad, visit, p14) |> 
    distinct() |> 
    mutate(
        p14_visit = paste0("p14_", visit, "m"), 
        visit     = NULL
    ) |> 
    pivot_wider(
        names_from  = p14_visit,
        values_from = p14
    )

# Inner join ----
keys_wide <- c("code", "af_type", "group", "node", "sex", "edad")

predimar_wide <- predimar_polyphenols_wide |> 
    inner_join(
        predimar_p14_wide,
        by = keys_wide
    ) |> 
    mutate(
        Hydroxytyrosol_derivatives = across(contains("Hydroxytyrosol"))
    )
    
write.csv(
    x    = predimar_wide,
    file = "data/processed/predimar_wide.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
)