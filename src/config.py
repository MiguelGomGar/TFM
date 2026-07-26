"""Project-wide configuration constants."""

from src.utils.paths import RAW_DATA_DIR, INTERMEDIATE_DATA_DIR, CLEAN_DATA_DIR

# %% RISK FACTORS REVIEW — pipeline 01_clinical_variables_review
VARIABLES_REVIEW_COLOR_MAP = {
    "yes": "#10b981",
    "related": "#f59e0b",
    "disaggregated": "#3b82f6",
    "no": "#ef4444",
}
VARIABLES_REVIEW_SORT_LEVELS = ["no", "disaggregated", "related", "yes"]
VARIABLES_REVIEW_ORIGINAL_LEVELS = ["yes", "related", "disaggregated", "no"]
VARIABLES_REVIEW_DEFAULT_COLOR = "#999999"
VARIABLES_REVIEW_USECOLS = "A:C"
VARIABLES_REVIEW_NROWS = 27
VARIABLES_REVIEW_EXPECTED_COLUMNS = {"Variable", "Predimar", "Scores"}

# %% DATA COLLECTION — pipeline 02_data_collection
SELECTED_FEATURES = [
    "sexo",  # sex
    "edad",  # age
    "fum_0m",  # smoking status
    "code",  # patient code
    "interv",  # intervention
    "bmi_autoref_0m",  # body mass index
    "glu_new0m",  # glucose
    "diabetest2_0m",  # type 2 diabetes
    "hdl_new0m",  # HDL cholesterol
    "ldl_new0m",  # LDL cholesterol
    "trig_new0m",  # triglycerides
    "hipolipemiantes_0m",  # lipid-lowering agents
    "saos_0m",  # obstructive sleep apnea
    "insurenal_0m",  # renal disease
    "fglopre_MDRD_cal_0m",  # estimated glomerular filtration rate (MDRD formula)
    "fglopre_CockroftGault_cal_0m",  # estimated glomerular filtration rate (Cockroft-Gault formula)
    "hta_0m",  # hypertension
    "epoc_0m",  # chronic obstructive pulmonary disease
    "ictus_0m",  # stroke
    "bri_0m",  # bundle branch block (left)
    "brd_0m",  # bundle branch block (right)
    "ic_v00",  # heart failure
    "mio_isq_0m",  # coronary artery disease
    "miocardiopat_0m_imp",  # myocardiopathy
    "faa_0m_new",  # antiarrhythmic drugs
    "dilat_tot4_imp",  # atrial enlargement
    "eco_diamai_0m",  # atrial diameter
    "eco_areaai_0m",  # atrial area
    "eco_volumenai_0m",  # atrial volume
    "eco_fe_0m",  # ejection fraction
    "tipofa",  # atrial fibrillation type
    "tiempo_fa_ablac",  # time between atrial fibrillation diagnosis and ablation
    "ablacion_previa",  # previous ablation
    "event_blank",  # early recurrence of atrial fibrillation
    "event18m_conkardia",  # atrial fibrillation between 3 and 18 months since ablation
    "chad2ds2vasc_v00",  # chad2ds2vasc score
]

NEW_FEATURES_NAMES = {
    "sexo": "sex",
    "edad": "age",
    "fum_0m": "smoking_status",
    "code": "code",
    "interv": "group_allocation",
    "bmi_autoref_0m": "BMI",
    "glu_new0m": "glucose",
    "diabetest2_0m": "diabetes",
    "hdl_new0m": "HDL",
    "ldl_new0m": "LDL",
    "trig_new0m": "triglycerides",
    "hipolipemiantes_0m": "LLA",
    "saos_0m": "OSA",
    "insurenal_0m": "renal_dysfunction",
    "fglopre_MDRD_cal_0m": "eGFR_MDRD",
    "fglopre_CockroftGault_cal_0m": "eGFR_CG",
    "hta_0m": "hypertension",
    "epoc_0m": "COPD",
    "ictus_0m": "stroke",
    "bri_0m": "LBBB",
    "brd_0m": "RBBB",
    "ic_v00": "heart_failure",
    "mio_isq_0m": "CAD",
    "miocardiopat_0m_imp": "cardiomyopathy",
    "faa_0m_new": "AAD",
    "dilat_tot4_imp": "LA_enlargment",
    "eco_diamai_0m": "LAD",
    "eco_areaai_0m": "LAA",
    "eco_volumenai_0m": "LAV",
    "eco_fe_0m": "LVEF",
    "tipofa": "AF_type",
    "tiempo_fa_ablac": "AF_duration",
    "ablacion_previa": "previous_ablation",
    "event_blank": "ERAF",
    "event18m_conkardia": "AF_recurrence",
    "chad2ds2vasc_v00": "score_chad2ds2_vasc",
}

CATEGORICAL_SPECS = {
    "sex": {
        "mapping": {"hombre": "male", "mujer": "female"},
        "order": ["male", "female"],
    },
    "smoking_status": {
        "mapping": {
            "Fumador activo": "current",
            "Exfumador": "former",
            "No fumador": "never",
        },
        "order": ["never", "former", "current"],
    },
    "group_allocation": {
        "mapping": {"Intervencion": "intervention", "Control": "control"},
        "order": ["control", "intervention"],
    },
    "diabetes": {
        "mapping": {"no": "no", "si": "yes"},
        "order": ["no", "yes"],
    },
    "LLA": {
        "mapping": {"no": "no", "si": "yes"},
        "order": ["no", "yes"],
    },
    "OSA": {
        "mapping": {"no": "no", "si": "yes"},
        "order": ["no", "yes"],
    },
    "renal_dysfunction": {
        "mapping": {"no": "no", "si": "yes"},
        "order": ["no", "yes"],
    },
    "hypertension": {
        "mapping": {"no": "no", "si": "yes"},
        "order": ["no", "yes"],
    },
    "COPD": {
        "mapping": {"no": "no", "si": "yes"},
        "order": ["no", "yes"],
    },
    "stroke": {
        "mapping": {"no": "no", "si": "yes"},
        "order": ["no", "yes"],
    },
    "LBBB": {
        "mapping": {"no": "no", "si": "yes"},
        "order": ["no", "yes"],
    },
    "RBBB": {
        "mapping": {"no": "no", "si": "yes"},
        "order": ["no", "yes"],
    },
    "heart_failure": {
        "mapping": {0: "no", 1: "yes"},
        "order": ["no", "yes"],
    },
    "CAD": {
        "mapping": {"no": "no", "si": "yes"},
        "order": ["no", "yes"],
    },
    "cardiomyopathy": {
        "mapping": {0: "no", 1: "yes"},
        "order": ["no", "yes"],
    },
    "AAD": {
        "mapping": {0: "no", 1: "yes"},
        "order": ["no", "yes"],
    },
    "LA_enlargment": {
        "mapping": {0: "normal", 1: "mild", 2: "moderate", 3: "severe"},
        "order": ["normal", "mild", "moderate", "severe"],
    },
    "LVEF": {
        "mapping": {
            "normal": "normal",
            "ligeramente anormal": "mild",
            "moderadamente anormal": "moderate",
            "severamente anormal": "severe",
        },
        "order": ["normal", "mild", "moderate", "severe"],
    },
    "AF_type": {
        "mapping": {
            "Paroxistica documentada": "paroxysmal",
            "Persistente": "persistent",
        },
        "order": ["paroxysmal", "persistent"],
    },
    "previous_ablation": {
        "mapping": {0: "no", 1: "yes"},
        "order": ["no", "yes"],
    },
    "ERAF": {
        "mapping": {0: "no", 1: "yes"},
        "order": ["no", "yes"],
    },
    "AF_recurrence": {
        "mapping": {0: "no", 1: "yes"},
        "order": ["no", "yes"],
    },
}

# %% MISSING DATA ANALYSIS — pipeline 03_missing_values_reporting
MISSING_RATE_THRESHOLD = 0.20
CATEGORICAL_COLOR_MAP = ["#2563eb", "#e11d48"]


# %% COLLINEARITY ANALYSIS — pipeline 04_collinearity_reporting
IDENTIFIER_VARIABLE = "code"
RISK_SCORES_PREFIX = "score"
HIGHLY_CORRELATED_FEATURES = "heart_failure"
VIF_THRESHOLD = 5.0

# %% CLINICAL DATA CLEANING — pipeline 05_clinical_data_cleaning
TARGET_VARIABLE = "AF_recurrence"
GROUP_ALLOCATION_VARIABLE = "group_allocation"
STRATIFY_VARIABLES = [TARGET_VARIABLE, GROUP_ALLOCATION_VARIABLE]
NON_NORMAL_VARIABLES = ["AF_duration", "triglycerides", "glucose"]

# %% STATISTICAL ANALYSIS — pipeline 06_statistical_reporting

# %% RISK SCORES EVALUATION — pipeline 07_risk_scores_validation
TARGET_ENCODING = {"no": 0, "yes": 1}

# %% CLINICAL DATA MODELLING — pipeline 08_clinical_data_modelling
SEED = 42

# %% PROTEOMIC DATA MODELLING — pipeline 09_proteomic_data_modelling

# %% MULTIMODAL DATA MODELLING
