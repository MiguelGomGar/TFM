"""Project-wide configuration constants."""

import numpy as np
from scipy.stats import uniform, loguniform, randint

from src.utils.paths import (
    CLINICAL_FILTERING_COMPARISON_DIR,
    CLINICAL_MODELS_DIR,
    CLINICAL_MODELS_FILTERED_DIR,
    CLINICAL_MODELS_MATCHED_DIR,
    MODALITY_COMPARISON_DIR,
    MULTIMODAL_MODELS_DIR,
    PROTEOMIC_MODELS_DIR,
)

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
# Physiologically plausible range for each numeric feature, as (minimum, maximum).
# Values outside these bounds are recording errors rather than real variability,
# so they are set to missing and left to the imputation step. The bounds are
# deliberately wide: they are meant to catch impossible values, not outliers.
PLAUSIBLE_RANGES = {
    "AF_duration": (0.0, 60.0),  # years; cannot precede the diagnosis
}

TARGET_VARIABLE = "AF_recurrence"
GROUP_ALLOCATION_VARIABLE = "group_allocation"
STRATIFY_VARIABLES = [TARGET_VARIABLE, GROUP_ALLOCATION_VARIABLE]
NON_NORMAL_VARIABLES = ["AF_duration", "triglycerides", "glucose"]

# %% STATISTICAL ANALYSIS — pipeline 06_statistical_reporting

# %% RISK SCORES EVALUATION — pipeline 07_risk_scores_validation
TARGET_ENCODING = {"no": 0, "yes": 1}

# %% MODELLING — shared configuration (pipelines 09 to 14)
# Single random seed for the whole project: data splitting, cross-validation,
# imputation and every estimator that accepts a random_state.
SEED = 42

# Hold-out external validation set (80/20 split, stratified by the target).
TEST_SIZE = 0.2

# Internal cross-validation used to assess overfitting within the training set.
CV_N_SPLITS = 5

# Parallelism for the randomized search (-1 uses every available core).
N_JOBS = -1

# Metrics reported in every phase. Keys are the labels used in plots and CSV
# files; values are the scikit-learn scorer names.
SCORING_METRICS = {
    "ROC-AUC": "roc_auc",
    "PR-AUC": "average_precision",
}

# Metric the randomized search maximizes (see the methodology).
OBJECTIVE_METRIC = "PR-AUC"

# Models fitted in every phase, in reporting order.
MODEL_ORDER = ["EN", "SVM", "DT", "RF", "ET", "AB", "GB", "MLP"]

MODEL_DISPLAY_NAMES = {
    "EN": "Elastic Net",
    "SVM": "Support Vector Machine",
    "DT": "Decision Tree",
    "RF": "Random Forest",
    "ET": "Extra Trees",
    "AB": "AdaBoost",
    "GB": "Gradient Boosting",
    "MLP": "Multilayer Perceptron",
}

# Models that need standardized features (get_full_preprocessor); the remaining
# tree-based models use get_trees_preprocessor.
SCALED_MODELS = ["EN", "SVM", "MLP"]

# Model used to derive the feature selection filter through its regularization.
FEATURE_SELECTION_MODEL = "EN"

# Randomized search budget per modelling phase. The high-dimensional phases use
# a smaller budget because each candidate is much more expensive to fit.
SEARCH_N_ITER = {
    "clinical": 200,
    "clinical_filtered": 200,
    "proteomic": 50,
    "clinical_matched": 200,
    "multimodal": 50,
}

# BASE-AF2 performance, the best clinical risk score, used as the reference line
# in the external validation bar plots.
RISK_SCORE_BASELINE = {"ROC-AUC": 0.632, "PR-AUC": 0.480}

# Plot colors for the modelling results.
INTERNAL_VALIDATION_COLORS = {"Train": "#2563eb", "Validation": "#f59e0b"}
BASELINE_COLOR = "#e11d48"
MODEL_BAR_COLOR = "#16a085"
MODALITY_COLORS = {
    "Clinical": "#16a085",
    "Multimodal": "#f59e0b",
    "Clinical filtered": "#16a085",
    "Proteomic": "#2563eb",
}
MODEL_PALETTE = [
    "#2563eb",
    "#e11d48",
    "#16a085",
    "#f59e0b",
    "#7c3aed",
    "#0891b2",
    "#65a30d",
    "#1e3a8a",
]

# %% CLINICAL DATA MODELLING — pipelines 09 and 10
clinical_hyperparameters_search_space = {
    # Elastic Net Logistic Regression
    # l1_ratio must stay strictly above 0: since scikit-learn 1.8 the penalty
    # type is derived from l1_ratio alone (the `penalty` argument is deprecated),
    # and l1_ratio=0 would mean a pure L2 penalty, which never drives any
    # coefficient to zero and would silently disable the feature selection step.
    "EN": {
        # Avoid very weak regularization values (large C)
        "clf__l1_ratio": uniform(0.1, 0.9),
        "clf__C": loguniform(1e-3, 1e1),
    },
    # Support Vector Machine
    "SVM": {
        "clf__C": loguniform(1e-3, 1e2),
        "clf__kernel": ["linear", "rbf"],
        "clf__gamma": ["scale", "auto"] + list(np.logspace(-5, -1, 25)),
        "clf__class_weight": [None, "balanced"],
    },
    # Decision Tree
    "DT": {
        "clf__max_depth": randint(2, 13),
        "clf__min_samples_split": randint(8, 41),
        "clf__min_samples_leaf": randint(4, 21),
        "clf__criterion": ["gini", "entropy"],
        "clf__ccp_alpha": loguniform(1e-6, 1e-2),
    },
    # Random Forest
    "RF": {
        "clf__max_depth": [None] + list(range(3, 13)),
        "clf__min_samples_split": randint(8, 31),
        "clf__min_samples_leaf": randint(3, 16),
        "clf__criterion": ["gini", "entropy"],
        "clf__ccp_alpha": loguniform(1e-7, 1e-3),
        "clf__n_estimators": randint(100, 401),
        "clf__max_features": ["sqrt", "log2"] + list(np.arange(0.2, 0.7, 0.1)),
        "clf__class_weight": [None, "balanced", "balanced_subsample"],
        "clf__max_samples": list(np.arange(0.6, 1.0, 0.1)),
    },
    # Extra Trees
    "ET": {
        "clf__max_depth": [None] + list(range(3, 13)),
        "clf__min_samples_split": randint(8, 31),
        "clf__min_samples_leaf": randint(3, 16),
        "clf__criterion": ["gini", "entropy"],
        "clf__ccp_alpha": loguniform(1e-7, 1e-3),
        "clf__n_estimators": randint(100, 401),
        "clf__max_features": ["sqrt", "log2"] + list(np.arange(0.2, 0.7, 0.1)),
        "clf__class_weight": [None, "balanced", "balanced_subsample"],
        "clf__bootstrap": [True],
        "clf__max_samples": list(np.arange(0.6, 1.0, 0.1)),
    },
    # Adaptive Boosting
    "AB": {
        "clf__estimator__max_depth": randint(1, 3),
        "clf__estimator__max_features": ["sqrt", "log2", None],
        "clf__estimator__min_samples_split": randint(10, 41),
        "clf__estimator__min_samples_leaf": randint(4, 16),
        "clf__estimator__criterion": ["gini", "entropy"],
        "clf__estimator__ccp_alpha": loguniform(1e-7, 1e-3),
        "clf__n_estimators": randint(50, 301),
        "clf__learning_rate": loguniform(0.01, 0.3),
    },
    # Gradient Boosting
    "GB": {
        "clf__max_depth": randint(2, 4),
        "clf__max_features": ["sqrt", "log2"] + list(np.arange(0.2, 0.7, 0.1)),
        "clf__min_samples_split": randint(8, 31),
        "clf__min_samples_leaf": randint(3, 16),
        "clf__n_estimators": randint(80, 351),
        "clf__learning_rate": loguniform(0.01, 0.2),
        "clf__subsample": uniform(0.6, 0.3),
    },
    # Multilayer Perceptron
    "MLP": {
        "clf__hidden_layer_sizes": [(32,), (64,), (64, 32), (128, 64)],
        "clf__alpha": loguniform(1e-4, 1e0),
        "clf__learning_rate_init": loguniform(1e-4, 5e-3),
        "clf__batch_size": [16, 32, 64, 128],
        "clf__activation": ["relu", "tanh", "logistic"],
        "clf__solver": ["adam", "sgd"],
    },
}

# %% PROTEOMIC DATA MODELLING — pipeline 11
proteomic_hyperparameters_search_space = {
    # Elastic Net Logistic Regression (see the note on l1_ratio above)
    "EN": {"clf__l1_ratio": uniform(0.01, 0.98), "clf__C": loguniform(1e-4, 1e2)},
    # Support Vector Machine
    "SVM": {
        "clf__C": loguniform(1e-3, 1e3),
        "clf__kernel": ["linear", "rbf"],
        "clf__gamma": ["scale", "auto"] + list(np.logspace(-4, -1, 10)),
        "clf__class_weight": [None, "balanced"],
    },
    # Decision Tree
    "DT": {
        "clf__max_depth": randint(2, 11),
        "clf__min_samples_split": randint(5, 21),
        "clf__min_samples_leaf": randint(3, 15),
        "clf__criterion": ["gini", "entropy"],
    },
    # Random Forest
    "RF": {
        "clf__n_estimators": randint(50, 301),
        "clf__max_depth": randint(3, 13),
        "clf__min_samples_split": randint(5, 21),
        "clf__min_samples_leaf": randint(3, 15),
        "clf__max_features": ["sqrt", "log2"],
        "clf__bootstrap": [True],
        "clf__max_samples": uniform(0.5, 0.4),
    },
    # Extra Trees
    "ET": {
        "clf__n_estimators": randint(50, 301),
        "clf__max_depth": randint(3, 13),
        "clf__min_samples_split": randint(5, 21),
        "clf__min_samples_leaf": randint(3, 15),
        "clf__max_features": ["sqrt", "log2"],
        "clf__bootstrap": [True],
        "clf__max_samples": uniform(0.5, 0.4),
    },
    # AdaBoost
    "AB": {
        "clf__n_estimators": randint(50, 301),
        "clf__learning_rate": loguniform(1e-3, 0.5),
        "clf__estimator__max_depth": randint(1, 3),
    },
    # Gradient Boosting
    "GB": {
        "clf__n_estimators": randint(50, 301),
        "clf__max_depth": randint(2, 6),
        "clf__learning_rate": loguniform(1e-3, 0.2),
        "clf__subsample": uniform(0.5, 0.4),
        "clf__min_samples_leaf": randint(3, 15),
        "clf__max_features": ["sqrt", "log2"],
    },
    # Multi-layer Perceptron (Neural Network)
    "MLP": {
        "clf__hidden_layer_sizes": [(50,), (100,), (50, 50)],
        "clf__activation": ["relu", "tanh"],
        "clf__alpha": loguniform(1e-4, 1e0),
        "clf__learning_rate_init": loguniform(1e-4, 1e-2),
        "clf__early_stopping": [True],
    },
}

# %% MULTIMODAL DATA MODELLING — pipeline 13
# The integrated dataset has a dimensionality comparable to the proteomic one
# (26 clinical predictors plus 361 proteins), so the same ranges apply. It is
# defined explicitly rather than aliased so that it can diverge later without
# side effects on the proteomic phase.
multimodal_hyperparameters_search_space = {
    # Elastic Net Logistic Regression (see the note on l1_ratio above)
    "EN": {"clf__l1_ratio": uniform(0.01, 0.98), "clf__C": loguniform(1e-4, 1e2)},
    # Support Vector Machine
    "SVM": {
        "clf__C": loguniform(1e-3, 1e3),
        "clf__kernel": ["linear", "rbf"],
        "clf__gamma": ["scale", "auto"] + list(np.logspace(-4, -1, 10)),
        "clf__class_weight": [None, "balanced"],
    },
    # Decision Tree
    "DT": {
        "clf__max_depth": randint(2, 11),
        "clf__min_samples_split": randint(5, 21),
        "clf__min_samples_leaf": randint(3, 15),
        "clf__criterion": ["gini", "entropy"],
    },
    # Random Forest
    "RF": {
        "clf__n_estimators": randint(50, 301),
        "clf__max_depth": randint(3, 13),
        "clf__min_samples_split": randint(5, 21),
        "clf__min_samples_leaf": randint(3, 15),
        "clf__max_features": ["sqrt", "log2"],
        "clf__class_weight": [None, "balanced", "balanced_subsample"],
        "clf__bootstrap": [True],
        "clf__max_samples": uniform(0.5, 0.4),
    },
    # Extra Trees
    "ET": {
        "clf__n_estimators": randint(50, 301),
        "clf__max_depth": randint(3, 13),
        "clf__min_samples_split": randint(5, 21),
        "clf__min_samples_leaf": randint(3, 15),
        "clf__max_features": ["sqrt", "log2"],
        "clf__class_weight": [None, "balanced", "balanced_subsample"],
        "clf__bootstrap": [True],
        "clf__max_samples": uniform(0.5, 0.4),
    },
    # AdaBoost
    "AB": {
        "clf__n_estimators": randint(50, 301),
        "clf__learning_rate": loguniform(1e-3, 0.5),
        "clf__estimator__max_depth": randint(1, 3),
    },
    # Gradient Boosting
    "GB": {
        "clf__n_estimators": randint(50, 301),
        "clf__max_depth": randint(2, 6),
        "clf__learning_rate": loguniform(1e-3, 0.2),
        "clf__subsample": uniform(0.5, 0.4),
        "clf__min_samples_leaf": randint(3, 15),
        "clf__max_features": ["sqrt", "log2"],
    },
    # Multi-layer Perceptron (Neural Network)
    "MLP": {
        "clf__hidden_layer_sizes": [(50,), (100,), (50, 50)],
        "clf__activation": ["relu", "tanh"],
        "clf__alpha": loguniform(1e-4, 1e0),
        "clf__learning_rate_init": loguniform(1e-4, 1e-2),
        "clf__early_stopping": [True],
    },
}

# %% MODALITY COMPARISON — pipeline 14
# Each entry compares two or more modelling arms trained on the same cohort
# and split. "extra_arms" lists additional arms (label, metrics_file) shown
# between the baseline and the comparison arm, purely for context: the
# saved delta table and its plotted values still contrast baseline vs
# comparison only.
MODALITY_COMPARISONS = [
    {
        "baseline_label": "Clinical",
        "baseline_metrics_file": CLINICAL_MODELS_MATCHED_DIR / "models_metrics.csv",
        "comparison_label": "Multimodal",
        "comparison_metrics_file": MULTIMODAL_MODELS_DIR / "models_metrics.csv",
        "extra_arms": [("Proteomic", PROTEOMIC_MODELS_DIR / "models_metrics.csv")],
        "output_dir": MODALITY_COMPARISON_DIR,
    },
    {
        "baseline_label": "Clinical",
        "baseline_metrics_file": CLINICAL_MODELS_DIR / "models_metrics.csv",
        "comparison_label": "Clinical filtered",
        "comparison_metrics_file": CLINICAL_MODELS_FILTERED_DIR / "models_metrics.csv",
        "output_dir": CLINICAL_FILTERING_COMPARISON_DIR,
    },
]

# %% PUBLICATION TABLES — pipeline 14
# Each entry is one modelling phase: where its per-model internal-validation
# CSVs live, the phase prefix used to name its hyperparameters tables, and
# the filename prefix of the performance tables it produces (one file per
# metric, see PUBLICATION_METRICS).
PERFORMANCE_PHASES = [
    {
        "label": "Clinical",
        "input_dir": CLINICAL_MODELS_DIR,
        "prefix": "clinical",
        "output_prefix": "performance_clinical",
    },
    {
        "label": "Clinical filtered",
        "input_dir": CLINICAL_MODELS_FILTERED_DIR,
        "prefix": "clinical_filtered",
        "output_prefix": "performance_clinical_filtered",
    },
    {
        "label": "Proteomic",
        "input_dir": PROTEOMIC_MODELS_DIR,
        "prefix": "proteomic",
        "output_prefix": "performance_proteomic",
    },
    {
        "label": "Multimodal",
        "input_dir": MULTIMODAL_MODELS_DIR,
        "prefix": "multimodal",
        "output_prefix": "performance_multimodal",
    },
]

# Each entry reuses the delta table already saved by pipeline 13 and reshapes
# it into a wide, manuscript-ready comparison table (one file per metric, see
# PUBLICATION_METRICS).
COMPARISON_PHASES = [
    {
        "label": "Clinical vs Multimodal",
        "delta_file": MODALITY_COMPARISON_DIR / "modality_comparison.csv",
        "baseline_label": "Clinical",
        "comparison_label": "Multimodal",
        "output_prefix": "comparison_clinical_vs_multimodal",
    },
    {
        "label": "Clinical vs Clinical filtered",
        "delta_file": CLINICAL_FILTERING_COMPARISON_DIR / "modality_comparison.csv",
        "baseline_label": "Clinical",
        "comparison_label": "Clinical filtered",
        "output_prefix": "comparison_clinical_vs_filtered",
    },
]

# Metric name (as used across the modelling CSVs) mapped to the filename slug
# used when splitting each publication table into one file per metric.
PUBLICATION_METRICS = {
    "ROC-AUC": "roc_auc",
    "PR-AUC": "pr_auc",
}

PUBLICATION_TABLES_DECIMALS = 3
