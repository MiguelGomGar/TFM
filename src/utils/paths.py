from pathlib import Path


PROJECT_ROOT = Path(__file__).resolve().parents[1]

DATA_DIR = PROJECT_ROOT / "data"
RAW_DATA_DIR = DATA_DIR / "raw"
INTERMEDIATE_DATA_DIR = DATA_DIR / "intermediate"
CLEAN_DATA_DIR = DATA_DIR / "clean"

RESULTS_DIR = PROJECT_ROOT / "results"
DATA_COLLECTION_DIR = RESULTS_DIR / "data_collection"
EDA_DIR = RESULTS_DIR / "eda"
CLINICAL_EDA_DIR = EDA_DIR / "clinical_features"
PROTEOMIC_EDA_DIR = EDA_DIR / "proteomic_features"
RISK_SCORES_VALIDATION_DIR = RESULTS_DIR / "risk_scores_validation"
MODELS_DIR = RESULTS_DIR / "models"
CLINICAL_MODELS_DIR = MODELS_DIR / "clinical_data"
CLINICAL_MODELS_FILTERED_DIR = MODELS_DIR / "clinical_data_filtered"
PROTEOMIC_MODELS_DIR = MODELS_DIR / "proteomic_data"