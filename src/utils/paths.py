"""Central filesystem paths used across the project."""

from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[2]

DATA_DIR = PROJECT_ROOT / "data"
RESULTS_DIR = PROJECT_ROOT / "results"
LOGS_DIR = PROJECT_ROOT / "logs"

RAW_DATA_DIR = DATA_DIR / "raw"
INTERMEDIATE_DATA_DIR = DATA_DIR / "intermediate"
CLEAN_DATA_DIR = DATA_DIR / "clean"
DATA_COLLECTION_DIR = RESULTS_DIR / "data_collection"
EDA_DIR = RESULTS_DIR / "eda"
CLINICAL_EDA_DIR = EDA_DIR / "clinical_features"
PROTEOMIC_EDA_DIR = EDA_DIR / "proteomic_features"
RISK_SCORES_VALIDATION_DIR = RESULTS_DIR / "risk_scores_validation"
MODELS_DIR = RESULTS_DIR / "models"
CLINICAL_MODELS_DIR = MODELS_DIR / "clinical_data"
CLINICAL_MODELS_FILTERED_DIR = MODELS_DIR / "clinical_data_filtered"
PROTEOMIC_MODELS_DIR = MODELS_DIR / "proteomic_data"
CLINICAL_MODELS_MATCHED_DIR = MODELS_DIR / "clinical_data_matched"
MULTIMODAL_MODELS_DIR = MODELS_DIR / "multimodal_data"
MODALITY_COMPARISON_DIR = MODELS_DIR / "modality_comparison"
CLINICAL_FILTERING_COMPARISON_DIR = MODELS_DIR / "clinical_filtering_comparison"

RISK_FACTORS_REVIEW_DATA_PATH = RAW_DATA_DIR / "clinical_variables_review.xlsx"
RAW_CLINICAL_DATA_PATH = RAW_DATA_DIR / "predimar_miguelgomez.dta"
INTERMEDIATE_CLINICAL_DATA_PATH = INTERMEDIATE_DATA_DIR / "clinical_data.parquet"
RISK_SCORES_DATA_PATH = CLEAN_DATA_DIR / "risk_scores_data.parquet"
CLEANED_CLINICAL_DATA_PATH = CLEAN_DATA_DIR / "clinical_data.parquet"
RAW_PROTEOMIC_DATA_PATH = RAW_DATA_DIR / "olink_baseline_wide.csv"
CLEANED_PROTEOMIC_DATA_PATH = CLEAN_DATA_DIR / "proteomic_data.parquet"
CLEANED_MULTIMODAL_DATA_PATH = CLEAN_DATA_DIR / "multimodal_data.parquet"
CLEANED_CLINICAL_MATCHED_DATA_PATH = CLEAN_DATA_DIR / "clinical_data_matched.parquet"

MISSING_VALUES_PER_FEATURE_PLOT_PATH = (
    CLINICAL_EDA_DIR / "missing_values_per_feature.png"
)
MISSING_VALUES_PER_RECORD_BEFORE_PLOT_PATH = (
    CLINICAL_EDA_DIR / "missing_values_per_record_before.png"
)
MISSING_VALUES_PER_RECORD_AFTER_PLOT_PATH = (
    CLINICAL_EDA_DIR / "missing_values_per_record_after.png"
)
NUMERIC_CORRELATION_MATRIX_PLOT_PATH = CLINICAL_EDA_DIR / "correlation_matrix_num.png"
CATEGORICAL_CORRELATION_MATRIX_PLOT_PATH = (
    CLINICAL_EDA_DIR / "correlation_matrix_cat.png"
)
VIF_PLOT_PATH = CLINICAL_EDA_DIR / "vif_plot.png"
TABLE1_PATH = CLINICAL_EDA_DIR / "table1.csv"
RISK_SCORES_METRICS_PATH = RISK_SCORES_VALIDATION_DIR / "risk_scores_metrics.csv"
RISK_SCORES_ROC_CURVE_PATH = RISK_SCORES_VALIDATION_DIR / "risk_scores_roc_curve.png"
RISK_SCORES_PR_CURVE_PATH = (
    RISK_SCORES_VALIDATION_DIR / "risk_scores_precision_recall_curve.png"
)
