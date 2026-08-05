"""Central filesystem paths used across the project.

Every path is derived from a single project root rather than hard-coded, so the
tree resolves identically no matter which directory the code is launched from.
``ProjectPaths`` is frozen and takes its root as a field, which also lets tests
point the whole tree at a temporary directory by instantiating it with a
different root.

The module-level constants at the bottom bind the canonical instance and are the
interface the rest of the project imports.
"""

from dataclasses import dataclass
from pathlib import Path

# src/utils/paths.py -> src/utils -> src -> project root
PROJECT_ROOT = Path(__file__).resolve().parents[2]


@dataclass(frozen=True)
class ProjectPaths:
    """Directories and data files the project reads from and writes to.

    Attributes
    ----------
    root : Path
        Project root every other path is derived from.
    """

    root: Path = PROJECT_ROOT

    # %% Top-level directories

    @property
    def data(self) -> Path:
        """Directory holding every dataset, raw through clean."""
        return self.root / "data"

    @property
    def results(self) -> Path:
        """Directory holding every table, figure and serialized model."""
        return self.root / "results"

    @property
    def logs(self) -> Path:
        """Directory holding the per-pipeline and orchestrator log files."""
        return self.root / "logs"

    # %% Data subdirectories

    @property
    def raw_data(self) -> Path:
        """Untouched source files as delivered by the PREDIMAR trial."""
        return self.data / "raw"

    @property
    def intermediate_data(self) -> Path:
        """Datasets after collection and renaming, before cleaning."""
        return self.data / "intermediate"

    @property
    def clean_data(self) -> Path:
        """Analysis-ready datasets consumed by the modelling pipelines."""
        return self.data / "clean"

    # %% Exploratory analysis result directories

    @property
    def data_collection(self) -> Path:
        """Outputs of the clinical variables review and data collection."""
        return self.results / "data_collection"

    @property
    def eda(self) -> Path:
        """Root of the exploratory data analysis outputs."""
        return self.results / "eda"

    @property
    def clinical_eda(self) -> Path:
        """Exploratory analysis of the clinical features."""
        return self.eda / "clinical_features"

    @property
    def proteomic_eda(self) -> Path:
        """Exploratory analysis of the proteomic features."""
        return self.eda / "proteomic_features"

    @property
    def risk_scores_validation(self) -> Path:
        """Validation of the published clinical risk scores."""
        return self.results / "risk_scores_validation"

    # %% Modelling result directories, one per phase

    @property
    def models(self) -> Path:
        """Root of every modelling output."""
        return self.results / "models"

    @property
    def clinical_models(self) -> Path:
        """Phase 1: models trained on the full clinical cohort."""
        return self.models / "clinical_data"

    @property
    def clinical_models_filtered(self) -> Path:
        """Phase 1b: clinical models after the feature selection filter."""
        return self.models / "clinical_data_filtered"

    @property
    def proteomic_models(self) -> Path:
        """Phase 2: models trained on the proteomic features."""
        return self.models / "proteomic_data"

    @property
    def clinical_models_matched(self) -> Path:
        """Clinical models refitted on the multimodal subcohort.

        Trained on the same patients and split as the multimodal arm, so the
        two are comparable and the proteomic contribution is isolated.
        """
        return self.models / "clinical_data_matched"

    @property
    def multimodal_models(self) -> Path:
        """Phase 3: models trained on clinical and proteomic features."""
        return self.models / "multimodal_data"

    @property
    def modality_comparison(self) -> Path:
        """Clinical vs multimodal comparison tables and figures."""
        return self.models / "modality_comparison"

    @property
    def clinical_filtering_comparison(self) -> Path:
        """Full vs filtered clinical comparison tables and figures."""
        return self.models / "clinical_filtering_comparison"

    @property
    def best_model(self) -> Path:
        """Threshold sensitivity analysis of the best-performing models."""
        return self.models / "best_model"

    @property
    def publication_tables(self) -> Path:
        """Manuscript-ready Excel workbooks."""
        return self.results / "publication_tables"

    # %% Individual data files

    @property
    def risk_factors_review_file(self) -> Path:
        """Literature review of the candidate clinical risk factors."""
        return self.raw_data / "clinical_variables_review.xlsx"

    @property
    def raw_clinical_file(self) -> Path:
        """Stata export of the PREDIMAR clinical database."""
        return self.raw_data / "predimar_miguelgomez.dta"

    @property
    def intermediate_clinical_file(self) -> Path:
        """Clinical data after selection and renaming, before cleaning."""
        return self.intermediate_data / "clinical_data.parquet"

    @property
    def risk_scores_file(self) -> Path:
        """Precomputed clinical risk scores per patient."""
        return self.clean_data / "risk_scores_data.parquet"

    @property
    def clean_clinical_file(self) -> Path:
        """Analysis-ready clinical dataset."""
        return self.clean_data / "clinical_data.parquet"

    @property
    def raw_proteomic_file(self) -> Path:
        """Olink baseline proteomic panel, wide format."""
        return self.raw_data / "olink_baseline_wide.csv"

    @property
    def clean_proteomic_file(self) -> Path:
        """Analysis-ready proteomic dataset."""
        return self.clean_data / "proteomic_data.parquet"

    @property
    def clean_multimodal_file(self) -> Path:
        """Clinical and proteomic features joined on the shared patients."""
        return self.clean_data / "multimodal_data.parquet"

    @property
    def clean_clinical_matched_file(self) -> Path:
        """Clinical features restricted to the multimodal subcohort."""
        return self.clean_data / "clinical_data_matched.parquet"

    # %% Individual result files

    @property
    def table1_file(self) -> Path:
        """Baseline characteristics table of the cohort."""
        return self.clinical_eda / "table1.csv"

    @property
    def risk_scores_metrics_file(self) -> Path:
        """Discrimination metrics of every clinical risk score."""
        return self.risk_scores_validation / "risk_scores_metrics.csv"

    @property
    def risk_scores_roc_curve_file(self) -> Path:
        """ROC curves of the clinical risk scores."""
        return self.risk_scores_validation / "risk_scores_roc_curve.png"

    @property
    def risk_scores_pr_curve_file(self) -> Path:
        """Precision-recall curves of the clinical risk scores."""
        return self.risk_scores_validation / "risk_scores_precision_recall_curve.png"


PATHS = ProjectPaths()

# %% Module-level bindings
# The canonical instance flattened into constants. Importing a name is cheaper
# to read at the call site than threading the instance through every function,
# and keeps the pipelines declarative.

DATA_DIR = PATHS.data
RESULTS_DIR = PATHS.results
LOGS_DIR = PATHS.logs

RAW_DATA_DIR = PATHS.raw_data
INTERMEDIATE_DATA_DIR = PATHS.intermediate_data
CLEAN_DATA_DIR = PATHS.clean_data
DATA_COLLECTION_DIR = PATHS.data_collection
EDA_DIR = PATHS.eda
CLINICAL_EDA_DIR = PATHS.clinical_eda
PROTEOMIC_EDA_DIR = PATHS.proteomic_eda
RISK_SCORES_VALIDATION_DIR = PATHS.risk_scores_validation
MODELS_DIR = PATHS.models
CLINICAL_MODELS_DIR = PATHS.clinical_models
CLINICAL_MODELS_FILTERED_DIR = PATHS.clinical_models_filtered
PROTEOMIC_MODELS_DIR = PATHS.proteomic_models
CLINICAL_MODELS_MATCHED_DIR = PATHS.clinical_models_matched
MULTIMODAL_MODELS_DIR = PATHS.multimodal_models
MODALITY_COMPARISON_DIR = PATHS.modality_comparison
CLINICAL_FILTERING_COMPARISON_DIR = PATHS.clinical_filtering_comparison
BEST_MODEL_DIR = PATHS.best_model
PUBLICATION_TABLES_DIR = PATHS.publication_tables

RISK_FACTORS_REVIEW_DATA_PATH = PATHS.risk_factors_review_file
RAW_CLINICAL_DATA_PATH = PATHS.raw_clinical_file
INTERMEDIATE_CLINICAL_DATA_PATH = PATHS.intermediate_clinical_file
RISK_SCORES_DATA_PATH = PATHS.risk_scores_file
CLEANED_CLINICAL_DATA_PATH = PATHS.clean_clinical_file
RAW_PROTEOMIC_DATA_PATH = PATHS.raw_proteomic_file
CLEANED_PROTEOMIC_DATA_PATH = PATHS.clean_proteomic_file
CLEANED_MULTIMODAL_DATA_PATH = PATHS.clean_multimodal_file
CLEANED_CLINICAL_MATCHED_DATA_PATH = PATHS.clean_clinical_matched_file

TABLE1_PATH = PATHS.table1_file
RISK_SCORES_METRICS_PATH = PATHS.risk_scores_metrics_file
RISK_SCORES_ROC_CURVE_PATH = PATHS.risk_scores_roc_curve_file
RISK_SCORES_PR_CURVE_PATH = PATHS.risk_scores_pr_curve_file
