"""Phase 3: integrated multi-modal models against a reduced clinical model.

Both arms are trained on the exact same matched subcohort and on the exact same
train/test partition, so the difference in performance can be attributed to the
proteomic panel rather than to the cohort or the split. The clinical arm uses
only the clinical predictors; the multimodal arm adds the protein abundances.

Refitting the Elastic Net on the full multimodal design matrix (all proteins
plus all clinical predictors) is by far the slowest step of the whole
pipeline. Since phases 1b and 2 already ran that same filtering on the
clinical and proteomic predictors separately, the multimodal arm reuses their
``features_kept.csv`` as its starting feature set instead of repeating the
selection from scratch.
"""

from pathlib import Path

from sklearn.model_selection import StratifiedKFold, train_test_split

from src.config import (
    CV_N_SPLITS,
    SEARCH_N_ITER,
    SEED,
    TARGET_VARIABLE,
    TEST_SIZE,
    clinical_hyperparameters_search_space,
    multimodal_hyperparameters_search_space,
)
from src.models.data_preprocessing import encode_target_variable
from src.models.feature_selection import (
    get_clinical_predictors,
    load_previously_kept_features,
)
from src.models.model_training import run_modelling_phase
from src.utils.io import read_parquet
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    CLEANED_CLINICAL_DATA_PATH,
    CLEANED_MULTIMODAL_DATA_PATH,
    CLINICAL_MODELS_MATCHED_DIR,
    MULTIMODAL_MODELS_DIR,
    PROTEOMIC_MODELS_DIR,
)

INPUT_FILE = CLEANED_MULTIMODAL_DATA_PATH
CLINICAL_REFERENCE_FILE = CLEANED_CLINICAL_DATA_PATH
CLINICAL_OUTPUT_DIR = CLINICAL_MODELS_MATCHED_DIR
MULTIMODAL_OUTPUT_DIR = MULTIMODAL_MODELS_DIR

# The clinical arm still filters its own predictors through the Elastic Net.
CLINICAL_APPLY_FILTER = True
# The multimodal arm starts from the features already kept by phases 1b and 2,
# so it does not need to filter any further.
MULTIMODAL_APPLY_FILTER = False

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    CLINICAL_OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    MULTIMODAL_OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading data from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)
    clinical_reference = read_parquet(CLINICAL_REFERENCE_FILE)

    clinical_predictors = get_clinical_predictors(df, clinical_reference)
    y = encode_target_variable(df, TARGET_VARIABLE)

    X_clinical = df[clinical_predictors]

    cv = StratifiedKFold(n_splits=CV_N_SPLITS, shuffle=True, random_state=SEED)

    # ------------------------------------------------------------------
    # Arm 1: reduced clinical model on the matched subcohort
    # ------------------------------------------------------------------
    logger.info("Training the reduced clinical arm...")
    X_train, X_test, y_train, y_test = train_test_split(
        X_clinical,
        y,
        test_size=TEST_SIZE,
        random_state=SEED,
        shuffle=True,
        stratify=y,
    )
    run_modelling_phase(
        X_train=X_train,
        X_test=X_test,
        y_train=y_train,
        y_test=y_test,
        search_spaces=clinical_hyperparameters_search_space,
        output_dir=CLINICAL_OUTPUT_DIR,
        n_iter=SEARCH_N_ITER["clinical_matched"],
        cv=cv,
        phase_title="matched clinical data",
        apply_filter=CLINICAL_APPLY_FILTER,
        logger=logger,
    )
    logger.info(f"Reduced clinical results saved to {CLINICAL_OUTPUT_DIR}.")

    # ------------------------------------------------------------------
    # Arm 2: integrated multi-modal model
    # ------------------------------------------------------------------
    # Instead of refitting the Elastic Net on every clinical predictor plus
    # every protein, reuse the features already kept by the clinical arm
    # above and by the standalone proteomic phase (phase 2). This keeps the
    # multimodal design matrix at a tractable dimensionality from the start.
    previously_kept_features = load_previously_kept_features(
        CLINICAL_OUTPUT_DIR, PROTEOMIC_MODELS_DIR
    )
    multimodal_predictors = [
        column for column in previously_kept_features if column in df.columns
    ]
    logger.info(
        f"Reusing {len(multimodal_predictors)} feature(s) already kept by the "
        "clinical and proteomic filterings as the multimodal starting set."
    )
    X_multimodal = df[multimodal_predictors]

    # Same rows, same seed and same stratification, so the partition matches the
    # one used by the clinical arm record by record.
    logger.info("Training the integrated multimodal arm...")
    X_train, X_test, y_train, y_test = train_test_split(
        X_multimodal,
        y,
        test_size=TEST_SIZE,
        random_state=SEED,
        shuffle=True,
        stratify=y,
    )
    run_modelling_phase(
        X_train=X_train,
        X_test=X_test,
        y_train=y_train,
        y_test=y_test,
        search_spaces=multimodal_hyperparameters_search_space,
        output_dir=MULTIMODAL_OUTPUT_DIR,
        n_iter=SEARCH_N_ITER["multimodal"],
        cv=cv,
        phase_title="multimodal data",
        apply_filter=MULTIMODAL_APPLY_FILTER,
        logger=logger,
    )
    logger.info(f"Multimodal results saved to {MULTIMODAL_OUTPUT_DIR}.")


if __name__ == "__main__":
    main()
