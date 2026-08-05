"""
Phase 3: integrated multi-modal models against a reduced clinical model on
the exact same matched subcohort and on the exact same train/test partition
"""

from pathlib import Path

from sklearn.model_selection import StratifiedKFold, train_test_split

from src.config import (
    CLINICAL_SEARCH_SPACE,
    CV_N_SPLITS,
    MULTIMODAL_SEARCH_SPACE,
    SEARCH_N_ITER,
    SEED,
    TARGET_VARIABLE,
    TEST_SIZE,
)
from src.models.data_preprocessing import encode_target_variable
from src.models.feature_selection import load_previously_kept_features
from src.models.model_training import run_modelling_phase
from src.utils.io import read_parquet
from src.utils.logging_utils import setup_logger
from src.utils.paths import (
    CLEANED_MULTIMODAL_DATA_PATH,
    CLINICAL_MODELS_FILTERED_DIR,
    CLINICAL_MODELS_MATCHED_DIR,
    MULTIMODAL_MODELS_DIR,
    PROTEOMIC_MODELS_DIR,
)

INPUT_FILE = CLEANED_MULTIMODAL_DATA_PATH
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

    clinical_predictors = load_previously_kept_features(CLINICAL_MODELS_FILTERED_DIR)
    clinical_predictors = [col for col in clinical_predictors if col in df.columns]
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
        search_spaces=CLINICAL_SEARCH_SPACE,
        output_dir=CLINICAL_OUTPUT_DIR,
        n_iter=SEARCH_N_ITER["clinical_matched"],
        cv=cv,
        apply_filter=CLINICAL_APPLY_FILTER,
        logger=logger,
    )
    logger.info(f"Reduced clinical results saved to {CLINICAL_OUTPUT_DIR}.")

    # ------------------------------------------------------------------
    # Arm 2: integrated multi-modal model
    # ------------------------------------------------------------------
    # Instead of refitting the Elastic Net on every clinical predictor plus
    # every protein, reuse the features already kept by phase 1b (clinical_filtered)
    # and by the standalone proteomic phase (phase 2). This keeps the
    # multimodal design matrix at a tractable dimensionality from the start.
    previously_kept_features = load_previously_kept_features(
        CLINICAL_MODELS_FILTERED_DIR, PROTEOMIC_MODELS_DIR
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
        search_spaces=MULTIMODAL_SEARCH_SPACE,
        output_dir=MULTIMODAL_OUTPUT_DIR,
        n_iter=SEARCH_N_ITER["multimodal"],
        cv=cv,
        apply_filter=MULTIMODAL_APPLY_FILTER,
        logger=logger,
    )
    logger.info(f"Multimodal results saved to {MULTIMODAL_OUTPUT_DIR}.")


if __name__ == "__main__":
    main()
