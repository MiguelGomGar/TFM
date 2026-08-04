"""Phase 1: train and evaluate the models on the full clinical dataset."""

from pathlib import Path

from sklearn.model_selection import StratifiedKFold, train_test_split

from src.config import (
    CV_N_SPLITS,
    SEARCH_N_ITER,
    SEED,
    TARGET_VARIABLE,
    TEST_SIZE,
    clinical_hyperparameters_search_space,
)
from src.models.data_preprocessing import encode_target_variable
from src.models.model_training import run_modelling_phase
from src.utils.io import read_parquet
from src.utils.logging_utils import setup_logger
from src.utils.paths import CLEANED_CLINICAL_DATA_PATH, CLINICAL_MODELS_DIR

INPUT_FILE = CLEANED_CLINICAL_DATA_PATH
OUTPUT_DIR = CLINICAL_MODELS_DIR

PHASE_KEY = "clinical"
SEARCH_SPACE = clinical_hyperparameters_search_space

# Phase 1 is the clinical-only benchmark: every predictor is kept so that the
# filtered phase can be compared against it.
APPLY_FILTER = False

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading data from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)

    X = df.drop(columns=[TARGET_VARIABLE])
    y = encode_target_variable(df, TARGET_VARIABLE)
    logger.info(f"Modelling {X.shape[0]} records and {X.shape[1]} features.")

    logger.info("Splitting data into training and external validation sets...")
    X_train, X_test, y_train, y_test = train_test_split(
        X,
        y,
        test_size=TEST_SIZE,
        random_state=SEED,
        shuffle=True,
        stratify=y,
    )
    cv = StratifiedKFold(n_splits=CV_N_SPLITS, shuffle=True, random_state=SEED)

    run_modelling_phase(
        X_train=X_train,
        X_test=X_test,
        y_train=y_train,
        y_test=y_test,
        search_spaces=SEARCH_SPACE,
        output_dir=OUTPUT_DIR,
        n_iter=SEARCH_N_ITER[PHASE_KEY],
        cv=cv,
        apply_filter=APPLY_FILTER,
        logger=logger,
    )

    logger.info(f"Results saved to {OUTPUT_DIR}.")


if __name__ == "__main__":
    main()
