"""Phase 1b: clinical models trained on the features kept by the Elastic Net.

The Elastic Net is fitted first on every clinical predictor; the variables whose
coefficients its regularization shrinks to exactly zero are then dropped, and
the remaining models are trained only on the surviving ones. The two lists are
written to features_kept.csv and features_removed.csv.
"""

from pathlib import Path

from sklearn.model_selection import StratifiedKFold, train_test_split

from src.config import (
    CLINICAL_SEARCH_SPACE,
    CV_N_SPLITS,
    SEARCH_N_ITER,
    SEED,
    TARGET_VARIABLE,
    TEST_SIZE,
)
from src.models.data_preprocessing import encode_target_variable
from src.models.model_training import run_modelling_phase
from src.utils.io import read_parquet
from src.utils.logging_utils import setup_logger
from src.utils.paths import CLEANED_CLINICAL_DATA_PATH, CLINICAL_MODELS_FILTERED_DIR

INPUT_FILE = CLEANED_CLINICAL_DATA_PATH
OUTPUT_DIR = CLINICAL_MODELS_FILTERED_DIR

PHASE_KEY = "clinical_filtered"
SEARCH_SPACE = CLINICAL_SEARCH_SPACE
APPLY_FILTER = True

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    """Train and evaluate every model on the Elastic Net feature subset."""
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading data from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)

    X = df.drop(columns=[TARGET_VARIABLE])
    y = encode_target_variable(df, TARGET_VARIABLE)
    logger.info(f"Modelling {X.shape[0]} records and {X.shape[1]} features.")

    # Same seed, split size and stratification as phase 1, so both phases share
    # the exact same training and external validation sets.
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
