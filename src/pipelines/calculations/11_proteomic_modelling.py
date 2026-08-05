"""Phase 2: train and evaluate the models on the proteomic panel alone."""

from pathlib import Path

from sklearn.model_selection import StratifiedKFold, train_test_split

from src.config import (
    CV_N_SPLITS,
    PROTEOMIC_SEARCH_SPACE,
    SEARCH_N_ITER,
    SEED,
    TARGET_VARIABLE,
    TEST_SIZE,
)
from src.models.data_preprocessing import encode_target_variable
from src.models.model_training import run_modelling_phase
from src.utils.dataframe_utils import get_proteomic_features
from src.utils.io import read_parquet
from src.utils.logging_utils import setup_logger
from src.utils.paths import CLEANED_PROTEOMIC_DATA_PATH, PROTEOMIC_MODELS_DIR

INPUT_FILE = CLEANED_PROTEOMIC_DATA_PATH
OUTPUT_DIR = PROTEOMIC_MODELS_DIR

PHASE_KEY = "proteomic"
SEARCH_SPACE = PROTEOMIC_SEARCH_SPACE
APPLY_FILTER = True

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    logger.info(f"Loading data from {INPUT_FILE}...")
    df = read_parquet(INPUT_FILE)

    # Keep the protein abundances only: the target and the stratification
    # variables are excluded from the predictors.
    X = df[get_proteomic_features(df)]
    y = encode_target_variable(df, TARGET_VARIABLE)
    logger.info(f"Modelling {X.shape[0]} records and {X.shape[1]} proteins.")

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
