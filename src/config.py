from pathlib import Path

# 1. Specifying the project root directory
# Buscamos la carpeta contenedora de 'src' (que es la raíz del proyecto)
PROJECT_ROOT = Path(__file__).resolve().parent.parent

# 2. Defining main directories
DATA_DIR = PROJECT_ROOT / "data"
RESULTS_DIR = PROJECT_ROOT / "results"
SRC_DIR = PROJECT_ROOT / "src"

# 3. Defining specific paths
CLEAN_DATA_DIR = DATA_DIR / "clean"
MODELS_RESULTS_DIR = RESULTS_DIR / "models"

# 4. Get specific data files
def get_data_path(file: str = "clinical_data.parquet") -> Path:
    """
    Returns the path to the data directory for a specific subfolder.
    """
    path = CLEAN_DATA_DIR / file
    return path

CLINICAL_DATA_PATH = CLEAN_DATA_DIR / "clinical_data.parquet"

def get_results_path(subfolder: str = "whole_data") -> Path:
    """
    Returns the path to the results directory for a specific subfolder (e.g., "whole_data", "train", "test").
    """
    path = MODELS_RESULTS_DIR / subfolder
    path.mkdir(parents=True, exist_ok=True)
    return path
