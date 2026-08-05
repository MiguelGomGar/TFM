"""Draw the figures of every modelling phase trained by pipelines 09 to 12.

Five families of charts are redrawn per phase, all from the tables the phase
left on disk: the Elastic Net feature selection coefficients, the
train-vs-validation overfitting bars, the ROC and PR curves of the external
validation, and the AUC bar charts against the risk score baseline. Nothing is
refitted, so a change of palette or title costs seconds.
"""

from pathlib import Path

from src.config import MODELLING_PHASES
from src.utils.logging_utils import setup_logger
from src.visualization.modelling_figures import plot_modelling_phase

logger = setup_logger(Path(__file__).stem)


# %% Main function
def main() -> None:
    for phase_spec in MODELLING_PHASES:
        plot_modelling_phase(**phase_spec, logger=logger)


if __name__ == "__main__":
    main()
