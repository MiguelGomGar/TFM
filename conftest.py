"""Make the project importable during a test run.

The project is not an installed package: the pipelines are launched from the
repository root, which puts it on sys.path implicitly. pytest may be invoked
from anywhere, so the root is added explicitly here. Living at the root also
tells pytest where rootdir is, so `pytest` with no arguments collects tests/.
"""

import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent

if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))
