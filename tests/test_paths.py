"""Tests for the project path layer."""

from pathlib import Path

from src.utils import paths
from src.utils.paths import PATHS, PROJECT_ROOT, ProjectPaths


def test_project_root_is_the_repository_root() -> None:
    """The root must be the directory holding src/, not wherever it was called from."""
    assert (PROJECT_ROOT / "src").is_dir()
    assert (PROJECT_ROOT / "src" / "utils" / "paths.py").is_file()


def test_every_path_lives_under_the_root() -> None:
    """No constant may escape the project tree."""
    exported = [
        value
        for name, value in vars(paths).items()
        if name.isupper() and isinstance(value, Path) and name != "PROJECT_ROOT"
    ]
    assert exported, "expected the module to export Path constants"
    for path in exported:
        assert path.is_relative_to(PROJECT_ROOT), path


def test_the_root_is_relocatable(tmp_path: Path) -> None:
    """A different root must rewrite every derived path.

    This is what makes the layer testable: a run can be pointed at a scratch
    tree without touching the real results.
    """
    relocated = ProjectPaths(root=tmp_path)

    assert relocated.clean_clinical_file == tmp_path / "data/clean/clinical_data.parquet"
    assert relocated.multimodal_models == tmp_path / "results/models/multimodal_data"
    assert relocated.logs == tmp_path / "logs"


def test_derived_paths_nest_under_their_parent() -> None:
    """Each directory must sit inside the one it is documented to belong to."""
    assert PATHS.clinical_eda.parent == PATHS.eda
    assert PATHS.eda.parent == PATHS.results
    assert PATHS.clean_data.parent == PATHS.data
    assert PATHS.multimodal_models.parent == PATHS.models
    assert PATHS.clean_clinical_file.parent == PATHS.clean_data


def test_paths_are_frozen() -> None:
    """Freezing is what makes the module-level constants trustworthy."""
    import dataclasses

    import pytest

    with pytest.raises(dataclasses.FrozenInstanceError):
        PATHS.root = Path("/somewhere/else")  # type: ignore[misc]


def test_data_files_have_the_expected_suffixes() -> None:
    """Readers are chosen by extension, so a wrong suffix breaks the loader."""
    assert PATHS.raw_clinical_file.suffix == ".dta"
    assert PATHS.raw_proteomic_file.suffix == ".csv"
    assert PATHS.risk_factors_review_file.suffix == ".xlsx"
    for parquet_file in (
        PATHS.clean_clinical_file,
        PATHS.clean_proteomic_file,
        PATHS.clean_multimodal_file,
        PATHS.clean_clinical_matched_file,
        PATHS.intermediate_clinical_file,
        PATHS.risk_scores_file,
    ):
        assert parquet_file.suffix == ".parquet", parquet_file
