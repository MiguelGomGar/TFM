"""Tests for the tables the threshold analysis writes to disk."""

import pandas as pd

from src.models.best_model import (
    build_model_comparison_table,
    build_ranking_metrics_table,
    build_threshold_info_table,
)


def make_results() -> dict[str, dict]:
    """Build two models' worth of analyze_model output."""
    return {
        "EN": {
            "ranking_metrics": {"ROC-AUC": 0.71, "PR-AUC": 0.54},
            "metrics_optimal": {
                "Accuracy": 0.70,
                "Precision": 0.60,
                "Recall": 0.65,
                "Specificity": 0.72,
                "F1": 0.62,
            },
            "threshold_rows": [{"Model": "EN", "Scenario": "Optimal", "Threshold": 0.42}],
        },
        "SVM": {
            "ranking_metrics": {"ROC-AUC": 0.68, "PR-AUC": 0.51},
            "metrics_optimal": {
                "Accuracy": 0.66,
                "Precision": 0.57,
                "Recall": 0.70,
                "Specificity": 0.64,
                "F1": 0.63,
            },
            "threshold_rows": [{"Model": "SVM", "Scenario": "Optimal", "Threshold": 0.38}],
        },
    }


def test_ranking_metrics_table_layout() -> None:
    """One row per metric, in the order the dict provides them."""
    table = build_ranking_metrics_table({"ROC-AUC": 0.71, "PR-AUC": 0.54})

    assert list(table.columns) == ["Metric", "Score"]
    assert table["Metric"].tolist() == ["ROC-AUC", "PR-AUC"]
    assert table["Score"].tolist() == [0.71, 0.54]


def test_model_comparison_table_excludes_f1() -> None:
    """F1 is reported per model but deliberately kept off the comparison chart."""
    table = build_model_comparison_table(make_results(), ["EN", "SVM"])

    assert list(table.columns) == ["Scenario", "Metric", "Score"]
    assert set(table["Metric"]) == {"Accuracy", "Precision", "Recall", "Specificity"}
    assert "F1" not in set(table["Metric"])


def test_model_comparison_table_follows_the_model_order() -> None:
    """Bar order on the chart comes from this, not from dict insertion order."""
    table = build_model_comparison_table(make_results(), ["SVM", "EN"])
    assert table["Scenario"].unique().tolist() == ["SVM", "EN"]


def test_model_comparison_table_carries_the_right_scores() -> None:
    """Each cell must come from that model's optimal-threshold metrics."""
    table = build_model_comparison_table(make_results(), ["EN", "SVM"])
    recall = table[(table["Scenario"] == "SVM") & (table["Metric"] == "Recall")]
    assert recall["Score"].item() == 0.70


def test_threshold_info_table_gathers_every_model() -> None:
    """One row per model and scenario, in reporting order."""
    table = build_threshold_info_table(make_results(), ["EN", "SVM"])

    assert len(table) == 2
    assert table["Model"].tolist() == ["EN", "SVM"]
    assert isinstance(table, pd.DataFrame)
