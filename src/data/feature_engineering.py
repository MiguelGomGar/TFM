"""Feature engineering helpers for clinical risk scores."""

import numpy as np
import pandas as pd

from src.config import TARGET_VARIABLE


def _compute_hatch_score(df: pd.DataFrame) -> pd.DataFrame:
    """
    Computes the HATCH score for each patient based on their age and comorbidities.
    """
    df_result = df.copy()

    # Propagate NaNs to match R's default behavior
    t1 = pd.Series(np.where(df_result["age"] > 75, 1, 0), index=df_result.index).where(
        df_result["age"].notna(), np.nan
    )
    t2 = pd.Series(
        np.where(df_result["hypertension"] == "yes", 1, 0), index=df_result.index
    ).where(df_result["hypertension"].notna(), np.nan)
    t3 = pd.Series(
        np.where(df_result["COPD"] == "yes", 1, 0), index=df_result.index
    ).where(df_result["COPD"].notna(), np.nan)
    t4 = pd.Series(
        np.where(df_result["stroke"] == "yes", 2, 0), index=df_result.index
    ).where(df_result["stroke"].notna(), np.nan)
    t5 = pd.Series(
        np.where(df_result["heart_failure"] == "yes", 2, 0), index=df_result.index
    ).where(df_result["heart_failure"].notna(), np.nan)

    df_result["score_hatch"] = t1 + t2 + t3 + t4 + t5
    return df_result


def _compute_chads2_score(df: pd.DataFrame) -> pd.DataFrame:
    """
    Computes the CHADS2 score for each patient based on their age and comorbidities.
    """
    df_result = df.copy()

    # Propagate NaNs to match R's default behavior
    t1 = pd.Series(np.where(df_result["age"] >= 75, 1, 0), index=df_result.index).where(
        df_result["age"].notna(), np.nan
    )
    t2 = pd.Series(
        np.where(df_result["hypertension"] == "yes", 1, 0), index=df_result.index
    ).where(df_result["hypertension"].notna(), np.nan)
    t3 = pd.Series(
        np.where(df_result["diabetes"] == "yes", 1, 0), index=df_result.index
    ).where(df_result["diabetes"].notna(), np.nan)
    t4 = pd.Series(
        np.where(df_result["stroke"] == "yes", 2, 0), index=df_result.index
    ).where(df_result["stroke"].notna(), np.nan)
    t5 = pd.Series(
        np.where(df_result["heart_failure"] == "yes", 1, 0), index=df_result.index
    ).where(df_result["heart_failure"].notna(), np.nan)

    df_result["score_chads2"] = t1 + t2 + t3 + t4 + t5
    return df_result


def _compute_base_af2_score(df: pd.DataFrame) -> pd.DataFrame:
    """
    Computes the BASE-AF2 score for each patient based on their comorbidities.
    """
    df_result = df.copy()

    # Propagate NaNs to match R's default behavior
    t1 = pd.Series(np.where(df_result["BMI"] >= 28, 1, 0), index=df_result.index).where(
        df_result["BMI"].notna(), np.nan
    )

    cond_la = (
        (df_result["sex"] == "male")
        & df_result["LA_enlargment"].isin(["moderate", "severe"])
    ) | (
        (df_result["sex"] == "female")
        & df_result["LA_enlargment"].isin(["mild", "moderate", "severe"])
    )
    t2 = pd.Series(np.where(cond_la, 1, 0), index=df_result.index).where(
        df_result["sex"].notna() & df_result["LA_enlargment"].notna(), np.nan
    )
    t3 = pd.Series(
        np.where(df_result["smoking_status"] == "yes", 1, 0), index=df_result.index
    ).where(df_result["smoking_status"].notna(), np.nan)
    t4 = pd.Series(
        np.where(df_result["ERAF"] == "yes", 1, 0), index=df_result.index
    ).where(df_result["ERAF"].notna(), np.nan)
    t5 = pd.Series(
        np.where(df_result["AF_duration"] > 6, 1, 0), index=df_result.index
    ).where(df_result["AF_duration"].notna(), np.nan)
    t6 = pd.Series(
        np.where(df_result["AF_type"] == "persistent", 1, 0), index=df_result.index
    ).where(df_result["AF_type"].notna(), np.nan)

    df_result["score_baseaf2"] = t1 + t2 + t3 + t4 + t5 + t6
    return df_result


def _compute_mb_later_score(df: pd.DataFrame) -> pd.DataFrame:
    """
    Computes the MB-LATER score for each patient based on their comorbidities.
    """
    df_result = df.copy()

    # Propagate NaNs to match R's default behavior
    t1 = pd.Series(
        np.where(df_result["sex"] == "male", 1, 0), index=df_result.index
    ).where(df_result["sex"].notna(), np.nan)

    cond_bbb = (df_result["RBBB"] == "yes") | (df_result["LBBB"] == "yes")
    t2 = pd.Series(np.where(cond_bbb, 1, 0), index=df_result.index).where(
        df_result["RBBB"].notna() | df_result["LBBB"].notna(),
        np.nan,
    )

    cond_la_f = (df_result["sex"] == "female") & df_result["LA_enlargment"].isin(
        ["moderate", "severe"]
    )
    t3 = pd.Series(np.where(cond_la_f, 1, 0), index=df_result.index).where(
        df_result["sex"].notna() & df_result["LA_enlargment"].notna(), np.nan
    )

    cond_la_m = (df_result["sex"] == "male") & (df_result["LA_enlargment"] == "severe")
    t4 = pd.Series(np.where(cond_la_m, 1, 0), index=df_result.index).where(
        df_result["sex"].notna() & df_result["LA_enlargment"].notna(), np.nan
    )

    t5 = pd.Series(
        np.where(df_result["AF_type"] == "persistent", 1, 0), index=df_result.index
    ).where(df_result["AF_type"].notna(), np.nan)
    t6 = pd.Series(
        np.where(df_result["ERAF"] == "yes", 1, 0), index=df_result.index
    ).where(df_result["ERAF"].notna(), np.nan)

    df_result["score_mblater"] = t1 + t2 + t3 + t4 + t5 + t6
    return df_result


def compute_risk_scores(df: pd.DataFrame) -> pd.DataFrame:
    """
    Computes all risk scores for each patient based on their comorbidities.
    """
    df_result = df.copy()
    df_result = _compute_hatch_score(df_result)
    df_result = _compute_chads2_score(df_result)
    df_result = _compute_base_af2_score(df_result)
    df_result = _compute_mb_later_score(df_result)

    columns_to_keep = [col for col in df_result.columns if col.startswith("score")] + [
        TARGET_VARIABLE
    ]
    df_result = df_result[columns_to_keep]

    return df_result
