"""Feature engineering helpers for clinical risk scores."""

import numpy as np
import pandas as pd

from src.config import TARGET_VARIABLE


def _compute_hatch_score(df: pd.DataFrame) -> pd.DataFrame:
    """Compute the HATCH risk score for atrial fibrillation ablation recurrence.

    HATCH score is calculated as: age >75 (1 pt) + hypertension (1 pt) +
    COPD (1 pt) + stroke (2 pts) + heart failure (2 pts).

    Parameters
    ----------
    df : pd.DataFrame
        Clinical dataframe with columns: 'age', 'hypertension', 'COPD',
        'stroke', 'heart_failure'.

    Returns
    -------
    pd.DataFrame
        Input dataframe with an added 'score_hatch' column containing
        computed HATCH scores. Missing values in input columns propagate.
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
    """Compute the CHADS2 stroke risk score for atrial fibrillation.

    CHADS2 score is calculated as: age ≥75 (1 pt) + hypertension (1 pt) +
    diabetes (1 pt) + stroke/TIA (2 pts) + heart failure (1 pt).

    Parameters
    ----------
    df : pd.DataFrame
        Clinical dataframe with columns: 'age', 'hypertension', 'diabetes',
        'stroke', 'heart_failure'.

    Returns
    -------
    pd.DataFrame
        Input dataframe with an added 'score_chads2' column containing
        computed CHADS2 scores. Missing values in input columns propagate.
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
    """Compute the BASE-AF2 recurrence risk score for atrial fibrillation ablation.

    BASE-AF2 score is calculated from BMI, left atrial enlargement (sex-specific),
    smoking, early recurrence after ablation, AF duration >6 months, and
    persistent AF type.

    Parameters
    ----------
    df : pd.DataFrame
        Clinical dataframe with columns: 'BMI', 'sex', 'LA_enlargment',
        'smoking_status', 'ERAF', 'AF_duration', 'AF_type'.

    Returns
    -------
    pd.DataFrame
        Input dataframe with an added 'score_baseaf2' column containing
        computed BASE-AF2 scores. Missing values propagate.
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
    """Compute the MB-LATER recurrence risk score for atrial fibrillation.

    MB-LATER score is calculated from male sex, bundle branch block,
    left atrial enlargement (sex-specific), persistent AF, and early recurrence.

    Parameters
    ----------
    df : pd.DataFrame
        Clinical dataframe with columns: 'sex', 'RBBB', 'LBBB',
        'LA_enlargment', 'AF_type', 'ERAF'.

    Returns
    -------
    pd.DataFrame
        Input dataframe with an added 'score_mblater' column containing
        computed MB-LATER scores. Missing values propagate.
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
    """Compute all risk scores (HATCH, CHADS2, BASE-AF2, MB-LATER) for patients.

    Parameters
    ----------
    df : pd.DataFrame
        Clinical dataframe containing all required fields for each risk score
        (age, sex, hypertension, diabetes, stroke, heart_failure, BMI,
        LA_enlargment, smoking_status, ERAF, AF_duration, AF_type, RBBB, LBBB).

    Returns
    -------
    pd.DataFrame
        DataFrame with only the target variable and all risk score columns
        (score_hatch, score_chads2, score_baseaf2, score_mblater).
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
