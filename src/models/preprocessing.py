import numpy as np
import pandas as pd
from sklearn.pipeline import Pipeline
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer

def get_clinical_preprocessor(X: pd.DataFrame) -> ColumnTransformer:
    """
    Creates and returns a scikit-learn ColumnTransformer tailored for clinical data.
    Automatically separates numeric features (imputed via median and standardized) 
    from categorical features (encoded via One-Hot Encoding).
    
    Parameters:
    -----------
    X : pd.DataFrame
        The input feature matrix used to dynamically extract column names by data type.
        
    Returns:
    --------
    ColumnTransformer
        A preconfigured, non-fitted preprocessing pipeline stage.
    """
    # 1. Dynamically extract feature groups based on current data types
    numeric_features = X.select_dtypes(include=[np.number]).columns
    categorical_features = X.select_dtypes(include=['category', 'object', 'bool']).columns
    
    # 2. Define the numerical transformation workflow
    numeric_transformer = Pipeline(steps=[
        ('imputer', SimpleImputer(strategy='median')),
        ('scaler', StandardScaler())
    ])
    
    # 3. Define the categorical transformation workflow
    # Note: sparse_output=False is recommended in modern sklearn to easily inspect arrays
    categorical_transformer = Pipeline(steps=[
        ('cat', OneHotEncoder(handle_unknown='ignore', sparse_output=False))
    ])
    
    # 4. Assemble the final unified structural preprocessor
    preprocessor = ColumnTransformer(
        transformers=[
            ('num', numeric_transformer, numeric_features),
            ('cat', categorical_transformer, categorical_features)
        ],
        remainder='drop' # Safely drop any missed tracking columns (like IDs)
    )
    
    return preprocessor

def get_svm_preprocessor(X: pd.DataFrame) -> ColumnTransformer:
    """
    Creates and returns a scikit-learn ColumnTransformer specifically designed for SVM models.
    
    Parameters:
    -----------
    X : pd.DataFrame
        The input feature matrix used to dynamically extract column names by data type.
        
    Returns:
    --------
    ColumnTransformer
        A preconfigured, non-fitted preprocessing pipeline stage suitable for SVM models.
    """
    