import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler, OrdinalEncoder

from sklearn.linear_model import LogisticRegression
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer

def get_regres_preprocessor(X: pd.DataFrame, seed: int) -> ColumnTransformer:
    """
    Creates a scikit-learn ColumnTransformer tailored for Logistic Regression models
    using R-generated Parquet files. Imputes numerical features with the median and
    applies StandardScaler, which is strictly mandatory for proper convergence and
    unbiased regularization (such as Elastic Net). Encodes all categorical variables
    ordinally using embedded Parquet metadata.
    
    Parameters:
    - X: The input DataFrame containing the features to be preprocessed.
    - seed: An integer seed for reproducibility in imputation (if needed).
    
    Returns:
    - A ColumnTransformer object that can be used in a scikit-learn pipeline
    """
    # 1. Dynamically isolate numerical and categorical features
    numeric_features = X.select_dtypes(include=[np.number]).columns.tolist()
    categorical_features = X.select_dtypes(include=['category']).columns.tolist()
    
    # 2. Define the standardized pipeline for continuous variables
    numeric_transformer = Pipeline(steps=[
        ('imputer', IterativeImputer(max_iter=10,
                                    initial_strategy='median',
                                    random_state=seed)),
        ('scaler', StandardScaler())
    ])
    
    # 3. Harvest embedded hierarchies and build customized OrdinalEncoders per feature
    transformers_list = [
        ('num', numeric_transformer, numeric_features)
    ]
    
    # Loop over every single factor (both binary and multi-class ordinals)
    for col in categorical_features:
        # Pull the pre-sorted levels array directly from the Parquet metadata slot
        extracted_order = list(X[col].dtype.categories)
        
        categorical_pipeline = Pipeline(steps=[
            ('encoder', OrdinalEncoder(categories=[extracted_order]))
        ])
        
        # Append as a clean, localized and independent processing lane
        transformers_list.append((f'ord_{col}', categorical_pipeline, [col]))
        
    # 4. Assemble the final unified two-lane preprocessor structural framework
    preprocessor = ColumnTransformer(
        transformers=transformers_list,
        remainder='drop'
    )

    return preprocessor


def get_geom_preprocessor(X: pd.DataFrame, seed: int) -> ColumnTransformer:
    """
    Creates a scikit-learn ColumnTransformer tailored for distance-based geometric 
    models like Support Vector Machines (SVM). Imputes numerical features with the 
    median and scales them using StandardScaler, which is essential since geometry-driven 
    algorithms are highly sensitive to feature magnitudes when computing hyperplanes. 
    Encodes all categorical variables ordinally using embedded Parquet metadata and 
    scales them with StandardScaler to ensure uniform distance weighting.
    
    Parameters:
    - X: The input DataFrame containing the features to be preprocessed.
    - seed: An integer seed for reproducibility in imputation (if needed).
    Returns:
    - A ColumnTransformer object that can be used in a scikit-learn pipeline
    """
    # 1. Dynamically isolate numerical and categorical features
    numeric_features = X.select_dtypes(include=[np.number]).columns.tolist()
    categorical_features = X.select_dtypes(include=['category']).columns.tolist()
    
    # 2. Define the standardized pipeline for continuous variables
    numeric_transformer = Pipeline(steps=[
        ('imputer', IterativeImputer(max_iter=10,
                                    initial_strategy='median',
                                    random_state=seed)),
        ('scaler', StandardScaler())
    ])
    
    # 3. Harvest embedded hierarchies and build customized OrdinalEncoders per feature
    transformers_list = [
        ('num', numeric_transformer, numeric_features)
    ]
    
    # Loop over every single factor (both binary and multi-class ordinals)
    for col in categorical_features:
        # Pull the pre-sorted levels array directly from the Parquet metadata slot
        extracted_order = list(X[col].dtype.categories)
        
        categorical_pipeline = Pipeline(steps=[
            ('encoder', OrdinalEncoder(categories=[extracted_order])),
            ('scaler', StandardScaler())
        ])
        
        # Append as a clean, localized and independent processing lane
        transformers_list.append((f'ord_{col}', categorical_pipeline, [col]))
        
    # 4. Assemble the final unified two-lane preprocessor structural framework
    preprocessor = ColumnTransformer(
        transformers=transformers_list,
        remainder='drop'
    )
    
    return preprocessor


def get_bagg_preprocessor(X: pd.DataFrame, seed: int) -> ColumnTransformer:
    """
    Creates a scikit-learn ColumnTransformer tailored for tree-based bagging ensembles
    such as Random Forest. Imputes numerical features with the median to satisfy 
    scikit-learn's structural constraints against NaNs, but skips scaling since tree
    splits are invariant to monotonic transformations. Encodes all categorical 
    variables ordinally using embedded Parquet metadata.
    """
    # 1. Dynamically isolate numerical and categorical features
    numeric_features = X.select_dtypes(include=[np.number]).columns.tolist()
    categorical_features = X.select_dtypes(include=['category']).columns.tolist()
    
    # 2. Define the standardized pipeline for continuous variables
    numeric_transformer = Pipeline(steps=[
        ('imputer', IterativeImputer(max_iter=10,
                                    initial_strategy='median',
                                    random_state=seed))
    ])
    
    # 3. Harvest embedded hierarchies and build customized OrdinalEncoders per feature
    transformers_list = [
        ('num', numeric_transformer, numeric_features)
    ]
    
    # Loop over every single factor (both binary and multi-class ordinals)
    for col in categorical_features:
        # Pull the pre-sorted levels array directly from the Parquet metadata slot
        extracted_order = list(X[col].dtype.categories)
        
        categorical_pipeline = Pipeline(steps=[
            ('encoder', OrdinalEncoder(categories=[extracted_order]))
        ])
        
        # Append as a clean, localized and independent processing lane
        transformers_list.append((f'ord_{col}', categorical_pipeline, [col]))
        
    # 4. Assemble the final unified two-lane preprocessor structural framework
    preprocessor = ColumnTransformer(
        transformers=transformers_list,
        remainder='drop'
    )
    
    return preprocessor


def get_boost_preprocessor(X: pd.DataFrame, seed: int) -> ColumnTransformer:
    """
    Creates a scikit-learn ColumnTransformer tailored for gradient boosting via XGBoost. 
    It routes all categorical features through an automated metadata-driven OrdinalEncoder 
    while setting remainder='passthrough'. This allows numerical features to bypass 
    imputation and scaling, retaining their missing values (NaNs) so XGBoost can natively 
    exploit them using its internal sparsity-aware split finding mechanism.
    """
    # 1. Dynamically isolate categorical features
    categorical_features = X.select_dtypes(include=['category']).columns.tolist()
    
    # 2. Harvest embedded hierarchies and build customized OrdinalEncoders per feature
    transformers_list = []
    
    # Loop over every single factor (both binary and multi-class ordinals)
    for col in categorical_features:
        # Pull the pre-sorted levels array directly from the Parquet metadata slot
        extracted_order = list(X[col].dtype.categories)
        
        categorical_pipeline = Pipeline(steps=[
            ('encoder', OrdinalEncoder(categories=[extracted_order]))
        ])
        
        # Append as a clean, localized and independent processing lane
        transformers_list.append((f'ord_{col}', categorical_pipeline, [col]))
        
    # 3. Assemble the final unified structural framework
    preprocessor = ColumnTransformer(
        transformers=transformers_list,
        remainder='passthrough'
    )
    
    return preprocessor