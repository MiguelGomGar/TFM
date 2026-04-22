#%% Modules
import numpy as np
import pandas as pd
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer

#%% Scaler and encoder
def preprocess_clinical_data(X):
    
    # Divide features by their data type
    numeric_features = X.select_dtypes(include=[np.number]).columns
    ordered_categorical_features = ["smoking_status", "working_status", "education_level",]
    unordered_categorical_features = ["gender", "marital_status", "cohort"]
    
    # Transformers
    numeric_transformer = Pipeline(steps=[
        ('scaler', StandardScaler())
    ])
    categorical_transformer = Pipeline(steps=[
        ('encoder', OneHotEncoder(handle_unknown='ignore'))
    ])

    # Join transformers
    preprocessor = ColumnTransformer(
        transformers=[
            ('num', numeric_transformer, numeric_features),
            ('cat', categorical_transformer, categorical_features)
        ]
    )
    
    return preprocessor