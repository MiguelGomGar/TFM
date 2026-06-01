def compute_hatch_score(df):
    
    """"
    Compute the HATCH score for each patient in the DataFrame.
    """""
    
    # Define the conditions for each risk factor
    condition1 = df['heart_failure'] == 'yes'
    condition2 = df['age'] >= 75
    condition3 = df['stroke'] == 'yes'
    condition4 = df['COPD'] == 'yes'
    condition5 = df['hypertension'] == 'yes'
    
    # Calculate the HATCH score based on the conditions
    df['HATCH_score'] = (condition1.astype(int) + 
                        condition2.astype(int) + 
                        condition3.astype(int) + 
                        condition4.astype(int) + 
                        condition5.astype(int))
    return df

def hatch_model_prediction(df, threshold=3):
    """
    Predicts AF recurrence based on the HATCH score.
    
    Parameters:
    - df (DataFrame): The input DataFrame containing patient data.
    - threshold (int): The HATCH score threshold for predicting high risk of AF recurrence.
    
    Returns:
    Series: A Series containing the predicted AF recurrence outcomes (1 for high risk, 0 for low risk).
    """
    
    # Compute the HATCH score for each patient
    df = compute_hatch_score(df)
    
    # Predict AF recurrence risk based on the HATCH score
    outcome = df['HATCH_score'].apply(lambda x: 1 if x >= threshold else 0)
    
    return outcome