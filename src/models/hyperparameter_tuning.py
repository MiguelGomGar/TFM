def objective_RF(trial):
    # 1. Define hyperparameters search space
    n_estimators = trial.suggest_int('n_estimators', 2, 512)
    min_samples_split = trial.suggest_int('min_samples_split', 2, 20)
    criterion = trial.suggest_categorical('criterion', ['gini', 'entropy'])
    class_weight = trial.suggest_categorical('class_weight', [None, 'balanced', 'balanced_subsample'])
    
    limit_depth = trial.suggest_categorical('limit_depth', [True, False])
    
    if limit_depth:
        max_depth = trial.suggest_int('max_depth', 2, 32, log=True)
    else:
        max_depth = None
    
    # 2. Instantiate the model using the suggested parameters
    clf = RandomForestClassifier(
        n_estimators=n_estimators,
        min_samples_split=min_samples_split,
        criterion=criterion,
        max_depth=max_depth,
        class_weight=class_weight,
        random_state=42
    )
    
    # 3. Build the pipeline with the preprocessor and the model
    pipe=Pipeline(steps=[
        ('preprocessor', preprocessor),
        ('clf', clf)
    ])
    
    # 4. Evaluate the full pipeline
    score = cross_val_score(pipe, X_train, y_train, scoring='recall', n_jobs=-1, cv=5)
    
    return score.mean()

def objective_SVM(trial):
    # 1. Define hyperparameters search space
    C = trial.suggest_float('C', 1e-3, 1e2, log=True)
    class_weight = trial.suggest_categorical('clf__class_weight', [None, 'balanced', 'balanced_subsample'])
    
    
    # 2. Instantiate the model using the suggested parameters
    clf = RandomForestClassifier(
        n_estimators=n_estimators,
        min_samples_split=min_samples_split,
        criterion=criterion,
        max_depth=max_depth,
        class_weight=class_weight,
        random_state=42
    )
    
    # 3. Build the pipeline with the preprocessor and the model
    pipe=Pipeline(steps=[
        ('preprocessor', preprocessor),
        ('clf', clf)
    ])
    
    # 4. Evaluate the full pipeline
    score = cross_val_score(pipe, X_train, y_train, scoring='recall', n_jobs=-1, cv=5)
    
    return score.mean()