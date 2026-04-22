from sklearn.metrics import confusion_matrix, accuracy_score, precision_score, recall_score, average_precision_score

def evaluate_model(model, X_test, y_test):
    
    # Get predictions
    y_pred = model.predict(X_test)
    
    # Get the confusion matrix
    cm = confusion_matrix(y_test, y_pred)
    
    # Get scores
    accuracy = accuracy_score(y_test, y_pred)
    precision = precision_score(y_test, y_pred)
    recall = recall_score(y_test, y_pred)
    auc = average_precision_score(y_test, y_pred)
    
    return y_pred, cm, accuracy, precision, recall, auc