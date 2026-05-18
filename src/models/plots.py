import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix

def plot_cm(y_true, y_pred, 
            class_names=None, 
            title='Confusion Matrix', 
            cmap='Blues'):
    """
    Plots a confusion matrix.

    Parameters:
    ----------
    - y_true : array-like
        True labels.
    - y_pred : array-like
        Predicted labels.
    - class_names : list, optional
        List with the names of the classes. If None, uses numbers.
    - title : str, default='Confusion Matrix'
        Title of the plot.
    - cmap : str, default='Blues'
        Color palette.
    """
    
    # 1. Compute the confusion matrix
    cm = confusion_matrix(y_true, y_pred)
    
    # 2. Compute the percentages per row
    cm_percentages = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
    
    # 3. Create the text labels by combining counts and percentages
    labels = [f"{v1}\n({v2:.1%})" for v1, v2 in zip(cm.flatten(), cm_percentages.flatten())]
    labels = np.asarray(labels).reshape(cm.shape)
    
    # 4. Set up the figure size
    plt.figure(figsize=(8, 6))
    
    # 5. Draw the heatmap
    ax = sns.heatmap(cm, 
                    annot=labels, 
                    fmt='', 
                    cmap=cmap, 
                    cbar=True, 
                    linewidths=1, 
                    linecolor='white', 
                    annot_kws={"size": 12, "weight": "bold"})
    
    # 6. Customize the plot
    plt.title(title, fontsize=16, pad=20, weight='bold')
    plt.ylabel('True values', fontsize=14, weight='bold')
    plt.xlabel('Predicted values', fontsize=14, weight='bold')
    
    # 7. Set class names if provided
    if class_names is not None:
        ax.set_xticklabels(class_names, fontsize=12)
        ax.set_yticklabels(class_names, fontsize=12, rotation=0)
        
    # 8. Adjust layout and show the plot
    plt.tight_layout()
    plt.show()

def autolabel(rects, ax):
        """
        Add labels on top of the bars.
        
        Parameters:
        ----------
        - rects : list
            List of bar objects.
        - ax : matplotlib.axes.Axes
            The axes object on which to draw the labels.
        """
        for rect in rects:
            height = rect.get_height()
            
            if np.isnan(height): continue
            
            ax.annotate(f'{height:.3f}',
                        xy=(rect.get_x() + rect.get_width() / 2, height),
                        xytext=(0, 3),
                        textcoords="offset points",
                        ha='center', va='bottom', fontsize=10, weight='bold')

def plot_overfitting_bars(metrics_train_mean, 
                        metrics_val_mean, 
                        metrics_train_std=None,
                        metrics_val_std=None,
                        model_name="Model"):
    """
    Plots a grouped bar chart to compare training and validation metrics, facilitating the 
    detection of overfitting.

    Parameters:
    ----------
    - metrics_train_mean : dict
        Dictionary with the means of the metrics in the training set.
    - metrics_val_mean : dict
        Dictionary with the means of the metrics in the validation set.
    - metrics_train_std : dict, optional
        Dictionary with the standard deviations of the training set.
    - metrics_val_std : dict, optional
        Dictionary with the standard deviations of the validation set.
    - model_name : str
        Name of the model for the chart title.
    """
    
    # 1. Retrieve the metric names and their corresponding mean values for train and test
    labels = list(metrics_train_mean.keys())
    
    train_scores = [metrics_train_mean[label] for label in labels]
    validation_scores = [metrics_val_mean[label] for label in labels]
    
    # Retrieve standard deviations if provided (for error bars)
    if metrics_train_std is not None:
        train_errors = [metrics_train_std[label] for label in labels]
    else:
        train_errors = None

    if metrics_val_std is not None:
        validation_errors = [metrics_val_std[label] for label in labels]
    else:
        validation_errors = None

    # 2. Set up the positions for the bars on the X-axis
    x = np.arange(len(labels))
    width = 0.35

    # 3. Set up the figure and axis
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # 4. Barplot
    rects1 = ax.bar(x - width/2, train_scores, width, 
                    label='Train', 
                    yerr=train_errors, capsize=5, 
                    color='#4C72B0', edgecolor='black', alpha=0.9)
    
    rects2 = ax.bar(x + width/2, validation_scores, width, 
                    label='Validation', 
                    yerr=validation_errors, capsize=5,
                    color='#DD8452', edgecolor='black', alpha=0.9)

    # 5. Customization
    ax.set_ylabel('Score', fontsize=12, weight='bold')
    ax.set_title(f'Overfitting Analysis: {model_name}', fontsize=16, weight='bold', pad=20)
    ax.set_xticks(x)
    ax.set_xticklabels(labels, fontsize=12, weight='bold')
    ax.set_ylim(0, 1.1)
    ax.legend(loc='upper right', fontsize=11)
    
    ax.grid(axis='y', linestyle='--', alpha=0.6)

    # Add labels on top of the bars
    autolabel(rects1, ax)
    autolabel(rects2, ax)

    # Adjust layout and show the plot
    plt.tight_layout()
    plt.show()