import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix

def plot_cm(y_true, y_pred, 
            class_names=None, 
            title='Confusion Matrix', 
            cmap='Blues'):
    """
    Plots a confusion matrix with absolute counts and relative percentages as text, 
    but strictly uses the relative percentages to drive the color scale.

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
    
    # 1. Compute the absolute confusion matrix
    cm = confusion_matrix(y_true, y_pred)
    
    # 2. Compute the percentages per row (normalization by true labels)
    cm_percentages = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
    
    # 3. Create the text labels by combining absolute counts and percentages
    labels = [f"{v1}\n({v2:.1%})" for v1, v2 in zip(cm.flatten(), cm_percentages.flatten())]
    labels = np.asarray(labels).reshape(cm.shape)
    
    # 4. Set up the figure size
    plt.figure(figsize=(8, 6))
    
    # 5. Draw the heatmap
    # CRITICAL FIX: Pass 'cm_percentages' as the data so the color scale 
    # uses relative values, while keeping 'annot=labels' for the dual text.
    ax = sns.heatmap(cm_percentages, 
                    annot=labels, 
                    fmt='', 
                    cmap=cmap, 
                    vmin=0.0,      # Min color is 0%
                    vmax=1.0,      # Max color is 100%
                    cbar=True,
                    cbar_kws={'label': 'Proportion of Actual Class'}, # Legend for the colorbar
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
    ax.legend(loc='upper right', fontsize=11, bbox_to_anchor=(1.02, 1))
    
    ax.grid(axis='y', linestyle='--', alpha=0.6)

    # Add labels on top of the bars
    # autolabel(rects1, ax)
    # autolabel(rects2, ax)

    # Adjust layout and show the plot
    plt.tight_layout()
    plt.show()

def plot_combined_roc_curves(df_long, title='ROC Curves'):
    """
    Plots multiple ROC curves on the same axes.
    
    Parameters:
    - df_long : pandas.DataFrame
        DataFrame in long format with three columns:
            - model's names.
            - false positive rate values.
            - true positive rate values.
    """
    
    # Create the figure and set the theme
    plt.figure(figsize=(10, 7))
    sns.set_theme(style="whitegrid")
    
    # Plot the ROC curves for each model
    sns.lineplot(
        data=df_long,
        x='False Positive Rate',
        y='True Positive Rate',
        hue='Model',
        drawstyle='steps-post',
        linewidth=2.5,
        alpha=0.8,
        errorbar=None
    )
    
    # Add a diagonal line for the baseline
    plt.plot([0, 1], [0, 1], 
            color='black', linestyle='--', linewidth=1.5, 
            label='Random Classifier')
    
    # Title and labels
    plt.title(title, fontsize=16, weight='bold', pad=20)
    plt.xlabel('False Positive Rate', fontsize=14, weight='bold')
    plt.ylabel('True Positive Rate', fontsize=14, weight='bold')
    
    # Limits of the axes
    plt.xlim([-0.02, 1.02])
    plt.ylim([-0.02, 1.05])
    
    # Legend outside the plot (the trick we saw before)
    plt.legend(title='Models', title_fontsize='12', fontsize='11', 
            loc='upper left', bbox_to_anchor=(1.02, 1))
    
    plt.tight_layout()
    plt.show()

def plot_combined_pr_curves(df_long, title='Precision-Recall Curves'):
    """
    Plots multiple Precision-Recall curves on the same axes.
    
    Parameters:
    - df_long : pandas.DataFrame
        DataFrame in long format with three columns:
            * model's names.
            * recall values.
            * precision values.
    """
    
    # Create the figure and set the theme
    plt.figure(figsize=(10, 7))
    sns.set_theme(style="whitegrid")
    
    # Plot the Precision-Recall curves for each model
    sns.lineplot(
        data=df_long,
        x='Recall',
        y='Precision',
        hue='Model',
        drawstyle='steps-post',
        linewidth=2.5,
        alpha=0.8,
        errorbar=None
    )
    
    # Add a horizontal line for the baseline
    plt.axhline(y=0.36, 
                color='black', linestyle='--', linewidth=1.5, 
                label='Random Classifier')
    
    # Title and labels
    plt.title(title, fontsize=16, weight='bold', pad=20)
    plt.xlabel('Recall', fontsize=14, weight='bold')
    plt.ylabel('Precision', fontsize=14, weight='bold')
    
    # Limits of the axes
    plt.xlim([-0.02, 1.02])
    plt.ylim([-0.02, 1.05])
    
    # Legend outside the plot (the trick we saw before)
    plt.legend(title='Models', title_fontsize='12', fontsize='11', 
            loc='upper left', bbox_to_anchor=(1.02, 1))
    
    plt.tight_layout()
    plt.show()

def plot_metric_comparison(df, metric_name, color_palette='viridis'):
    """
    Plots a bar chart comparing a specific metric across multiple models.

    Parameters:
    ----------
    - df : pandas.DataFrame
        DataFrame with models as index and metrics as columns.
    - metric_name : str
        The exact name of the column containing the metric to plot (e.g., 'PR-AUC').
    - color_palette : str, default='viridis'
        Color palette for seaborn.
    """
    # 1. Check if the metric exists
    if metric_name not in df.columns:
        print(f"Error: The metric does not exist in the DataFrame.")
        return
    
    # 2. Set up the figure and theme
    plt.figure(figsize=(8, 6))
    sns.set_theme(style="whitegrid")
    
    # 3. Plot the bar chart
    ax = sns.barplot(
        x=df.index, 
        y=df[metric_name], 
        hue=df.index,
        palette=color_palette,
        edgecolor='black',
        linewidth=1.5,
        legend=False
    )
    
    # 4. Customizations
    plt.title(f'Models comparison: {metric_name} (Test)', fontsize=16, weight='bold', pad=20)
    plt.xlabel('Model', fontsize=14, weight='bold')
    plt.ylabel(metric_name, fontsize=14, weight='bold')
    
    # Fix the y-axis limits to better visualize differences
    plt.ylim(0, 1.05)
    
    # Add value labels on top of the bars
    autolabel(ax.patches, ax)
        
    plt.tight_layout()
    plt.show()

def plot_all_metrics_comparison(df, 
                                metrics=['Accuracy', 'Precision', 'Recall', 'Specificity', 'F1-Score', 'PR-AUC'], 
                                color_palette='viridis'):
    """
    Plots a grid of bar charts comparing multiple metrics across different models.

    Parameters:
    ----------
    - df : pandas.DataFrame
        DataFrame with models as index and metrics as columns.
    - metrics : list
        List with the exact names of the metric columns to plot.
    - color_palette : str, default='viridis'
        Color palette for seaborn.
    """
    # 1. Determine the grid size based on the number of metrics
    n_metrics = len(metrics)
    cols = 3
    rows = int(np.ceil(n_metrics / cols)) 
    
    # 2. Set up the figure and axes
    fig, axes = plt.subplots(rows, cols, figsize=(5 * cols, 5 * rows))
    axes = axes.flatten() 
    sns.set_theme(style="whitegrid")
    
    # 3. Iterate over the metrics and plot each one in its respective subplot
    for i, metric in enumerate(metrics):
        if metric not in df.columns:
            print(f"Warning: The '{metric}' metric does not exist in the DataFrame. It will be skipped.")
            continue
            
        ax = axes[i]
        
        # Plot the bar chart
        sns.barplot(
            x=df.index, 
            y=df[metric], 
            hue=df.index,
            palette=color_palette,
            edgecolor='black',
            linewidth=1.5,
            legend=False,
            ax=ax 
        )
        
        # Customizations
        ax.set_title(metric, fontsize=14, weight='bold')
        ax.set_xlabel('')
        ax.set_ylabel('', fontsize=12)
        ax.set_ylim(0, 1.05)
        
        # Rotate x-axis labels for better readability
        ax.set_xticks(range(len(df.index)))
        ax.set_xticklabels(df.index, fontsize=12, weight='bold', rotation=45, ha='right')
        
        # Add value labels on top of the bars
        autolabel(ax.patches, ax)
    
    # 4. Delete any unused subplots (in case the number of metrics is less than rows*cols)
    for j in range(i + 1, len(axes)):
        fig.delaxes(axes[j])
        
    # 5. Global title for the entire figure
    plt.suptitle('Comparison of Metrics Across Models', 
                fontsize=18, weight='bold', y=1.02)
    
    # Adjusts the layout to prevent overlap and show the plot
    plt.tight_layout()
    plt.show()