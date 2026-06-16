import numpy as np
import pandas as pd
import seaborn as sns
from pathlib import Path
import matplotlib.pyplot as plt
from optuna import visualization as vis
from optuna.visualization import matplotlib as vis_plt
from sklearn.metrics import confusion_matrix, auc

#%% OPTIMIZATION HISTORY
def plot_optimization_history(study, model_name=None, output_dir=None, identifier=None):
    """
    Creates and customizes an optimization history plot for a given study and model.
        
    Parameters:
    ----------
    - study : optuna.study.Study
        The Optuna study object containing the optimization results.
    - model_name : str, optional
        The name of the model to display in the title. If None, it tries to get it 
        from the study's or the best trial's user attributes.
    - output_dir : str or pathlib.Path, optional
        The directory path where the plot will be saved as a PNG file.
    - identifier : str, optional
        Identifier for the plot file.

    Returns:
    - None
    """
    # 1. Get the model's name
    if model_name is None:
        model_name = study.user_attrs.get('model_name', 
                        study.best_trial.user_attrs.get('model_name', 'Model'))

    # 2. Generate the plot using the matplotlib backend to avoid kaleido/browser dependencies
    ax = vis_plt.plot_optimization_history(study)
    fig = ax.figure
    
    # 3. Customization
    ax.set_title(f"{model_name} Optimization History", fontsize=16, weight='bold', pad=20)
    ax.set_xlabel("Number of Trials", fontsize=12, weight='bold')
    ax.set_ylabel("Metric Score Value", fontsize=12, weight='bold')
    ax.set_ylim(-0.02, 1.02)
    ax.grid(True, linestyle='--', alpha=0.7)
    ax.legend(loc='lower right', frameon=True, facecolor='#f8f9fa', edgecolor='gray', framealpha=0.9)
    
    # 4. Save the plot as a PNG file if output_dir is provided
    if output_dir is not None:
        file_path = output_dir / f"optimization_history_{identifier}.png"
        plt.savefig(str(file_path), dpi=300, bbox_inches='tight')
    
    # 5. Show the plot
    plt.tight_layout()
    plt.show()
    
    
#%% CONFUSION MATRIX
def plot_confusion_matrix(y_true, y_pred, 
            class_names=None, 
            title='Confusion Matrix', 
            cmap='Blues',
            output_dir=None,
            identifier=None):
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
    - output_dir : str, optional
        Directory where the plot will be saved.
    - identifier : str, optional
        Identifier for the plot file.
    
    Returns:
    - None
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
        ax.set_xticklabels(class_names, fontsize=12, rotation=45, ha='right')
        ax.set_yticklabels(class_names, fontsize=12, rotation=0)
    
    # 8. Save the plot as a PNG file if output_dir is provided
    if output_dir is not None:
        path = Path(output_dir)
        file_path = path / f'confusion_matrix_{identifier}.png'
        plt.savefig(str(file_path), dpi=300, bbox_inches='tight')
    
    # 9. Adjust layout and show the plot
    plt.tight_layout()
    plt.show()

#%% OVERFITTING ANALYSIS
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

def plot_overfitting_bars(df_cv_results, title, output_dir=None, identifier=None):
    """
    Plots a grouped bar chart directly from raw CV fold data using Seaborn,
    automatically computing means and standard deviation error bars.
    
    Parameters:
    ----------
    - df_cv_results : pandas.DataFrame
        DataFrame in long format with columns: 'Metric', 'Dataset', 'Score'.
    - title : str
        Title for the plot.
    - output_dir : str, optional
        Directory where the plot will be saved as a PNG file.
    - identifier : str, optional
        Identifier for the plot file.
    
    Returns:
    - None
    """
    # 1. Set up the figure size and theme
    plt.figure(figsize=(11, 6))
    sns.set_theme(style="whitegrid")
    
    # 2. Filter out the test results if they are present, since we only want to compare train vs val
    df_cv_results = df_cv_results[df_cv_results['Dataset'].isin(['Train', 'Validation'])]
    
    # 3. Plot the grouped bar chart with automatic error bars (standard deviation)
    ax = sns.barplot(
        data=df_cv_results,
        x='Metric',
        y='Score',
        hue='Dataset',
        errorbar='sd', 
        palette=['#4C72B0', '#DD8452'], 
        edgecolor='black',
        linewidth=1.2,
        alpha=0.9
    )
    
    # 4. Customize the plot aesthetics
    plt.title(f'Overfitting Analysis: {title}', fontsize=16, weight='bold', pad=20)
    plt.ylabel('Score Value', fontsize=12, weight='bold')
    plt.xlabel('', fontsize=12)
    plt.xticks(rotation=45, ha='right')
    plt.ylim(0, 1.1)
    plt.legend(title='Dataset', loc='upper right', bbox_to_anchor=(1.02, 1))
    
    # 5. Save the plot as a PNG file if output_dir is provided
    if output_dir is not None:
        path = Path(output_dir)
        file_path = path / f'overfitting_analysis_{identifier}.png'
        plt.savefig(str(file_path), dpi=300, bbox_inches='tight')
    
    # 6. Adjust layout and show the plot
    plt.tight_layout()
    plt.show()

#%% METRICS COMPARISON
def plot_metrics_bars(df, metrics, color_palette='viridis', baselines=None, output_dir=None):
    """
    Plots a grid of bar charts comparing multiple Test metrics across different models
    using a long-format (tidy) DataFrame.

    Parameters:
    ----------
    - df : pandas.DataFrame
        Long-format DataFrame containing columns: 'Metric', 'Dataset', 'Score', and 'Model'.
    - metrics : list
        List with the exact names of the metrics to plot (e.g., ['Accuracy', 'Precision', 'ROC-AUC']).
    - color_palette : str, default='viridis'
        Color palette for seaborn.
    - baselines : float or list, optional
        Horizontal line(s) to indicate reference value(s) (one for each metric).
    - output_dir : str, optional
        Directory where the plot will be saved as a PNG file.
    
    Returns:
    - None
    """
    # 1. Filter the DataFrame to keep only the 'Test' dataset
    df_test = df[df['Dataset'] == 'Test']
    
    if df_test.empty:
        print("Warning: No 'Test' data found in the 'Dataset' column. Please check your DataFrame.")
        return

    # 2. Determine the grid size based on the number of requested metrics
    n_metrics = len(metrics)
    cols = min(n_metrics, 3) # <-- El cambio clave: usar máximo 3, pero menos si hay pocas métricas
    rows = int(np.ceil(n_metrics / cols))
    
    # 3. Set up the figure and axes for matplotlib
    fig, axes = plt.subplots(rows, cols, figsize=(5 * cols, 5 * rows))
    axes = axes.flatten() 
    sns.set_theme(style="whitegrid")
    
    # Initialize the index variable to handle removing empty axes later
    i = 0
    
    # 4. Iterate over the metrics and plot each one in its respective subplot
    for i, metric in enumerate(metrics):
        # Check if the requested metric exists in the test data
        if metric not in df_test['Metric'].values:
            print(f"Warning: '{metric}' metric does not exist in the Test data. It will be skipped.")
            continue
            
        ax = axes[i]
        
        # Filter the specific subset of data for the current metric
        df_metric = df_test[df_test['Metric'] == metric]
        
        # Plot the bar chart interacting with the long-format columns
        sns.barplot(
            data=df_metric,
            x='Model', 
            y='Score', 
            hue='Model',
            palette=color_palette,
            edgecolor='black',
            linewidth=1.5,
            legend=False,
            ax=ax 
        )
        
        # Add the horizontal baseline at a reference value
        if baselines is not None:
            # Determine the baseline value for the current metric
            if isinstance(baselines, list):
                baseline_val = baselines[i] if i < len(baselines) else None
                
            elif isinstance(baselines, (int, float)):
                baseline_val = baselines
                
            else:
                baseline_val = None

            # Plot the line only if a valid baseline value is provided
            if baseline_val is not None:
                ax.axhline(y=baseline_val, color='red', linestyle='--', linewidth=1.5, alpha=0.8)
            
        # Aesthetic customizations for each subplot
        ax.set_title(metric, fontsize=14, weight='bold')
        ax.set_xlabel('')
        ax.set_ylabel('', fontsize=12)
        ax.set_ylim(0, 1.05)
        
        # Adjust and rotate the model labels on the X-axis to prevent overlapping
        models_list = df_metric['Model'].unique()
        ax.set_xticks(range(len(models_list)))
        ax.set_xticklabels(models_list, fontsize=11, weight='bold', rotation=45, ha='right')
        
        # Keep your helper function to add numerical labels on top of the bars
        autolabel(ax.patches, ax)
    
    # 5. Delete any remaining empty subplots in the grid
    for j in range(i + 1, len(axes)):
        fig.delaxes(axes[j])
        
    # 6. Global title for the entire figure layout
    plt.suptitle('Comparison of Test Metrics Across Models', 
                fontsize=18, weight='bold', y=1.02)
    
    # 7. Save the entire figure as a PNG file if output_dir is provided
    if output_dir is not None:
        path = Path(output_dir)
        file_path = path / 'test_metrics_comparison.png'
        plt.savefig(str(file_path), dpi=300, bbox_inches='tight')
    
    # 8. Adjust the layout and show the plot
    plt.tight_layout()
    plt.show()
    
#%% CURVES COMPARISON
def plot_model_curves(df, x_col, y_col, model_col='Model', 
                    curve_type='roc', prevalence=0.5, title=None, output_dir=None):
    """
    Plots multiple ROC or Precision-Recall curves from a single long-format DataFrame
    and includes the appropriate random classifier baseline.

    Parameters:
    ----------
    - df : pandas.DataFrame
        The long-format DataFrame containing the curve coordinates.
    - x_col : str
        Name of the column for the X-axis (e.g., 'fpr' or 'recall').
    - y_col : str
        Name of the column for the Y-axis (e.g., 'tpr' or 'precision').
    - model_col : str, default='Model'
        Name of the column that identifies each model.
    - curve_type : str, default='roc'
        The type of curve to plot. Options are 'roc' or 'pr'.
    - prevalence : float, default=0.5
        The proportion of positive samples in the dataset (used as baseline for PR curve).
    - title : str, optional
        Custom title for the plot.
    - output_dir : str, optional
        Directory where the plot will be saved.
    
    Returns:
    - None
    """
    plt.figure(figsize=(8, 6))
    sns.set_theme(style="whitegrid")
    
    # 1. Group the DataFrame by model and plot each curve
    for model_name, group in df.groupby(model_col):
        group_sorted = group.sort_values(by=x_col)
        
        # Calculate AUC for the legend
        area = auc(group_sorted[x_col], group_sorted[y_col])
        label_with_auc = f"{model_name} (AUC = {area:.3f})"
        
        plt.step(group_sorted[x_col], group_sorted[y_col], label=label_with_auc, linewidth=2)
    
    # 2. Configure axes and the baseline for the random classifier
    if curve_type.lower() == 'roc':
        # Random classifier: diagonal line from (0,0) to (1,1)
        plt.plot([0, 1], [0, 1], linestyle='--', color='gray', label='Random Classifier (AUC = 0.5)')
        
        plt.xlabel('False Positive Rate', fontsize=11, weight='bold')
        plt.ylabel('True Positive Rate', fontsize=11, weight='bold')
        default_title = 'ROC curves'
        legend_loc = 'lower right'
        
    elif curve_type.lower() == 'pr':
        # Random classifier: horizontal line at the prevalence value
        plt.axhline(y=prevalence, linestyle='--', color='gray', label=f'Random Classifier (Baseline = {prevalence:.2f})')
        
        plt.xlabel('Recall', fontsize=11, weight='bold')
        plt.ylabel('Precision', fontsize=11, weight='bold')
        default_title = 'Precision-Recall curves'
        legend_loc = 'upper right'
        
    else:
        raise ValueError("curve_type must be either 'roc' or 'pr'")
        
    # 3. Customize the plot
    plt.xlim([-0.02, 1.02])
    plt.ylim([-0.02, 1.02])
    plt.title(title if title else default_title, fontsize=13, weight='bold', pad=15)
    plt.legend(loc=legend_loc, frameon=True)
    plt.tight_layout()
    
    # 4. Save the plot as a PNG file if output_dir is provided
    if output_dir is not None:
        path = Path(output_dir)
        file_path = path / f'{curve_type}_curves.png'
        plt.savefig(str(file_path), dpi=300, bbox_inches='tight')
    
    plt.show()