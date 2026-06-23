#%% IMPORTS
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

from pathlib import Path

from optuna import visualization as vis
from optuna.visualization import matplotlib as vis_plt
from sklearn.metrics import confusion_matrix, auc

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
                    ha='center', va='bottom', fontsize=8, weight='bold')

def plot_overfitting_bars(df_cv_results, 
                        title, 
                        output_dir=None, 
                        identifier=None, 
                        filtering=False):
    """
    Plots a grouped bar chart directly from raw CV fold data using seaborn,
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
    - filtering : bool, default=False
        If True, adds a subtitle indicating features whose coefficients where 
        forced to 0 in the baseline model were filtered out.
    
    Returns:
    - None
    """
    # 1. Set up the figure size and theme
    plt.figure(figsize=(11, 6))
    sns.set_theme(style="whitegrid")
    
    # 2. Filter out the test results if they are present, since we only want to 
    # compare train vs val
    df_cv_results = df_cv_results[df_cv_results['Dataset'].isin(['Train', 
                                                                'Validation'])]
    
    # 3. Plot the grouped bar chart with automatic error bars (standard 
    # deviation)
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
    plt.suptitle(f'Overfitting Analysis: {title}', fontsize=16, weight='bold')
    
    if filtering:
        plt.suptitle(f'Overfitting Analysis: {title} ' + '(filtering enabled)', 
                    fontsize=16, 
                    weight='bold')
    
    
    plt.ylabel('Score Value', fontsize=12, weight='bold')
    plt.xlabel('', fontsize=12)
    plt.xticks(ha='right')
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
def plot_metrics_bars(df, 
                    metrics, 
                    color_palette='viridis', 
                    baselines=None, 
                    output_dir=None, 
                    filtering=False):
    """
    Plots a grid of bar charts comparing multiple Test metrics across different 
    models using a long-format (tidy) DataFrame.

    Parameters:
    ----------
    - df : pandas.DataFrame
        Long-format DataFrame containing columns: 'Metric', 'Dataset', 'Score', 
        and 'Model'.
    - metrics : list
        List with the exact names of the metrics to plot (e.g., ['Accuracy', 
        'Precision', 'ROC-AUC']).
    - color_palette : str, default='viridis'
        Color palette for seaborn.
    - baselines : float or list, optional
        Horizontal line(s) to indicate reference value(s) (one for each metric).
    - output_dir : str, optional
        Directory where the plot will be saved as a PNG file.
    - filtering : bool, default=False
        If True, adds a subtitle indicating features whose coefficients where 
        forced to 0 were filtered out.

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
    cols = min(n_metrics, 3)
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
                ax.axhline(y=baseline_val, 
                        color='red', 
                        linestyle='--', 
                        linewidth=1.5, 
                        alpha=0.8, 
                        label=f'HATCH score (AUC = {baseline_val:.3f})')
            
        # Aesthetic customizations for each subplot
        ax.set_title(metric, fontsize=14, weight='bold')
        ax.set_xlabel('')
        ax.set_ylabel('', fontsize=12)
        ax.set_ylim(0, 1.05)
        
        # Adjust and rotate the model labels on the X-axis to prevent 
        # overlapping
        models_list = df_metric['Model'].unique()
        ax.set_xticks(range(len(models_list)))
        ax.set_xticklabels(models_list, 
                        fontsize=11, 
                        weight='bold', 
                        rotation=45, 
                        ha='right')
        
        # Use helper function to add numerical labels on top of the bars
        autolabel(ax.patches, ax)
        
        # Add legend
        if baselines is not None and baseline_val is not None:
            ax.legend(loc='upper right', 
                    frameon=True, 
                    facecolor='#f8f9fa', 
                    edgecolor='gray', 
                    framealpha=0.9)
    
    # 5. Delete any remaining empty subplots in the grid
    for j in range(i + 1, len(axes)):
        fig.delaxes(axes[j])
        
    # 6. Global title for the entire figure layout
    plt.suptitle('Evaluation on test set' + ('(filtering enabled)' if filtering else ''), 
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
def plot_model_curves(df, 
                    x_col, 
                    y_col, 
                    model_col='Model', 
                    curve_type='roc', 
                    prevalence=0.5, 
                    title=None, 
                    output_dir=None,
                    filtering=False):
    """
    Plots multiple ROC or Precision-Recall curves from a single long-format 
    DataFrame and includes the appropriate random classifier baseline.

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
        The proportion of positive samples in the dataset (used as baseline for 
        PR curve).
    - title : str, optional
        Custom title for the plot.
    - output_dir : str, optional
        Directory where the plot will be saved.
    - filtering : bool, default=False
        Whether to apply filtering to the data.

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
        
        plt.plot(group_sorted[x_col], 
                group_sorted[y_col], 
                label=label_with_auc, 
                linewidth=2)
    
    # 2. Configure axes and the baseline for the random classifier
    if curve_type.lower() == 'roc':
        # Random classifier
        plt.plot([0, 1], 
                [0, 1], 
                linestyle='--', 
                color='gray', 
                label='Random Classifier (AUC = 0.5)')
        
        plt.xlabel('False Positive Rate', fontsize=11, weight='bold')
        plt.ylabel('True Positive Rate', fontsize=11, weight='bold')
        default_title = 'ROC curves'
        legend_loc = 'lower right'
        
    elif curve_type.lower() == 'pr':
        # Random classifier: horizontal line at the prevalence value
        plt.axhline(y=prevalence, 
                    linestyle='--', 
                    color='gray', 
                    label=f'Random Classifier (Baseline = {prevalence:.2f})')
        
        plt.xlabel('Recall', fontsize=11, weight='bold')
        plt.ylabel('Precision', fontsize=11, weight='bold')
        default_title = 'Precision-Recall curves'
        legend_loc = 'upper right'
        
    else:
        raise ValueError("curve_type must be either 'roc' or 'pr'")
        
    # 3. Customize the plot
    plt.xlim([-0.02, 1.02])
    plt.ylim([-0.02, 1.02])
    plt.suptitle(title if title else default_title, 
            fontsize=14, 
            weight='bold')
    plt.title('Filtering enabled' if filtering else '', fontsize=10, weight='normal')
    plt.legend(loc=legend_loc, frameon=True)
    plt.tight_layout()
    
    # 4. Save the plot as a PNG file if output_dir is provided
    if output_dir is not None:
        path = Path(output_dir)
        file_path = path / f'models_{curve_type}_curves.png'
        plt.savefig(str(file_path), dpi=300, bbox_inches='tight')
    
    plt.show()

def plot_risk_score_curves(fpr, 
                        tpr, 
                        recalls, 
                        precisions, 
                        title=None, 
                        prevalence=None, 
                        output_dir=None):
    """
    Plots both the ROC and Precision-Recall curves for risk scores in a single 
    figure.

    Parameters:
    ----------
    - fpr : array-like
        False Positive Rates for the ROC curve.
    - tpr : array-like
        True Positive Rates for the ROC curve.
    - recalls : array-like
        Recall values for the Precision-Recall curve.
    - precisions : array-like
        Precision values for the Precision-Recall curve.
    - title : str, optional
        Title for the overall figure.
    - prevalence : float, optional
        The proportion of positive samples in the dataset (used as baseline for 
        PR curve).
    - output_dir : str, optional
        Directory where the plot will be saved as a PNG file.

    Returns:
    - None
    """
    
    # Calculate AUC for both curves
    roc_auc_ = auc(fpr, tpr)
    pr_auc_ = auc(recalls, precisions)
    
    # Set up the figure
    fig, ax = plt.subplots(1, 2, figsize=(12, 5))

    # Plot ROC Curve
    ax[0].plot(fpr, tpr, color='blue', lw=2, label=f'Risk score (AUC = {roc_auc_:.3f})')
    ax[0].plot([0, 1], 
            [0, 1], 
            color='black', 
            lw=2, 
            linestyle='--', 
            label='Random Classifier (AUC = 0.5)')
    ax[0].set_xlim([0.0, 1.0])
    ax[0].set_ylim([0.0, 1.05])
    ax[0].set_xlabel('False Positive Rate')
    ax[0].set_ylabel('True Positive Rate')
    ax[0].set_title('ROC Curve')
    ax[0].legend(loc='lower right')

    # Plot Precision-Recall Curve
    ax[1].plot(recalls, 
            precisions, 
            color='blue', 
            lw=2, 
            label=f'Risk score (AUC = {pr_auc_:.3f})')
    
    # Add horizontal line for random classifier baseline if prevalence is 
    # provided
    if prevalence is not None:
        ax[1].axhline(y=prevalence, 
                    color='black', 
                    lw=2, 
                    linestyle='--', 
                    label=f'Random Classifier (AUC = {prevalence:.3f})')
        
    ax[1].set_xlim([0.0, 1.0])
    ax[1].set_ylim([0.0, 1.05])
    ax[1].set_xlabel('Recall')
    ax[1].set_ylabel('Precision')
    ax[1].set_title('Precision-Recall Curve')
    ax[1].legend(loc='upper right')
    
    # Add grid
    for axis in ax:
        axis.grid(True)

    # Set the overall title for the figure
    fig.suptitle(title if title else 'Risk Score Performance', fontsize=16)
    
    # Save the figure as a PNG file if output_dir is provided
    if output_dir is not None:
        path = Path(output_dir)
        file_path = path / 'risk_scores_curves.png'
        plt.savefig(str(file_path), dpi=300, bbox_inches='tight')
        
    plt.show()