import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from sklearn.metrics import confusion_matrix

def plot_cm(y_true, y_pred, 
            class_names=None, 
            title='Confusion Matrix', 
            cmap='Blues'):
    """
    Dibuja una matriz de confusión, mostrando números absolutos y porcentajes.

    Parameters:
    ----------
    - y_true : array-like
        Etiquetas reales.
    - y_pred : array-like
        Etiquetas predichas por el modelo.
    - class_names : list, opcional
        Lista con los nombres de las clases. Si es None, usa números.
    - title : str, default='Confusion Matrix'
        Título del gráfico.
    - cmap : str, default='Blues'
        Paleta de colores de seaborn/matplotlib.
    """
    
    # 1. Calcular la matriz de confusión
    cm = confusion_matrix(y_true, y_pred)
    
    # 2. Calcular los porcentajes por fila (para ver el acierto/fallo real de cada clase)
    cm_percentages = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
    
    # 3. Crear las etiquetas de texto combinando conteo y porcentaje
    labels = [f"{v1}\n({v2:.1%})" for v1, v2 in zip(cm.flatten(), cm_percentages.flatten())]
    labels = np.asarray(labels).reshape(cm.shape)
    
    # 4. Configurar el tamaño del lienzo
    plt.figure(figsize=(8, 6))
    
    # 5. Dibujar el mapa de calor (heatmap)
    ax = sns.heatmap(cm, 
                    annot=labels, 
                    fmt='', 
                    cmap=cmap, 
                    cbar=True, 
                    linewidths=1, 
                    linecolor='white', 
                    annot_kws={"size": 12, "weight": "bold"})
    
    # 6. Estilizar títulos y ejes
    plt.title(title, fontsize=16, pad=20, weight='bold')
    plt.ylabel('True values', fontsize=14, weight='bold')
    plt.xlabel('Predicted values', fontsize=14, weight='bold')
    
    # 7. Añadir nombres a las clases si se proporcionan
    if class_names is not None:
        ax.set_xticklabels(class_names, fontsize=12)
        ax.set_yticklabels(class_names, fontsize=12, rotation=0)
        
    # Ajustar el diseño para que nada quede cortado
    plt.tight_layout()
    plt.show()