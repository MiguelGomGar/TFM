# Instrucciones

## Contexto

Esta carpeta es mi trabajo de fin de máster. Trata sobre la elaboración de modelos predictivos de Machine Learning (ML) para la recurrencia de fibrilación auricular (FA) en el contexto del ensayo clínico PREDIMAR.

## Objetivo

Objetivo principal: entrenar, optimizar y validar modelos de machine learning para predecir si un paciente va a tener recidiva de FA.

Objetivos secundarios:

- Comprobar si los modelos de ML mejoran la capacidad predictiva de los índices de riesgo clínicos.
- Estudiar si la información proteómica mejora significativamente la predicción con respecto a la información clínica.
- Identificar los modelos de ML que mejor rinden en función de la información disponible.

## Especificaciones

Código Python ejecutado a través de conda, ubicación del entorno: `C:/Users/Miguel/miniconda3/envs/tfm`. 

Todos los módulos se recgen en la carpeta `/src/`, agrupados por dominio:

- `/src/data/`: limpieza y transformación de datos.
- `/src/visualization/`: generación de gráficos.
- `/src/models/`: entrenamiento, optimización y evaluación de modelos.
- `/src/utils/`: helpers genéricos (definición de rutas, funciones para cargar datos o guardar resultados...).
- `/src/pipelines/`: módulos que ejecutan el análisis de los datos, el entrenamiento y la evaluación de los modelos. NO se definen funciones aquí (además del `main`), solo las importan desde otros archivo y las llaman dentro de `main`.
- `/src/config.py`: configuración general (definición de constantes).
