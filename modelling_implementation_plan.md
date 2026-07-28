# Plan de implementación — Fase de modelado (TFM: recurrencia de FA)

## 1. Contexto

`ml_methodology.md` y el §2.4 del `borrador_v2.docx` describen un diseño experimental en tres fases
(clínica global → proteómica pura → multimodal integrado) que hoy **no existe en código ejecutable**:

- `src/models/data_train_eval.py` está **vacío (0 bytes)** y `src/models/__init__.py` también, de modo
  que `src/pipelines/09_clinical_data_modelling.py` y `10_proteomic_data_modelling.py` fallan en el
  `import` (`optimize_model_random_search`, `plot_internal_validation`, `plot_external_validation` y
  las versiones «modelo» de `plot_roc_curves`/`plot_pr_curves` ya no existen en el árbol de trabajo).
- Ambos pipelines repiten **8 veces el mismo bloque de ~40 líneas** (uno por modelo) y llevan un
  `TODO` en español pidiendo justo la refactorización que aquí se plantea.
- `results/models/` no existe en disco: no hay ningún resultado de modelado producido con la
  estructura actual, y §3.4 y §3.5 del borrador son epígrafes vacíos.
- No existe dataset multimodal: `code` se elimina en `05_clinical_data_cleaning.py`, así que la
  subcohorte emparejada de la Fase 3 hay que reconstruirla.

**Resultado buscado:** cuatro fases de modelado ejecutables e independientes, con código modularizado
(`src/models` para entrenar/evaluar, `src/visualization` para graficar, `src/pipelines` para
orquestar), que produzcan de forma reproducible los 5 gráficos + CSV por fase y la comparación
multimodal vs. clínico exigidos.

### Decisiones acordadas

| Decisión | Elección |
|---|---|
| Filtrado por Elastic Net | Fase 1 sin filtrar; Fases 1b, 2 y 3 con filtrado |
| Dataset multimodal | Pipeline nuevo de preparación (no se toca el 05) |
| Estructura Fase 3 | Un pipeline entrena ambos brazos + pipeline ligero de comparación |
| Presupuesto de búsqueda | Configurable por fase: clínica `n_iter=200`, proteómica/multimodal `n_iter=50`, `cv=5` en todas |
| Ejecución | **Fuera del alcance de la implementación**: la lanzará el usuario en un Codespace |

### Punto de atención: la API de `LogisticRegression` en scikit-learn 1.9

El entorno `tfm_python` tiene **scikit-learn 1.9.0**, donde la firma es
`LogisticRegression(penalty='deprecated', *, C=1.0, l1_ratio=0.0, …)`:

> *`penalty` was deprecated in version 1.8 and will be removed in 1.10. Use `l1_ratio` and `C`
> instead. `l1_ratio=0` for `penalty='l2'`, `l1_ratio=1` for `penalty='l1'`, `l1_ratio` set to any
> float between 0 and 1 for `penalty='elasticnet'`.*

Consecuencia: el `LogisticRegression(random_state=seed, solver="saga", max_iter=10000)` que ya usa el
pipeline 09 es **correcto tal cual** — `l1_ratio` (que viene del espacio de búsqueda
`clinical_hyperparameters_search_space["EN"]`, `uniform(0.1, 0.9)` → siempre > 0) determina por sí
solo la penalización elastic-net, y los coeficientes sí pueden anularse. **No hay que añadir
`penalty="elasticnet"`**: hoy emitiría un `FutureWarning` y a partir de 1.10 rompería.

Lo que sí se documenta en `model_zoo.py` con un comentario explícito:

- `solver="saga"` es **obligatorio** (único solver que soporta elastic-net) y no debe entrar en la
  rejilla de búsqueda;
- el espacio de búsqueda de `EN` debe mantener `l1_ratio` estrictamente > 0, porque con `l1_ratio=0`
  (el nuevo valor por defecto) la penalización es L2 pura y **ningún coeficiente llegaría a cero**,
  dejando sin efecto el filtrado de las fases 1b, 2 y 3.

### Defecto detectado que el plan corrige

**Asignación encadenada en `get_relevant_features`.** `src/utils/results_saving.py:85` asigna
`df_relevant["Abs_Coefficient"]` sobre una vista (`df_coefficients[...]`); con pandas 3.0 y
copy-on-write eso emite aviso o se descarta silenciosamente. Se corrige con un `.copy()`.

---

## 2. Arquitectura

```
src/
├── config.py                              ← (M) nueva sección MODELLING + espacio multimodal
├── models/
│   ├── model_zoo.py                       ← (N) catálogo de estimadores y factoría de pipelines
│   ├── model_training.py                  ← (N) búsqueda aleatoria + CV interna + run_modelling_phase
│   ├── model_evaluation.py                ← (N) métricas y curvas en test externo
│   ├── feature_selection.py               ← (N) filtrado por Elastic Net + listas conservadas/eliminadas
│   ├── data_preprocessing.py              ← (=) se reutiliza tal cual
│   └── data_train_eval.py                 ← (D) fichero vacío: eliminar
├── visualization/
│   ├── model_evaluation.py                ← (N) los 5 gráficos + el de comparación
│   └── models.py                          ← (=) intacto (lo usa el pipeline 08)
├── utils/
│   ├── paths.py                           ← (M) rutas de las nuevas fases
│   └── results_saving.py                  ← (M) fix del `.copy()` + guardado de listas en CSV
└── pipelines/
    ├── 09_clinical_data_modelling.py            ← (R) Fase 1 — clínica completa
    ├── 10_clinical_filtered_modelling.py        ← (N) Fase 1b — clínica filtrada por EN
    ├── 11_proteomic_data_modelling.py           ← (R) Fase 2 — proteómica (renombra el 10 actual)
    ├── 12_multimodal_data_preparation.py        ← (N) construye data/clean/multimodal_data.parquet
    ├── 13_multimodal_data_modelling.py          ← (N) Fase 3 — brazo clínico-emparejado + multimodal
    └── 14_modality_comparison.py                ← (N) barplots de comparación por modelo
```

(N)=nuevo, (M)=modificar, (R)=reescribir, (D)=borrar, (=)=sin cambios.

**Principio rector:** los pipelines no contienen lógica de ML ni de dibujo; sólo cargan datos, llaman
a `src/models/*` y `src/visualization/*`, y guardan con `save_figure`/`save_csv`. Es exactamente el
patrón de los pipelines 01–08, que este plan respeta (sin `argparse`, constantes de módulo,
`logger = setup_logger(Path(__file__).stem)`, `main()`, `if __name__ == "__main__"`).

---

## 3. `src/config.py` y `src/utils/paths.py`

Se añade una sección `# %% MODELLING — configuración común` y se rellena la sección
`# %% MULTIMODAL DATA MODELLING`, hoy vacía. `SEED = 42` ya existe y pasa a ser la **única** semilla
del proyecto (los pipelines actuales mezclan `seed = 7214` para modelos y `42` para split/CV, sin
justificación).

```python
# %% MODELLING — configuración común (pipelines 09–14)
TEST_SIZE = 0.2
CV_N_SPLITS = 5
N_JOBS = -1

SCORING_METRICS = {"ROC-AUC": "roc_auc", "PR-AUC": "average_precision"}
OBJECTIVE_METRIC = "PR-AUC"          # la memoria exige optimizar PR-AUC

MODEL_ORDER = ["EN", "SVM", "DT", "RF", "ET", "AB", "GB", "MLP"]
MODEL_DISPLAY_NAMES = {
    "EN": "Elastic Net", "SVM": "Support Vector Machine", "DT": "Decision Tree",
    "RF": "Random Forest", "ET": "Extra Trees", "AB": "AdaBoost",
    "GB": "Gradient Boosting", "MLP": "Multilayer Perceptron",
}
SCALED_MODELS = ["EN", "SVM", "MLP"]  # usan get_full_preprocessor; el resto, get_trees_preprocessor

# Presupuesto de búsqueda por fase (clave = identificador de fase)
SEARCH_N_ITER = {
    "clinical": 200, "clinical_filtered": 200, "proteomic": 50,
    "clinical_matched": 200, "multimodal": 50,
}

# Benchmark: BASE-AF2, el mejor score clínico (§3.3 del borrador v2)
RISK_SCORE_BASELINE = {"ROC-AUC": 0.632, "PR-AUC": 0.480}

# Paleta de modelado (coherente con la casa: azul/rojo de CATEGORICAL_COLOR_MAP)
INTERNAL_VALIDATION_COLORS = {"Train": "#2563eb", "Validation": "#f59e0b"}
BASELINE_COLOR = "#e11d48"
MODEL_BAR_COLOR = "#16a085"
MODALITY_COLORS = {"Clinical": "#2563eb", "Multimodal": "#e11d48"}
MODEL_PALETTE = ["#2563eb", "#e11d48", "#16a085", "#f59e0b",
                 "#7c3aed", "#0891b2", "#65a30d", "#1e3a8a"]

# %% MULTIMODAL DATA MODELLING — pipeline 13
multimodal_hyperparameters_search_space = { ... }   # mismos rangos que el proteómico (p≈387 ≈ p=361)
```

`multimodal_hyperparameters_search_space` se define **explícitamente** (no como alias del proteómico)
para que pueda divergir sin efectos colaterales; los rangos iniciales se copian del proteómico porque
la dimensionalidad es equivalente.

`src/utils/paths.py` añade:

```python
CLINICAL_MODELS_MATCHED_DIR  = MODELS_DIR / "clinical_data_matched"
MULTIMODAL_MODELS_DIR        = MODELS_DIR / "multimodal_data"
MODALITY_COMPARISON_DIR      = MODELS_DIR / "modality_comparison"
CLEANED_MULTIMODAL_DATA_PATH = CLEAN_DATA_DIR / "multimodal_data.parquet"
```

(`CLINICAL_MODELS_DIR`, `CLINICAL_MODELS_FILTERED_DIR` y `PROTEOMIC_MODELS_DIR` ya existen.)

---

## 4. `src/models` — entrenamiento y evaluación

### 4.1 `model_zoo.py`

```python
def get_estimator(abbreviation: str, seed: int) -> BaseEstimator
def build_model_pipeline(X: pd.DataFrame, abbreviation: str, seed: int) -> Pipeline
```

- `get_estimator` devuelve el clasificador con sus parámetros fijos:
  `EN` → `LogisticRegression(solver="saga", max_iter=10000, random_state=seed)` — **sin `penalty`**,
  que está deprecado en sklearn 1.8+; la penalización elastic-net la fija `l1_ratio` desde el espacio
  de búsqueda; `SVM` → `SVC(random_state=seed)`; `AB` →
  `AdaBoostClassifier(estimator=DecisionTreeClassifier(random_state=seed), random_state=seed)`
  (necesario porque el espacio de búsqueda usa claves `clf__estimator__*`); resto, el estimador con
  `random_state=seed`.
- `build_model_pipeline` elige `get_full_preprocessor` si `abbreviation in SCALED_MODELS` y
  `get_trees_preprocessor` en caso contrario, y devuelve
  `Pipeline([("preprocessor", ...), ("clf", ...)])` — nombres de paso obligatorios, porque
  `get_relevant_features` y todos los espacios de búsqueda (`clf__…`) dependen de ellos.

### 4.2 `model_training.py`

```python
def optimize_model(pipeline, param_distributions, X_train, y_train, cv, aim,
                   n_iter, seed, n_jobs=-1) -> tuple[Pipeline, dict]
def cross_validate_model(model, X_train, y_train, cv, scoring) -> pd.DataFrame
def summarize_internal_validation(cv_results, model_name) -> pd.DataFrame
def run_modelling_phase(...) -> None
```

- `optimize_model`: `RandomizedSearchCV(..., scoring=aim, cv=cv, n_iter=n_iter, random_state=seed,
  n_jobs=n_jobs, refit=True)`; devuelve `best_estimator_` y `best_params_`.
- `cross_validate_model`: `cross_validate(..., scoring=SCORING_METRICS, return_train_score=True)` sobre
  el **conjunto de entrenamiento** y devuelve formato largo `["Metric", "Dataset", "Score", "Fold"]`
  con `Dataset ∈ {Train, Validation}` — exactamente lo que consume el gráfico 1 y lo que espera
  `save_metrics_results` (que ya elimina la columna `Fold`).
- `run_modelling_phase` sustituye el bloque copiado 8 veces y ejecuta, en orden: (1) entrena y
  optimiza el EN; (2) si `apply_filter`, deriva las listas conservadas/eliminadas con
  `feature_selection` y recorta `X_train`/`X_test`; (3) entrena y optimiza los 7 modelos restantes;
  (4) CV interna y evaluación en test; (5) guarda modelos (`save_model`), métricas
  (`save_metrics_results`), curvas (`save_curves_results`), `best_params.csv` y los 5 gráficos con sus
  CSV gemelos.

### 4.3 `model_evaluation.py`

```python
def get_decision_scores(model, X) -> np.ndarray
def evaluate_on_test(model, X_test, y_test) -> tuple[pd.DataFrame, dict]
def build_auc_table(metrics_df, metric) -> pd.DataFrame
def build_modality_comparison_table(metrics_by_modality: dict) -> pd.DataFrame
```

- `get_decision_scores` usa `predict_proba(X)[:, 1]` y cae a `decision_function(X)` si no existe
  — **imprescindible**, porque `SVC` se instancia sin `probability=True`.
- `evaluate_on_test` calcula sobre el **test externo** `Accuracy, Precision, Recall, Specificity, F1,
  ROC-AUC, PR-AUC` (especificidad vía `recall_score(..., pos_label=0)`, como en el código histórico)
  y devuelve las curvas (`fpr, tpr, precision, recall`) para los gráficos 2 y 3.
- `build_modality_comparison_table` cruza los `models_metrics.csv` de dos brazos (`Dataset == "Test"`,
  `Metric ∈ {ROC-AUC, PR-AUC}`) y produce `["Model", "Metric", "Modality", "Score"]`.

### 4.4 `feature_selection.py`

```python
def select_features_with_elastic_net(fitted_en_pipeline, output_dir, identifier="EN", logger=None)
    -> tuple[list[str], list[str], pd.DataFrame]
def apply_feature_filter(X, irrelevant_features) -> pd.DataFrame
```

- Comprueba que el pipeline recibido lleva `solver="saga"` y que el `l1_ratio` seleccionado por la
  búsqueda es > 0; si no, avisa por logger de que el filtrado no puede anular coeficientes.
- Envuelve `get_relevant_features` (`src/utils/results_saving.py`), que ya limpia los prefijos
  `num__` / `ord_<col>__` y por tanto devuelve nombres de columna originales, directamente usables en
  `X.drop(columns=...)`.
- Persiste, en el directorio de la fase:
  - `features_kept.csv` — variables conservadas (con su coeficiente y rango por |coef|),
  - `features_removed.csv` — variables eliminadas,
  - `feature_selection_EN.csv` — tabla completa `Feature, Coefficient, Selected`,
  - `feature_selection_EN.joblib` — vía `save_feature_selection_results` (ya existente).
- **Guarda:** si el EN anula *todas* las variables, registra un `warning` y devuelve el conjunto
  completo sin filtrar, para que la fase no falle con un `X` vacío.
- El ajuste del EN se hace **sólo sobre `X_train`**, de modo que el filtrado no introduce fuga hacia
  el conjunto de test externo.

---

## 5. `src/visualization/model_evaluation.py` — los cinco gráficos

Módulo nuevo (no se toca `src/visualization/models.py`, cuyas firmas usa el pipeline 08). Sigue las
convenciones de la casa: `matplotlib.use("Agg", force=True)` **antes** de importar `pyplot`, las
funciones **devuelven `Figure`** y nunca guardan, tipografía en negrita con `#2c3e50` / `#34495e`,
rejilla `#eaeded`, `sns.despine`, `fig.tight_layout()`.

| # | Función | Descripción | Datos volcados a CSV |
|---|---|---|---|
| 1 | `plot_internal_validation(summary_df, model_name)` | Barras agrupadas: eje X = {ROC-AUC, PR-AUC}, dos series {Train, Validation} sobre los folds del **conjunto de entrenamiento**; altura = media, barras de error = desviación típica. **Una figura por modelo.** | `Model, Metric, Dataset, Mean, Std, N_Folds` |
| 2 | `plot_model_roc_curves(curves_roc, auc_by_model, title)` | Un único gráfico, una serie por modelo, sobre el **test externo**; diagonal de referencia; leyenda con AUC. | `False Positive Rate, True Positive Rate, Model` |
| 3 | `plot_model_pr_curves(curves_pr, ap_by_model, prevalence, title)` | Un único gráfico, una serie por modelo; línea horizontal de no-skill = prevalencia del test. | `Recall, Precision, Model` |
| 4 | `plot_metric_by_model(auc_df, metric="ROC-AUC", baseline=…)` | Barplot con una barra por modelo = ROC-AUC en test, ordenado descendente, línea discontinua del benchmark BASE-AF2. | `Model, Metric, Score` |
| 5 | `plot_metric_by_model(auc_df, metric="PR-AUC", baseline=…)` | Igual, con PR-AUC. | idem |
| + | `plot_modality_comparison(comparison_df, model_name)` | Sólo Fase 3: dos categorías {ROC-AUC, PR-AUC} × dos series {Clinical, Multimodal}. **Una figura por tipo de modelo.** | `Model, Metric, Modality, Score` |

Colores desde `config.py`: series Train/Validation con `INTERNAL_VALIDATION_COLORS`, benchmark con
`BASELINE_COLOR`, barras simples con `MODEL_BAR_COLOR`, curvas con `MODEL_PALETTE`, modalidades con
`MODALITY_COLORS`. Ejes limitados a `ylim(0, 1)` en todos los barplots de AUC.

---

## 6. Pipelines

### Estructura común de una fase de modelado

Los pipelines de modelado (09, 10, 11 y los dos brazos del 13) son el **mismo procedimiento**
parametrizado por: fichero de entrada, directorio de salida, espacio de búsqueda, `n_iter` y
`apply_filter`. Gracias a `run_modelling_phase`, cada pipeline queda en ~50 líneas:

```python
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    df = read_parquet(INPUT_FILE)
    X = df.drop(columns=[TARGET_VARIABLE])
    y = encode_target_variable(df, TARGET_VARIABLE)          # ya existe en data_preprocessing

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=TEST_SIZE, random_state=SEED, shuffle=True, stratify=y)
    cv = StratifiedKFold(n_splits=CV_N_SPLITS, shuffle=True, random_state=SEED)

    run_modelling_phase(
        X_train, X_test, y_train, y_test, cv=cv,
        search_spaces=SEARCH_SPACE, n_iter=SEARCH_N_ITER[PHASE_KEY],
        apply_filter=APPLY_FILTER, output_dir=OUTPUT_DIR, phase_title=PHASE_TITLE, logger=logger)
```

> Con `apply_filter=True`, el EN se entrena sobre **todas** las variables y los demás modelos sólo
> sobre las supervivientes, que es literalmente lo pedido para la fase intermedia.

### Mapa de fases

| Pipeline | Fase | Entrada | Salida (`results/models/…`) | Filtro | `n_iter` |
|---|---|---|---|---|---|
| `09_clinical_data_modelling.py` | 1 — clínica global (n=719, p=26) | `clean/clinical_data.parquet` | `clinical_data/` | No | 200 |
| `10_clinical_filtered_modelling.py` | 1b — clínica filtrada | `clean/clinical_data.parquet` | `clinical_data_filtered/` | **Sí** | 200 |
| `11_proteomic_data_modelling.py` | 2 — proteómica (n=488, p=361) | `clean/proteomic_data.parquet` | `proteomic_data/` | Sí | 50 |
| `12_multimodal_data_preparation.py` | — (datos) | `intermediate/clinical_data.parquet` + `raw/olink_baseline_wide.csv` | `data/clean/multimodal_data.parquet` | — | — |
| `13_multimodal_data_modelling.py` | 3 — dos brazos | `clean/multimodal_data.parquet` | `clinical_data_matched/` y `multimodal_data/` | Sí | 200 / 50 |
| `14_modality_comparison.py` | 3 — comparación | los dos `models_metrics.csv` anteriores | `modality_comparison/` | — | — |

Las Fases 1 y 1b comparten `SEED`, `TEST_SIZE` y estratificación, luego **la partición
entrenamiento/test es idéntica** y sus resultados son directamente comparables. Lo mismo vale para
los dos brazos de la Fase 3, que además parten de las mismas filas.

### 6.1 `12_multimodal_data_preparation.py` (nuevo, no toca el 05)

Reproduce exactamente la secuencia de limpieza del pipeline 05 reutilizando las mismas funciones de
`src/data/data_cleaning.py` y las mismas constantes de `config.py`, pero **conservando `code`**:

1. `read_parquet(INTERMEDIATE_CLINICAL_DATA_PATH)` (720 × 36, con `code`).
2. `mask_out_of_range_values(df, PLAUSIBLE_RANGES)`.
3. Guardar `codes = df[IDENTIFIER_VARIABLE]` (Series alineada al índice original).
4. `drop_columns(df, IDENTIFIER_VARIABLE)` → **antes** de los filtros de missingness, igual que el 05.
   Esto es crítico: `drop_high_missingness_rows` calcula la tasa por fila sobre el número de columnas,
   así que dejar `code` dentro cambiaría el denominador y podría alterar qué filas sobreviven.
5. `drop_high_missingness_columns` → `drop_high_missingness_rows` →
   `drop_columns(HIGHLY_CORRELATED_FEATURES)` → `drop_columns_by_prefix(RISK_SCORES_PREFIX)`.
   El resultado es **idéntico** a `clean/clinical_data.parquet` (719 × 27).
6. Reasociar `code` por índice.
7. `read_csv(RAW_PROTEOMIC_DATA_PATH)` → `inner_join(proteomic, df, on=IDENTIFIER_VARIABLE)` →
   `drop_columns(IDENTIFIER_VARIABLE)`.
8. `save_parquet(..., CLEANED_MULTIMODAL_DATA_PATH)` → ≈488 filas × (26 clínicas + 361 proteínas +
   `AF_recurrence`). El parquet conserva los dtypes `category` ordenados, de los que dependen
   `get_full_preprocessor`/`get_trees_preprocessor`.
9. Log de las dimensiones finales y de la prevalencia del target, para poder citarlas en la memoria.

> El brazo **clínico-emparejado** de la Fase 3 no necesita fichero propio: el pipeline 13 selecciona
> las 26 columnas clínicas de este mismo parquet, garantizando que ambos brazos usan exactamente las
> mismas filas.

### 6.2 `14_modality_comparison.py`

Lee `results/models/clinical_data_matched/models_metrics.csv` y
`results/models/multimodal_data/models_metrics.csv`, filtra `Dataset == "Test"`, y genera en
`results/models/modality_comparison/`:

- `modality_comparison.csv` — tabla consolidada `Model, Metric, Clinical, Multimodal, Delta`
  (multimodal − clínico), que es el número que responde a la pregunta de la Fase 3.
- `comparison_{abbr}.png` + `comparison_{abbr}.csv` — **una figura por tipo de modelo** (8 en total),
  con dos categorías (ROC-AUC, PR-AUC) y dos series (clínico / multimodal).

Al no reentrenar nada, cualquier retoque estético se regenera en segundos.

### 6.3 Orquestador

`run_all_pipelines.py` mantiene `PIPELINES` (01–08) y añade una lista `MODELLING_PIPELINES` (09–14)
concatenada a la principal, con un comentario indicando el coste y cómo omitirla.

---

## 7. Artefactos producidos por cada fase

En cada directorio de fase (`results/models/<fase>/`):

```
internal_validation_{en,svm,dt,rf,et,ab,gb,mlp}.png   ← gráfico 1 (8 figuras)
internal_validation_{…}.csv                            ← media, std y nº de folds
curves_roc.png / curves_roc.csv                        ← gráfico 2
curves_pr.png  / curves_pr.csv                         ← gráfico 3
auc_roc_by_model.png / auc_roc_by_model.csv            ← gráfico 4
auc_pr_by_model.png  / auc_pr_by_model.csv             ← gráfico 5
models_metrics.csv                                     ← todas las métricas (Train/Validation/Test)
best_params.csv                                        ← hiperparámetros seleccionados por modelo
optimized_{EN,SVM,DT,RF,ET,AB,GB,MLP}.joblib           ← pipelines ajustados
```

y, en las fases con filtrado (1b, 2 y los dos brazos de la 3):

```
features_kept.csv          ← variables conservadas por el Elastic Net
features_removed.csv       ← variables cuyo coeficiente se forzó a 0
feature_selection_EN.csv   ← tabla completa con coeficientes
feature_selection_EN.joblib
```

Se respeta la convención de la casa: **toda figura tiene un CSV gemelo con el mismo nombre base**.

---

## 8. Fidelidad a la metodología

| Requisito de `ml_methodology.md` / §2.4.2 | Cómo se cumple |
|---|---|
| 8 modelos (EN, SVM, DT, RF, ET, AB, GB, MLP) | `MODEL_ORDER` + `model_zoo.get_estimator` |
| Split 80/20 entrenamiento / validación externa | `TEST_SIZE = 0.2`, `stratify=y` |
| Estandarización sólo para EN y SVM | `SCALED_MODELS` (se incluye también MLP: es sensible a escala y es la práctica estándar; se documentará en la memoria) |
| Imputación MICE | `IterativeImputer` dentro del `Pipeline` ⇒ se reajusta **en cada fold**, como afirma §3.2 |
| Selección de variables por regularización del EN | `feature_selection.select_features_with_elastic_net`, con `solver="saga"` y `l1_ratio > 0` garantizados |
| Búsqueda aleatoria maximizando PR-AUC | `OBJECTIVE_METRIC = "PR-AUC"` → `scoring="average_precision"` |
| CV 5-fold interna para valorar sobreajuste | `CV_N_SPLITS = 5` en las cuatro fases; gráfico 1 |
| Validación externa con ROC-AUC y PR-AUC | `evaluate_on_test` + gráficos 2–5 |
| Sólo scikit-learn (Python 3.11) | Ningún import nuevo fuera de sklearn/pandas/numpy/matplotlib/seaborn |

**Desbalanceo de clases:** la metodología no prescribe remuestreo, y `imblearn` no está instalado. Se
gestiona como hasta ahora: objetivo PR-AUC, `class_weight` como hiperparámetro buscable en SVM/RF/ET
y particiones estratificadas. Se dejará constancia explícita en la memoria.

---

## 9. Alcance de la implementación y verificación

> **El alcance de este plan termina en el código.** Los pipelines 09–14 **no se ejecutan** durante la
> implementación: el entrenamiento se lanzará manualmente en un Codespace. Por tanto no se genera
> ningún fichero bajo `results/models/` ni `data/clean/multimodal_data.parquet`.
>
> Esto encaja además con el estado del worktree, que **no contiene `data/` ni `results/`** (están en
> `.gitignore` y viven sólo en el repositorio principal).

### 9.1 Verificación estática (lo único que se hace al implementar)

Comprobaciones que no cargan datos, no ajustan modelos y no escriben resultados:

1. **Compilación de todos los módulos y pipelines nuevos:**

```bash
conda run -n tfm_python python -m compileall -q src
```

2. **Resolución de importaciones** — detecta los nombres que hoy faltan (la causa por la que los
   pipelines 09/10 actuales están rotos) sin ejecutar `main()`:

```bash
conda run -n tfm_python python -c "import importlib; [importlib.import_module(m) for m in ['src.models.model_zoo','src.models.model_training','src.models.model_evaluation','src.models.feature_selection','src.visualization.model_evaluation']]; print('modulos OK')"
```

3. **Importación de los pipelines sin ejecutarlos.** Los pipelines sólo tienen constantes y el logger
   a nivel de módulo, así que importarlos valida firmas y rutas sin trabajo pesado. Los módulos
   empiezan por dígito, de modo que hay que usar `importlib.import_module`, no `import`:

```bash
conda run -n tfm_python python -c "import importlib; [importlib.import_module('src.pipelines.'+m) for m in ['09_clinical_data_modelling','10_clinical_filtered_modelling','11_proteomic_data_modelling','12_multimodal_data_preparation','13_multimodal_data_modelling','14_modality_comparison']]; print('pipelines OK')"
```

> Ningún pipeline hace `mkdir` a nivel de módulo (el 09 antiguo sí lo hacía): se mueve dentro de
> `main()` precisamente para que importar no toque el disco.

4. **Validación de los espacios de búsqueda contra los pipelines.** Construye los 8 pipelines con un
   `DataFrame` sintético (numéricas + categóricas ordenadas) y comprueba que **toda** clave
   `clf__…` de los tres espacios de búsqueda existe en `pipeline.get_params(deep=True)`. Es la
   comprobación que detecta erratas como `clf__estimator__ccp_alpha` en un modelo que no anida
   estimador, y no requiere ajustar nada.

5. **Comprobación de la capa de tablas** con métricas sintéticas: `summarize_internal_validation`,
   `save_metrics_results`, `save_curves_results`, `build_auc_table`,
   `build_modality_comparison_table` y `build_modality_delta_table` deben devolver exactamente las
   columnas que consumen los gráficos.

6. **Revisión manual de coherencia**, sin ejecutar: que cada pipeline lea la constante de ruta
   correcta de `paths.py`, que las claves de `SEARCH_N_ITER` coincidan con las usadas por cada fase,
   y que los nombres de paso del `Pipeline` sean `"preprocessor"` y `"clf"` (de los que dependen
   `get_relevant_features` y todos los espacios de búsqueda `clf__…`).

### 9.1.1 Resultado de la verificación estática realizada

| Comprobación | Resultado |
|---|---|
| `compileall` sobre `src` y `run_all_pipelines.py` | OK |
| Importación de los 5 módulos nuevos | OK |
| Importación de los 6 pipelines (todos exponen `main()`) | OK |
| Firmas públicas frente a los puntos de llamada | OK |
| Claves de los 3 espacios de búsqueda × 8 modelos | OK, ninguna clave inválida |
| Capa de tablas con datos sintéticos | OK |
| Capa de gráficos | **No verificable en este PC** (ver la nota de entorno) |

### 9.2 Guía de ejecución para el Codespace (a realizar por el usuario)

1. `python -m src.pipelines.12_multimodal_data_preparation` — debe registrar ≈488 filas × 388 columnas
   y una prevalencia de `AF_recurrence` en torno a 0.44.
2. **Ensayo rápido antes de la tirada larga:** bajar temporalmente `SEARCH_N_ITER` a `{…: 3}` en
   `config.py`, ejecutar la Fase 1 y comprobar que aparecen las 12 figuras con sus CSV gemelos en
   `results/models/clinical_data/`; restaurar después los valores.
3. Fases 09 → 14 en orden (varias horas; conviene `nohup`/`tmux` en el Codespace).
4. **Comprobación crítica del filtrado:** en la Fase 1b, `features_kept.csv` + `features_removed.csv`
   deben sumar las 26 variables clínicas y `features_removed.csv` **no debe estar vacío**; si lo
   estuviera, revisar en `best_params.csv` que el `l1_ratio` elegido sea > 0 y el solver `saga`.
5. **Coherencia con el borrador:** contrastar los ROC-AUC/PR-AUC de test contra el benchmark BASE-AF2
   (0.632 / 0.480), dibujado como línea de referencia en los gráficos 4 y 5.
6. Revisar el signo de la columna `Delta` en
   `results/models/modality_comparison/modality_comparison.csv`: es la evidencia directa del valor
   añadido de la proteómica sobre los datos clínicos.

> ### Nota de entorno: el BLAS de `tfm_python` está roto en este PC
>
> Durante la verificación se detectó que **cualquier operación de álgebra lineal aborta el proceso**
> en el entorno conda `tfm_python`:
>
> ```
> >>> import numpy as np; np.ones((3,3)) @ np.ones((3,3))
> Windows fatal exception: code 0xc06d007f
> ```
>
> `0xC06D007F` es el fallo de carga diferida de una DLL: numpy 2.4.6 encuentra su BLAS/LAPACK en tiempo
> de compilación pero no puede cargar la DLL en tiempo de ejecución. Los imports puros funcionan, y por
> eso toda la verificación estática pasa, pero se cae todo lo que llegue al BLAS:
>
> - `sklearn` en cualquier `.fit()`;
> - `matplotlib` en `ax.bar` / `ax.plot` con parches (`matplotlib/bezier.py` usa `np.dot`), lo que
>   implica que **los pipelines 01–08 tampoco pueden ejecutarse hoy en este equipo**;
> - por eso la capa de gráficos no se pudo probar aquí.
>
> No afecta al código entregado, pero conviene arreglarlo o dar por hecho que el trabajo se hace en el
> Codespace. Para repararlo en local, reinstalar la pila numérica del entorno:
>
> ```bash
> conda install -n tfm_python --force-reinstall -c conda-forge numpy scipy scikit-learn libblas liblapack
> ```
>
> Además, `tfm_python.yml` está **vacío (0 bytes)** desde el commit `fa56d72`, así que habrá que
> regenerarlo (`conda env export -n tfm_python > tfm_python.yml`) antes de montar el Codespace, fijando
> allí las versiones (scikit-learn 1.9, pandas 3.0, numpy 2.4) para que los resultados sean
> reproducibles.

---

## 10. Orden de implementación

1. Este documento en la raíz del proyecto.
2. `src/config.py` (sección MODELLING + espacio multimodal) y `src/utils/paths.py`.
3. Fix en `src/utils/results_saving.py` (`.copy()`).
4. `src/models/model_zoo.py` → `feature_selection.py` → `model_evaluation.py` → `model_training.py`;
   borrar `src/models/data_train_eval.py`.
5. `src/visualization/model_evaluation.py`.
6. Pipelines 09, 10, 11 (renombrando el 10 actual), 12, 13, 14.
7. `run_all_pipelines.py`.
8. Verificación **estática** según §9.1. **No se ejecuta ningún pipeline de modelado**: la tirada
   completa la lanza el usuario en un Codespace siguiendo §9.2.
