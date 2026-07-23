# %% Configuration
print("Loading libraries and setting configurations...")
import sys
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.ensemble import (
    AdaBoostClassifier,
    ExtraTreesClassifier,
    GradientBoostingClassifier,
    RandomForestClassifier,
)
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import StratifiedKFold, train_test_split
from sklearn.neural_network import MLPClassifier
from sklearn.pipeline import Pipeline
from sklearn.svm import SVC
from sklearn.tree import DecisionTreeClassifier

from src.utils.paths import CLEAN_DATA_DIR, PROJECT_ROOT, PROTEOMIC_MODELS_DIR

PROJECT_PATH = PROJECT_ROOT
if str(PROJECT_PATH) not in sys.path:
    sys.path.append(str(PROJECT_PATH))

from src.models import (
    proteomic_hyperparameters_search_space,
    get_full_preprocessor,
    get_relevant_features,
    get_trees_preprocessor,
    optimize_model_random_search,
    plot_external_validation,
    plot_internal_validation,
    plot_pr_curves,
    plot_roc_curves,
    save_curves_results,
    save_feature_selection_results,
    save_metrics_results,
    save_model,
)

data_path = CLEAN_DATA_DIR / "proteomic_data.parquet"

enable_filter = True

results_path = PROTEOMIC_MODELS_DIR
results_path.mkdir(parents=True, exist_ok=True)


# %% Main function
def main() -> None:
    seed = 7214
    n_trials = 30
    n_cv = 3
    objective_metric = "PR-AUC"
    scoring_dict = {
        "ROC-AUC": "roc_auc",
        "PR-AUC": "average_precision",
    }

    print(f"Loading data from: {data_path}")
    df = pd.read_parquet(data_path)

    for col in df.select_dtypes(include=["object"]).columns:
        df[col] = df[col].astype("category")
        print(f"Categories of column '{col}': {df[col].cat.categories.tolist()}")

    print("Splitting data into training and testing sets...")
    X = df.drop(
        [
            "code",
            "AF_recurrence",
        ],
        axis=1,
    )
    y = df["AF_recurrence"].map({"no": 0, "yes": 1})

    X_train, X_test, y_train, y_test = train_test_split(
        X,
        y,
        test_size=0.2,
        random_state=42,
        shuffle=True,
        stratify=y,
    )
    # ENCAPSULAR EN UN BUCLE UTILIZANDO UN DICCIONARIO DE MODELOS Y SUS ABREVIATURAS

    print("Starting model training and optimization...")
    my_cv = StratifiedKFold(n_splits=n_cv, shuffle=True, random_state=42)

    preprocessor_EN = get_full_preprocessor(X_train, seed=seed)
    pipe_EN = Pipeline(
        steps=[
            ("preprocessor", preprocessor_EN),
            (
                "clf",
                LogisticRegression(random_state=seed, solver="saga", max_iter=1000),
            ),
        ]
    )
    params_EN = proteomic_hyperparameters_search_space["EN"]
    print("Optimizing model: Elastic Net")
    (
        optimized_EN,
        cv_results_EN,
        fpr_EN,
        tpr_EN,
        precs_EN,
        recs_EN,
    ) = optimize_model_random_search(
        pipeline=pipe_EN,
        param_distributions=params_EN,
        X_train=X_train,
        y_train=y_train,
        X_test=X_test,
        y_test=y_test,
        metrics_dict=scoring_dict,
        aim=scoring_dict[objective_metric],
        cv=my_cv,
        n_iter=n_trials,
        seed=seed,
    )
    fig_EN, _, _ = plot_internal_validation(
        cv_results_EN,
        metrics_list=["ROC-AUC", "PR-AUC"],
        title="Elastic Net",
    )
    save_model(fitted_pipeline=optimized_EN, output_dir=results_path, identifier="EN")

    relevant_cols, irrelevant_cols = get_relevant_features(optimized_EN)
    print("relevant features: ", relevant_cols)
    print("\n")
    print("irrelevant features: ", irrelevant_cols)
    feature_selection_path = save_feature_selection_results(
        relevant_cols=relevant_cols,
        irrelevant_cols=irrelevant_cols,
        output_dir=results_path,
        identifier="EN",
    )
    print(f"Saved Elastic Net feature selection lists to: {feature_selection_path}")

    if enable_filter:
        X_train_filtered = X_train.drop(columns=irrelevant_cols)
        X_test_filtered = X_test.drop(columns=irrelevant_cols)
    else:
        X_train_filtered = X_train
        X_test_filtered = X_test

    preprocessor_SVM = get_full_preprocessor(X_train_filtered, seed=seed)
    pipe_SVM = Pipeline(
        steps=[
            ("preprocessor", preprocessor_SVM),
            ("clf", SVC(random_state=seed, max_iter=1000)),
        ]
    )
    params_dist_SVM = proteomic_hyperparameters_search_space["SVM"]
    print("Optimizing model: SVM")
    (
        optimized_SVM,
        cv_results_SVM,
        fpr_SVM,
        tpr_SVM,
        precs_SVM,
        recs_SVM,
    ) = optimize_model_random_search(
        pipeline=pipe_SVM,
        param_distributions=params_dist_SVM,
        X_train=X_train_filtered,
        y_train=y_train,
        X_test=X_test_filtered,
        y_test=y_test,
        metrics_dict=scoring_dict,
        cv=my_cv,
        n_iter=n_trials,
        seed=seed,
    )
    fig_SVM, _, _ = plot_internal_validation(
        cv_results_SVM,
        metrics_list=["ROC-AUC", "PR-AUC"],
        title="SVM",
    )
    save_model(fitted_pipeline=optimized_SVM, output_dir=results_path, identifier="SVM")

    preprocessor_DT = get_trees_preprocessor(X_train_filtered, seed=seed)
    pipe_DT = Pipeline(
        steps=[
            ("preprocessor", preprocessor_DT),
            ("clf", DecisionTreeClassifier(random_state=seed)),
        ]
    )
    params_dist_DT = proteomic_hyperparameters_search_space["DT"]
    print("Optimizing model: Decision Tree")
    (
        optimized_DT,
        cv_results_DT,
        fpr_DT,
        tpr_DT,
        precs_DT,
        recs_DT,
    ) = optimize_model_random_search(
        pipeline=pipe_DT,
        param_distributions=params_dist_DT,
        X_train=X_train_filtered,
        y_train=y_train,
        X_test=X_test_filtered,
        y_test=y_test,
        metrics_dict=scoring_dict,
        cv=my_cv,
        n_iter=n_trials,
        seed=seed,
    )
    fig_DT, _, _ = plot_internal_validation(
        cv_results_DT,
        metrics_list=["ROC-AUC", "PR-AUC"],
        title="Decision Tree",
    )
    save_model(fitted_pipeline=optimized_DT, output_dir=results_path, identifier="DT")

    preprocessor_RF = get_trees_preprocessor(X_train_filtered, seed=seed)
    pipe_RF = Pipeline(
        steps=[
            ("preprocessor", preprocessor_RF),
            ("clf", RandomForestClassifier(random_state=seed)),
        ]
    )
    params_dist_RF = proteomic_hyperparameters_search_space["RF"]
    print("Optimizing model: Random Forest")
    (
        optimized_RF,
        cv_results_RF,
        fpr_RF,
        tpr_RF,
        precs_RF,
        recs_RF,
    ) = optimize_model_random_search(
        pipeline=pipe_RF,
        param_distributions=params_dist_RF,
        X_train=X_train_filtered,
        y_train=y_train,
        X_test=X_test_filtered,
        y_test=y_test,
        metrics_dict=scoring_dict,
        cv=my_cv,
        n_iter=n_trials,
        seed=seed,
    )
    fig_RF, _, _ = plot_internal_validation(
        cv_results_RF,
        metrics_list=["ROC-AUC", "PR-AUC"],
        title="Random Forest",
    )
    save_model(fitted_pipeline=optimized_RF, output_dir=results_path, identifier="RF")

    preprocessor_ET = get_trees_preprocessor(X_train_filtered, seed=seed)
    pipe_ET = Pipeline(
        steps=[
            ("preprocessor", preprocessor_ET),
            ("clf", ExtraTreesClassifier(random_state=seed)),
        ]
    )
    params_dist_ET = proteomic_hyperparameters_search_space["ET"]
    print("Optimizing model: Extra Trees")
    (
        optimized_ET,
        cv_results_ET,
        fpr_ET,
        tpr_ET,
        precs_ET,
        recs_ET,
    ) = optimize_model_random_search(
        pipeline=pipe_ET,
        param_distributions=params_dist_ET,
        X_train=X_train_filtered,
        y_train=y_train,
        X_test=X_test_filtered,
        y_test=y_test,
        metrics_dict=scoring_dict,
        cv=my_cv,
        n_iter=n_trials,
        seed=seed,
    )
    fig_ET, _, _ = plot_internal_validation(
        cv_results_ET,
        metrics_list=["ROC-AUC", "PR-AUC"],
        title="Extra Trees",
    )
    save_model(fitted_pipeline=optimized_ET, output_dir=results_path, identifier="ET")

    preprocessor_AB = get_trees_preprocessor(X_train_filtered, seed=seed)
    pipe_AB = Pipeline(
        steps=[
            ("preprocessor", preprocessor_AB),
            (
                "clf",
                AdaBoostClassifier(
                    random_state=seed,
                    estimator=DecisionTreeClassifier(random_state=seed),
                ),
            ),
        ]
    )
    params_dist_AB = proteomic_hyperparameters_search_space["AB"]
    print("Optimizing model: AdaBoost")
    (
        optimized_AB,
        cv_results_AB,
        fpr_AB,
        tpr_AB,
        precs_AB,
        recs_AB,
    ) = optimize_model_random_search(
        pipeline=pipe_AB,
        param_distributions=params_dist_AB,
        X_train=X_train_filtered,
        y_train=y_train,
        X_test=X_test_filtered,
        y_test=y_test,
        metrics_dict=scoring_dict,
        cv=my_cv,
        n_iter=n_trials,
        seed=seed,
    )
    fig_AB, _, _ = plot_internal_validation(
        cv_results_AB,
        metrics_list=["ROC-AUC", "PR-AUC"],
        title="AdaBoost",
    )
    save_model(fitted_pipeline=optimized_AB, output_dir=results_path, identifier="AB")

    preprocessor_GB = get_trees_preprocessor(X_train_filtered, seed=seed)
    pipe_GB = Pipeline(
        steps=[
            ("preprocessor", preprocessor_GB),
            ("clf", GradientBoostingClassifier(random_state=seed)),
        ]
    )
    params_dist_GB = proteomic_hyperparameters_search_space["GB"]
    print("Optimizing model: Gradient Boosting")
    (
        optimized_GB,
        cv_results_GB,
        fpr_GB,
        tpr_GB,
        precs_GB,
        recs_GB,
    ) = optimize_model_random_search(
        pipeline=pipe_GB,
        param_distributions=params_dist_GB,
        X_train=X_train_filtered,
        y_train=y_train,
        X_test=X_test_filtered,
        y_test=y_test,
        metrics_dict=scoring_dict,
        cv=my_cv,
        n_iter=n_trials,
        seed=seed,
    )
    fig_GB, _, _ = plot_internal_validation(
        cv_results_GB,
        metrics_list=["ROC-AUC", "PR-AUC"],
        title="Gradient Boosting",
    )
    save_model(fitted_pipeline=optimized_GB, output_dir=results_path, identifier="GB")

    preprocessor_MLP = get_full_preprocessor(X_train_filtered, seed=seed)
    pipe_MLP = Pipeline(
        steps=[
            ("preprocessor", preprocessor_MLP),
            (
                "clf",
                MLPClassifier(
                    random_state=seed,
                    max_iter=1000,
                    early_stopping=True,
                    validation_fraction=0.1,
                ),
            ),
        ]
    )
    params_dist_MLP = proteomic_hyperparameters_search_space["MLP"]
    print("Optimizing model: MLP")
    (
        optimized_MLP,
        cv_results_MLP,
        fpr_MLP,
        tpr_MLP,
        precs_MLP,
        recs_MLP,
    ) = optimize_model_random_search(
        pipeline=pipe_MLP,
        param_distributions=params_dist_MLP,
        X_train=X_train_filtered,
        y_train=y_train,
        X_test=X_test_filtered,
        y_test=y_test,
        metrics_dict=scoring_dict,
        cv=my_cv,
        n_iter=n_trials,
        seed=seed,
    )
    fig_MLP, _, _ = plot_internal_validation(
        cv_results_MLP,
        metrics_list=["ROC-AUC", "PR-AUC"],
        title="Multi-Layer Perceptron",
    )
    save_model(fitted_pipeline=optimized_MLP, output_dir=results_path, identifier="MLP")

    # %% 4. Save metrics results and external validation plots
    models = [
        "Elastic Net",
        "SVM",
        "Decision Tree",
        "Random Forest",
        "Extra Trees",
        "AdaBoost",
        "Gradient Boost",
        "MLP",
    ]
    models_dict = {
        "Elastic Net": cv_results_EN,
        "SVM": cv_results_SVM,
        "Decision Tree": cv_results_DT,
        "Random Forest": cv_results_RF,
        "Extra Trees": cv_results_ET,
        "AdaBoost": cv_results_AB,
        "Gradient Boost": cv_results_GB,
        "MLP": cv_results_MLP,
    }

    print("Saving results...")
    results_df = save_metrics_results(models_dict=models_dict, output_dir=results_path)

    roc_auc_baseline = 0.633
    pr_auc_baseline = 0.482

    fig_roc_auc, _, _ = plot_external_validation(
        results_df,
        baseline=roc_auc_baseline,
        metric="ROC-AUC",
        title="ROC-AUC by model",
    )
    fig_pr_auc, _, _ = plot_external_validation(
        results_df,
        baseline=pr_auc_baseline,
        metric="PR-AUC",
        title="PR-AUC by model",
    )

    fpr = [fpr_EN, fpr_SVM, fpr_DT, fpr_RF, fpr_ET, fpr_AB, fpr_GB, fpr_MLP]
    tpr = [tpr_EN, tpr_SVM, tpr_DT, tpr_RF, tpr_ET, tpr_AB, tpr_GB, tpr_MLP]
    roc_results = save_curves_results(
        models, fpr, tpr, curve_type="roc", output_dir=results_path
    )

    precs = [
        precs_EN,
        precs_SVM,
        precs_DT,
        precs_RF,
        precs_ET,
        precs_AB,
        precs_GB,
        precs_MLP,
    ]
    recs = [recs_EN, recs_SVM, recs_DT, recs_RF, recs_ET, recs_AB, recs_GB, recs_MLP]
    pr_results = save_curves_results(
        models, recs, precs, curve_type="pr", output_dir=results_path
    )

    # %% 5. Save plots
    print("Saving plots...")
    fig_EN.savefig(
        results_path / "internal_validation_en.png", dpi=300, bbox_inches="tight"
    )
    plt.close(fig_EN)

    fig_SVM.savefig(
        results_path / "internal_validation_svm.png", dpi=300, bbox_inches="tight"
    )
    plt.close(fig_SVM)

    fig_DT.savefig(
        results_path / "internal_validation_dt.png", dpi=300, bbox_inches="tight"
    )
    plt.close(fig_DT)

    fig_RF.savefig(
        results_path / "internal_validation_rf.png", dpi=300, bbox_inches="tight"
    )
    plt.close(fig_RF)

    fig_ET.savefig(
        results_path / "internal_validation_et.png", dpi=300, bbox_inches="tight"
    )
    plt.close(fig_ET)

    fig_AB.savefig(
        results_path / "internal_validation_ab.png", dpi=300, bbox_inches="tight"
    )
    plt.close(fig_AB)

    fig_GB.savefig(
        results_path / "internal_validation_gb.png", dpi=300, bbox_inches="tight"
    )
    plt.close(fig_GB)

    fig_MLP.savefig(
        results_path / "internal_validation_mlp.png", dpi=300, bbox_inches="tight"
    )
    plt.close(fig_MLP)

    fig_roc_auc.savefig(
        results_path / "auc_roc_by_model.png", dpi=300, bbox_inches="tight"
    )
    plt.close(fig_roc_auc)

    fig_pr_auc.savefig(
        results_path / "auc_pr_by_model.png", dpi=300, bbox_inches="tight"
    )
    plt.close(fig_pr_auc)

    fig_roc, _ = plot_roc_curves(
        roc_results,
        title="ROC curves",
    )
    fig_roc.savefig(results_path / "curves_roc.png", dpi=300, bbox_inches="tight")
    plt.close(fig_roc)

    fig_pr, _ = plot_pr_curves(
        pr_results,
        baseline=pr_auc_baseline,
        title="Precision-Recall curves",
    )
    fig_pr.savefig(results_path / "curves_pr.png", dpi=300, bbox_inches="tight")
    plt.close(fig_pr)


if __name__ == "__main__":
    main()
