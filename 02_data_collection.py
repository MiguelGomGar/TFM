# %% Setup
print("Setting up paths and loading modules...")

# Set paths
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent
INPUT_FILE = PROJECT_ROOT / "data" / "raw" / "predimar_miguelgomez.dta"
OUTPUT_FILE = PROJECT_ROOT / "data" / "intermediate" / "clinical_data.parquet"

# Load modules
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq


# %% Main
def main():
    print(f"Loading raw data from {INPUT_FILE}...")
    original_clinical_data = pd.read_stata(INPUT_FILE)

    print("Extracting relevant features...")
    clinical_data_selected = original_clinical_data[
        [
            # SOCIODEMOGRAPHIC DATA
            "sexo",  # sex
            "edad",  # age
            "fum_0m",  # smoking status
            # CLINICAL DATA
            "code",  # patient code
            "interv",  # intervention
            "bmi_autoref_0m",  # body mass index
            "glu_new0m",  # glucose
            "diabetest2_0m",  # type 2 diabetes
            "hdl_new0m",  # HDL cholesterol
            "ldl_new0m",  # LDL cholesterol
            "trig_new0m",  # triglycerides
            "hipolipemiantes_0m",  # lipid-lowering agents
            "saos_0m",  # obstructive sleep apnea
            "insurenal_0m",  # renal disease
            "fglopre_MDRD_cal_0m",  # estimated glomerular filtration rate (MDRD formula)
            "fglopre_CockroftGault_cal_0m",  # estimated glomerular filtration rate (Cockroft-Gault formula)
            "hta_0m",  # hypertension
            "epoc_0m",  # chronic obstructive pulmonary disease
            "ictus_0m",  # stroke
            "bri_0m",  # bundle branch block (left)
            "brd_0m",  # bundle branch block (right)
            "ic_v00",  # heart failure
            "mio_isq_0m",  # coronary artery disease
            "miocardiopat_0m_imp",  # myocardiopathy
            "faa_0m_new",  # antiarrhythmic drugs
            "dilat_tot4_imp",  # atrial enlargement
            "eco_diamai_0m",  # atrial diameter
            "eco_areaai_0m",  # atrial area
            "eco_volumenai_0m",  # atrial volume
            "eco_fe_0m",  # ejection fraction
            "tipofa",  # atrial fibrillation type
            "tiempo_fa_ablac",  # time between atrial fibrillation diagnosis and ablation
            "ablacion_previa",  # previous ablation
            "event_blank",  # early recurrence of atrial fibrillation
            "event18m_conkardia",  # atrial fibrillation between 3 and 18 months since ablation
            "chad2ds2vasc_v00",  # chad2ds2vasc score
        ]
    ]

    print("Renaming features...")
    new_names = {
        "sexo": "sex",
        "edad": "age",
        "fum_0m": "smoking_status",
        "code": "code",
        "interv": "intervention",
        "bmi_autoref_0m": "BMI",
        "glu_new0m": "glucose",
        "diabetest2_0m": "diabetes",
        "hdl_new0m": "HDL",
        "ldl_new0m": "LDL",
        "trig_new0m": "triglycerides",
        "hipolipemiantes_0m": "LLA",
        "saos_0m": "OSA",
        "insurenal_0m": "renal_dysfunction",
        "fglopre_MDRD_cal_0m": "eGFR_MDRD",
        "fglopre_CockroftGault_cal_0m": "eGFR_CG",
        "hta_0m": "hypertension",
        "epoc_0m": "COPD",
        "ictus_0m": "stroke",
        "bri_0m": "LBBB",
        "brd_0m": "RBBB",
        "ic_v00": "heart_failure",
        "mio_isq_0m": "CAD",
        "miocardiopat_0m_imp": "cardiomyopathy",
        "faa_0m_new": "AAD",
        "dilat_tot4_imp": "LA_enlargment",
        "eco_diamai_0m": "LAD",
        "eco_areaai_0m": "LAA",
        "eco_volumenai_0m": "LAV",
        "eco_fe_0m": "LVEF",
        "tipofa": "AF_type",
        "tiempo_fa_ablac": "AF_duration",
        "ablacion_previa": "previous_ablation",
        "event_blank": "ERAF",
        "event18m_conkardia": "AF_recurrence",
        "chad2ds2vasc_v00": "score_chad2ds2_vasc",
    }
    clinical_data_renamed = clinical_data_selected.rename(columns=new_names)

    print("Renaming categories...")
    clinical_data_renamed["sex"] = (
        clinical_data_renamed["sex"]
        .map({"hombre": "male", "mujer": "female"})
        .astype("category")
        .cat.reorder_categories(["male", "female"], ordered=True)
    )
    clinical_data_renamed["smoking_status"] = (
        clinical_data_renamed["smoking_status"]
        .map(
            {
                "Fumador activo": "current",
                "Exfumador": "former",
                "No fumador": "never",
            }
        )
        .astype("category")
        .cat.reorder_categories(["never", "former", "current"], ordered=True)
    )
    clinical_data_renamed["intervention"] = (
        clinical_data_renamed["intervention"]
        .map({"Intervencion": "intervention", "Control": "control"})
        .astype("category")
        .cat.reorder_categories(["control", "intervention"], ordered=True)
    )
    clinical_data_renamed["diabetes"] = (
        clinical_data_renamed["diabetes"]
        .map({"no": "no", "si": "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["LLA"] = (
        clinical_data_renamed["LLA"]
        .map({"no": "no", "si": "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["OSA"] = (
        clinical_data_renamed["OSA"]
        .map({"no": "no", "si": "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["renal_dysfunction"] = (
        clinical_data_renamed["renal_dysfunction"]
        .map({"no": "no", "si": "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["hypertension"] = (
        clinical_data_renamed["hypertension"]
        .map({"no": "no", "si": "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["COPD"] = (
        clinical_data_renamed["COPD"]
        .map({"no": "no", "si": "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["stroke"] = (
        clinical_data_renamed["stroke"]
        .map({"no": "no", "si": "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["LBBB"] = (
        clinical_data_renamed["LBBB"]
        .map({"no": "no", "si": "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["RBBB"] = (
        clinical_data_renamed["RBBB"]
        .map({"no": "no", "si": "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["heart_failure"] = (
        clinical_data_renamed["heart_failure"]
        .map({0: "no", 1: "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["CAD"] = (
        clinical_data_renamed["CAD"]
        .map({"no": "no", "si": "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["cardiomyopathy"] = (
        clinical_data_renamed["cardiomyopathy"]
        .map({0: "no", 1: "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["AAD"] = (
        clinical_data_renamed["AAD"]
        .map({0: "no", 1: "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["LA_enlargment"] = (
        clinical_data_renamed["LA_enlargment"]
        .map({0: "normal", 1: "mild", 2: "moderate", 3: "severe"})
        .astype("category")
        .cat.reorder_categories(["normal", "mild", "moderate", "severe"], ordered=True)
    )
    clinical_data_renamed["LVEF"] = (
        clinical_data_renamed["LVEF"]
        .map(
            {
                "normal": "normal",
                "ligeramente anormal": "mild",
                "moderadamente anormal": "moderate",
                "severamente anormal": "severe",
            }
        )
        .astype("category")
        .cat.reorder_categories(["normal", "mild", "moderate", "severe"], ordered=True)
    )
    clinical_data_renamed["AF_type"] = (
        clinical_data_renamed["AF_type"]
        .map({"Paroxistica documentada": "paroxysmal", "Persistente": "persistent"})
        .astype("category")
        .cat.reorder_categories(["paroxysmal", "persistent"], ordered=True)
    )
    clinical_data_renamed["previous_ablation"] = (
        clinical_data_renamed["previous_ablation"]
        .map({0: "no", 1: "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["ERAF"] = (
        clinical_data_renamed["ERAF"]
        .map({0: "no", 1: "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )
    clinical_data_renamed["AF_recurrence"] = (
        clinical_data_renamed["AF_recurrence"]
        .map({0: "no", 1: "yes"})
        .astype("category")
        .cat.reorder_categories(["no", "yes"], ordered=True)
    )

    print("Converting all numeric columns to float64...")
    numeric_columns = clinical_data_renamed.select_dtypes(include=["number"]).columns
    clinical_data_renamed[numeric_columns] = clinical_data_renamed[
        numeric_columns
    ].astype("float64")

    print(f"Saving data to {OUTPUT_FILE}...")
    table = pa.Table.from_pandas(clinical_data_renamed)
    pq.write_table(table, OUTPUT_FILE)


if __name__ == "__main__":
    main()
