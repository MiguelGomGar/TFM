* =====================================================================
* STAGE 1: UNIFY NUMERIC & LABELED CATEGORICAL VARIABLES
* =====================================================================
* Identify all columns that are NOT stored as raw text strings
ds, not(type string)

* Loop and convert extended missings (.a, .b, etc.) to the standard dot (.)
foreach var of varlist `r(varlist)' {
    quietly replace `var' = . if `var' > .
}

* =====================================================================
* STAGE 2: UNIFY PURE STRING VARIABLES
* =====================================================================
* FIX: Wrap the specification inside 'has()' to comply with Stata syntax
ds, has(type string)

* Loop through text columns and clear literal dot-coded missings
foreach var of varlist `r(varlist)' {
    quietly replace `var' = "" if inlist(`var', ".", ".a", ".b")
}
