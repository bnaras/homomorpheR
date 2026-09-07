# DLBCL Lymphochip gene-expression matrix

Gene-expression profiles for the 235-patient DLBCL cohort of Rosenwald
et al. (2002), used as the high-dimensional benchmark in the encrypted
distributed Cox-lasso demonstration.

## Usage

``` r
DLBCL_gex
```

## Format

A numeric matrix with 235 rows (patients) and 6416 columns (Lymphochip
microarray features). Row names are the patient LYM identifiers,
matching `DLBCL$ID`; column names are the microarray UNIQIDs. Values are
log-ratios on the original scale.

## Source

Rosenwald A, Wright G, Chan WC, et al. (2002). The use of molecular
profiling to predict survival after chemotherapy for diffuse
large-B-cell lymphoma. *New England Journal of Medicine*
346(25):1937–1947. Data: <https://llmpp.ccr.cancer.gov/DLBCL/>.

## Details

Derived from the public Lymphoma/Leukemia Molecular Profiling Project
release (<https://llmpp.ccr.cancer.gov/DLBCL/>; files
`DLBCL_patient_data_NEW.txt` and `NEJM_Web_Fig1data`). Patients are
matched to expression columns by LYM number. Of the 7399 Lymphochip
features, the 6416 measured across the cohort are retained, and the
sporadic remaining missing values are imputed by the per-feature median
(cf. Bayle, Fan and Lou, 2025); the five patients with zero follow-up
time are excluded, leaving 235 patients. Standardization is deliberately
*not* baked into the stored matrix — it is performed inside the
encrypted pipeline so that the demonstration exercises encrypted
standardization. The full processing script is `data-raw/DLBCL.R`.

## See also

[DLBCL](https://bnaras.github.io/homomorpheR/reference/DLBCL.md)
