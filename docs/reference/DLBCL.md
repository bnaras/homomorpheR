# Diffuse Large B-cell Lymphoma Cohort (Rosenwald et al. 2002)

Patient-level survival data and gene-expression signature scores from
the diffuse large-B-cell lymphoma (DLBCL) cohort of Rosenwald et al.
(2002). Used in the Cox-regression vignettes to demonstrate distributed
Cox estimation under threshold FHE with sites partitioned by molecular
subgroup.

## Usage

``` r
data(DLBCL)
```

## Format

A data frame with 235 observations on the following 12 variables:

- `ID`:

  LYM patient identifier (integer).

- `Set`:

  Original analysis set assignment, either `"Training"` or
  `"Validation"`.

- `Subgroup`:

  Molecular subgroup, a factor with levels `"GCB"` (germinal-center
  B-cell-like), `"ABC"` (activated B-cell-like), and `"Type III"`
  (unclassified).

- `IPI`:

  International Prognostic Index group (`"Low"`, `"Medium"`, `"High"`,
  or `NA`).

- `time`:

  Follow-up time in years.

- `status`:

  Vital status at last follow-up coded as `1` for death and `0` for
  alive at follow-up.

- `GCB_sig`:

  Germinal-center B-cell signature score.

- `LN_sig`:

  Lymph-node signature score.

- `Prolif_sig`:

  Proliferation signature score.

- `BMP6`:

  BMP6 expression score.

- `MHC2_sig`:

  MHC class II signature score.

- `Score`:

  Outcome predictor score combining the five signatures, as published.

## Details

Each row represents one patient. The five signature columns (`GCB_sig`,
`LN_sig`, `Prolif_sig`, `BMP6`, `MHC2_sig`) are standardized
expression-signature scores carried over from the published supplement.
Following Bayle, Fan and Lou (2025), the five patients with zero
follow-up time are excluded, so the cohort spans 235 patients with 133
deaths (event rate 56.6%) over a median follow-up of 2.8 years. The
molecular-subgroup partition gives three sites of unequal size: GCB
(n=115, 54 deaths), ABC (n=71, 49 deaths), and Type III (n=49, 30
deaths).

Splitting by `Subgroup` for distributed Cox estimation is biologically
meaningful: GCB, ABC, and Type III tumors arise from different cells of
origin and are routinely diagnosed at different referral centers.
Stratified Cox regression with `strata(Subgroup)` factors the partial
log-likelihood additively across the three subgroups, which is exactly
the decomposition the master/worker protocol exploits.

## Source

Web supplement to Rosenwald et al. (2002), available from the New
England Journal of Medicine.

## References

Rosenwald, A., Wright, G., Chan, W. C., et al. (2002). The use of
molecular profiling to predict survival after chemotherapy for diffuse
large-B-cell lymphoma. *New England Journal of Medicine* **346**(25),
1937–1947.
[doi:10.1056/NEJMoa012914](https://doi.org/10.1056/NEJMoa012914)

Bayle, P., Fan, J., and Lou, Z. (2025). Communication-Efficient
Distributed Estimation and Inference for Cox's Model. *Journal of the
American Statistical Association*.
[doi:10.1080/01621459.2025.2516820](https://doi.org/10.1080/01621459.2025.2516820)

## See also

[`DLBCL_gex`](https://bnaras.github.io/homomorpheR/reference/DLBCL_gex.md)
for the full Lymphochip gene-expression matrix (235 x 6416) on the same
cohort.

## Examples

``` r
data(DLBCL)
table(DLBCL$Subgroup, DLBCL$status)
#>           
#>             0  1
#>   GCB      61 54
#>   ABC      22 49
#>   Type III 19 30

## Stratified Cox fit on the five signatures
if (requireNamespace("survival", quietly = TRUE)) {
  fit <- survival::coxph(
    survival::Surv(time, status) ~ GCB_sig + LN_sig + Prolif_sig +
      BMP6 + MHC2_sig + survival::strata(Subgroup),
    data = DLBCL)
  print(fit)
}
#> Call:
#> survival::coxph(formula = survival::Surv(time, status) ~ GCB_sig + 
#>     LN_sig + Prolif_sig + BMP6 + MHC2_sig + survival::strata(Subgroup), 
#>     data = DLBCL)
#> 
#>                coef exp(coef) se(coef)      z        p
#> GCB_sig    -0.26387   0.76807  0.11940 -2.210 0.027112
#> LN_sig     -0.25436   0.77541  0.08515 -2.987 0.002816
#> Prolif_sig  0.30313   1.35408  0.14981  2.023 0.043036
#> BMP6        0.30364   1.35478  0.10728  2.830 0.004649
#> MHC2_sig   -0.31915   0.72677  0.09413 -3.391 0.000698
#> 
#> Likelihood ratio test=42.74  on 5 df, p=4.174e-08
#> n= 235, number of events= 133 
```
