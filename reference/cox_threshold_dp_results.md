# Precomputed results for the `cox-threshold-dp` vignette

Precomputed results for the `cox-threshold-dp` vignette

## Usage

``` r
cox_threshold_dp_results
```

## Format

A list of the vignette's tables: `clean_check` (the fit at zero noise
against `coxph()`), `bfgs_table` and `nm_table` (BFGS and Nelder-Mead
fits over the noise grid), and `budget` (the zCDP privacy budget of the
fits at the first three noise scales).

## Source

`data-raw/cox_threshold_dp_results.R`, from
`vignettes/cox-threshold-dp.Rmd`.
