# Precomputed results for the `cvxr-consensus-admm-dp` vignette

Precomputed results for the `cvxr-consensus-admm-dp` vignette

## Usage

``` r
cvxr_admm_dp_results
```

## Format

A list with `tol`, `rho_sweep` (convergence on the surrogate cohort),
`rho_chosen` and `T_fixed` (the pre-committed constants), `sigma_grid`
(the noise scales), `clean_dev` (deviation from the centralized fit at
zero noise), and `summary_table` (coefficients at each noise scale).

## Source

`data-raw/cvxr_admm_dp_results.R`, from
`vignettes/cvxr-consensus-admm-dp.Rmd`.
