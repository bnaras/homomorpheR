# Precomputed results for the `cox-threshold` vignette

Precomputed results for the `cox-threshold` vignette

## Usage

``` r
cox_threshold_results
```

## Format

A list with `coef`, `loglik`, and `counts` for the threshold-encrypted
[`stats4::mle()`](https://rdrr.io/r/stats4/mle.html) fit, as in
[cox_results](https://bnaras.github.io/homomorpheR/reference/cox_results.md),
and `share_check`, a logical vector recording that the master holds no
key share and that a site holds its own.

## Source

`data-raw/cox_threshold_results.R`, from `vignettes/cox-threshold.Rmd`.
