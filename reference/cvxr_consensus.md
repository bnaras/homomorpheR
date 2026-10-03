# Precomputed encrypted Cox-lasso consensus-ADMM results

Result objects from the encrypted stratified Cox-lasso consensus-ADMM
demonstration on the
[DLBCL](https://bnaras.github.io/homomorpheR/reference/DLBCL.md) /
[DLBCL_gex](https://bnaras.github.io/homomorpheR/reference/DLBCL_gex.md)
cohort: a centralized
[CVXR](https://www.cvxgrp.org/CVXR/reference/CVXR-package.html)
ground-truth fit, the same fit recovered by consensus ADMM in the clear,
and the encrypted threshold-FHE fit, whose standardization, screening,
and consensus rounds all run under encryption. The iterated ADMM runs
are expensive, so they are computed once and shipped here; the
manuscript and the `cvxr-cox-lasso-dlbcl` vignette load this object
instead of recomputing (see Details).

## Usage

``` r
cvxr_consensus
```

## Format

A named list with components

- params:

  list of the run constants: `K` (screened probes, 100), `LAMBDA` (L1
  penalty, 5), `RHO` (ADMM penalty, 50), `MAX_ITER` (200), `TOL` (5e-3).

- top_idx:

  integer vector of length `K`; column indices into `DLBCL_gex` of the
  top-`K` univariate-screened probes.

- sigma_K:

  numeric vector of length `K`; pooled standard deviations of the
  screened probes, for the back-transform to the original scale.

- agg_beta:

  numeric vector of length `K`; centralized CVXR Cox-lasso coefficients
  (the ground truth), on the standardized scale.

- z_ref:

  numeric vector of length `K`; consensus-ADMM coefficients computed in
  the clear (cleartext reference).

- z_enc:

  numeric vector of length `K`; consensus-ADMM coefficients under
  threshold FHE.

- trajectory:

  list of numeric vectors of length `K`; the encrypted consensus iterate
  \\z^t\\ at each ADMM iteration.

- n_iter_ref, n_iter_enc:

  iterations to convergence for the plaintext and encrypted runs.

- pool_agree:

  list `mu`, `sigma`: max absolute disagreement between the encrypted
  and plaintext pooled standardization moments.

- screen_match:

  logical; whether the encrypted screen selected the same probes, in the
  same order, as the plaintext screen.

## Details

The `cvxr-cox-lasso-dlbcl` vignette is the single source of truth.
`data-raw/cvxr_consensus.R` extracts its code chunks with
[`knitr::purl()`](https://rdrr.io/pkg/knitr/man/knit.html) into
`inst/scripts/cvxr-consensus.R`, runs that script, and saves the result.
The openfhe-jss manuscript reads the labeled chunks of the generated
script with
[`knitr::read_chunk()`](https://rdrr.io/pkg/knitr/man/read_chunk.html),
so the code displayed there is exactly the code that produced these
results. Find the installed copy with
`system.file("scripts", "cvxr-consensus.R", package = "homomorpheR")`.

## See also

[DLBCL](https://bnaras.github.io/homomorpheR/reference/DLBCL.md),
[DLBCL_gex](https://bnaras.github.io/homomorpheR/reference/DLBCL_gex.md)
