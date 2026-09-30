# Precomputed results for the `similarity` vignette

Precomputed results for the `similarity` vignette

## Usage

``` r
similarity_results
```

## Format

A list of the encrypted walk-through's outputs: the printed public
parameters (`pub_print`), the smoke-test errors (`rot_err`,
`matvec_err`, `ip_err`, `fold_err`, `slots_same`, `slots_dev`), the
site-1 and full-query timings (`site1_n`, `site1_elapsed`,
`query_elapsed`), the encrypted top-k table (`top_result`), and its
agreement with the cleartext reference (`score_err`, `set_match`).

## Source

`data-raw/similarity_results.R`, from `vignettes/similarity.Rmd`.
