## R-SPECIFIC: build data/cox_threshold_results.rda for
## vignettes/cox-threshold.Rmd.
##
## The vignette is the single source of truth: its encrypted chunks are
## gated `eval = RECOMPUTE`. This script purls them with RECOMPUTE = TRUE
## (so they tangle as runnable code), sources the result to run the
## threshold-encrypted fit once, and saves what the vignette displays.
##
## Run from the package root:  Rscript data-raw/cox_threshold_results.R

RECOMPUTE <- TRUE   # un-gate the eval=RECOMPUTE chunks when purling

script <- knitr::purl("vignettes/cox-threshold.Rmd",
                      output = tempfile(fileext = ".R"), quiet = TRUE)
source(script)

stopifnot(is.list(cox_threshold_results),
          nrow(cox_threshold_results$coef) == 5L,
          is.finite(cox_threshold_results$loglik),
          !cox_threshold_results$share_check[["master_holds_shares"]],
          cox_threshold_results$share_check[["gcb_holds_own_share"]])

save(cox_threshold_results, file = "data/cox_threshold_results.rda",
     compress = "xz")
