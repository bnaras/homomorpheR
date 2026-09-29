## R-SPECIFIC: build data/cox_results.rda for vignettes/cox.Rmd.
##
## The vignette is the single source of truth: its encrypted chunks are
## gated `eval = RECOMPUTE`. This script purls them with RECOMPUTE = TRUE
## (so they tangle as runnable code), sources the result to run the
## encrypted fit once, and saves what the vignette displays.
##
## Run from the package root:  Rscript data-raw/cox_results.R

RECOMPUTE <- TRUE   # un-gate the eval=RECOMPUTE chunks when purling

script <- knitr::purl("vignettes/cox.Rmd",
                      output = tempfile(fileext = ".R"), quiet = TRUE)
source(script)

stopifnot(is.list(cox_results),
          nrow(cox_results$coef) == 5L,
          is.finite(cox_results$loglik))

save(cox_results, file = "data/cox_results.rda", compress = "xz")
