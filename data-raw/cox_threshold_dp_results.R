## R-SPECIFIC: build data/cox_threshold_dp_results.rda for
## vignettes/cox-threshold-dp.Rmd.
##
## The vignette is the single source of truth: its encrypted chunks are
## gated `eval = RECOMPUTE`. This script purls them with RECOMPUTE = TRUE
## (so they tangle as runnable code), sources the result to run the
## threshold-DP fits once, and saves the tables the vignette displays.
##
## Run from the package root:  Rscript data-raw/cox_threshold_dp_results.R

RECOMPUTE <- TRUE   # un-gate the eval=RECOMPUTE chunks when purling

script <- knitr::purl("vignettes/cox-threshold-dp.Rmd",
                      output = tempfile(fileext = ".R"), quiet = TRUE)
source(script)

stopifnot(is.list(cox_threshold_dp_results),
          nrow(cox_threshold_dp_results$clean_check) == 5L,
          nrow(cox_threshold_dp_results$bfgs_table)  == 5L,
          nrow(cox_threshold_dp_results$budget)      == 6L)

save(cox_threshold_dp_results, file = "data/cox_threshold_dp_results.rda",
     compress = "xz")
