## R-SPECIFIC: build data/cvxr_admm_results.rda for
## vignettes/cvxr-consensus-admm.Rmd.
##
## The vignette is the single source of truth: its encrypted chunks are
## gated `eval = RECOMPUTE`. This script purls them with RECOMPUTE = TRUE
## (so they tangle as runnable code), sources the result to run the
## encrypted rho sweep and ADMM once, and saves what the vignette
## displays.
##
## Run from the package root:  Rscript data-raw/cvxr_admm_results.R

RECOMPUTE <- TRUE   # un-gate the eval=RECOMPUTE chunks when purling

script <- knitr::purl("vignettes/cvxr-consensus-admm.Rmd",
                      output = tempfile(fileext = ".R"), quiet = TRUE)
source(script)

stopifnot(is.list(cvxr_admm_results),
          length(cvxr_admm_results$z_final) == 4L,
          !cvxr_admm_results$share_check[["aggregator_has_share"]],
          cvxr_admm_results$share_check[["every_site_has_one"]])

save(cvxr_admm_results, file = "data/cvxr_admm_results.rda",
     compress = "xz")
