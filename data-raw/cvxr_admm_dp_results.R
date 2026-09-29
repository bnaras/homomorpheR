## R-SPECIFIC: build data/cvxr_admm_dp_results.rda for
## vignettes/cvxr-consensus-admm-dp.Rmd.
##
## The vignette is the single source of truth: its expensive chunks are
## gated `eval = RECOMPUTE`. This script purls them with RECOMPUTE = TRUE
## (so they tangle as runnable code), sources the result to run the
## surrogate rho sweep and the DP-ADMM noise sweep once, and saves what
## the vignette displays.
##
## Run from the package root:  Rscript data-raw/cvxr_admm_dp_results.R

RECOMPUTE <- TRUE   # un-gate the eval=RECOMPUTE chunks when purling

script <- knitr::purl("vignettes/cvxr-consensus-admm-dp.Rmd",
                      output = tempfile(fileext = ".R"), quiet = TRUE)
source(script)

stopifnot(is.list(cvxr_admm_dp_results),
          cvxr_admm_dp_results$clean_dev <= 10 * cvxr_admm_dp_results$tol,
          nrow(cvxr_admm_dp_results$summary_table) ==
              length(cvxr_admm_dp_results$sigma_grid) + 1L)

save(cvxr_admm_dp_results, file = "data/cvxr_admm_dp_results.rda",
     compress = "xz")
