## R-SPECIFIC: build data/similarity_results.rda for
## vignettes/similarity.Rmd.
##
## The vignette is the single source of truth: its encrypted chunks are
## gated `eval = RECOMPUTE`. This script purls them with RECOMPUTE = TRUE
## (so they tangle as runnable code), sources the result to run the
## encrypted walk-through and top-k retrieval once, and saves what the
## vignette displays. (The cleartext recall study runs live in the
## vignette and is not stored.)
##
## Run from the package root:  Rscript data-raw/similarity_results.R

RECOMPUTE <- TRUE   # un-gate the eval=RECOMPUTE chunks when purling

script <- knitr::purl("vignettes/similarity.Rmd",
                      output = tempfile(fileext = ".R"), quiet = TRUE)
source(script)

stopifnot(is.list(similarity_results),
          similarity_results$rot_err < 1e-6,
          similarity_results$score_err < 1e-6,
          similarity_results$set_match == nrow(similarity_results$top_result))

save(similarity_results, file = "data/similarity_results.rda",
     compress = "xz")
