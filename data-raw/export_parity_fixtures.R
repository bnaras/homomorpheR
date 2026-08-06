## Export cross-language parity fixtures for the homomorphepy port.
##
## WHY THIS EXISTS
##
## R's rpois/rbinom/sample/rnorm algorithms have no numpy equivalent:
## no seeding makes Python reproduce R's stream. Eight of the twelve
## vignettes simulate their inputs from set.seed(), so a Python twin
## that re-simulates computes on DIFFERENT DATA and there is nothing
## meaningful to compare. This script exports each example's inputs
## once, from R, so both languages consume identical bytes.
##
## Run from the package root:
##   Rscript data-raw/export_parity_fixtures.R [outdir]
## Default outdir: ../../fixtures/parity relative to the package
## (i.e. the monorepo's fixtures/parity), overridable for testing.
##
## CONTRACTS THIS SCRIPT ENFORCES
##
## 1. Categorical codings are FORCED, never inferred. Every factor
##    ships its level order in the manifest, because site order is
##    protocol semantics: sites are visited in level order and the
##    FIRST site is the lead decryptor in multiparty_decrypt_lead().
##    A Python groupby that sorts alphabetically would silently
##    permute the protocol.
## 2. Float matrices ship as raw little-endian float64 (.f64) plus a
##    sha256 of those exact bytes -- NOT as CSV. The Cox-lasso
##    univariate screen ranks 6416 probes and keeps the top 100;
##    near-ties at the boundary flip under a 1-ulp perturbation, and
##    write.csv's 15-significant-digit formatting loses bits.
## 3. Integer dtypes and string/int alignment keys are declared in the
##    manifest so the Python loader asserts rather than infers.
## 4. Every emitted file is listed in manifest.json with its sha256,
##    so a stale copy fails loudly instead of drifting quietly.
##
## The manifest is the API. Keep it stable; add fields, do not rename.

suppressPackageStartupMessages({
    library(homomorpheR)
    library(jsonlite)
})

args   <- commandArgs(trailingOnly = TRUE)
OUTDIR <- if (length(args) >= 1) args[[1]] else
    normalizePath(file.path("..", "..", "fixtures", "parity"), mustWork = FALSE)

dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)
cat("exporting to:", OUTDIR, "\n\n")

## ---- helpers --------------------------------------------------------------

.entries <- list()   # accumulates manifest entries

sha256 <- function(path) {
    ## digest is not a package dependency; shell out to the system tool
    ## available on macOS and Linux.
    out <- suppressWarnings(system2("shasum", c("-a", "256", shQuote(path)),
                                    stdout = TRUE, stderr = FALSE))
    if (length(out) != 1L || !nzchar(out))
        out <- system2("sha256sum", shQuote(path), stdout = TRUE)
    sub(" .*$", "", out[[1]])
}

record <- function(file, kind, ...) {
    path <- file.path(OUTDIR, file)
    .entries[[file]] <<- c(list(file = file, kind = kind,
                                bytes = as.integer(file.size(path)),
                                sha256 = sha256(path)), list(...))
    cat(sprintf("  %-46s %9d B  %s\n", file, file.size(path),
                substr(.entries[[file]]$sha256, 1, 12)))
}

## Raw little-endian float64 dump: the only lossless, language-neutral
## format for a numeric matrix. Row-major, so numpy reads it with
## np.fromfile(...).reshape(nrow, ncol).
write_f64 <- function(m, file, dimnames_files = NULL) {
    path <- file.path(OUTDIR, file)
    con  <- file(path, "wb")
    on.exit(close(con))
    writeBin(as.double(as.vector(t(m))), con, size = 8, endian = "little")
    invisible(path)
}

write_csv_exact <- function(df, file) {
    ## 17 significant digits round-trips a float64 exactly. Used only
    ## for small tabular data; matrices go through write_f64().
    utils::write.csv(format(df, digits = 17, trim = TRUE, scientific = FALSE),
                     file.path(OUTDIR, file), row.names = FALSE,
                     quote = TRUE, na = "")
}

write_json <- function(x, file) {
    writeLines(toJSON(x, auto_unbox = TRUE, digits = 17, pretty = TRUE,
                      null = "null"), file.path(OUTDIR, file))
}

## Factor -> forced coding. Returns the spec the Python loader asserts.
factor_spec <- function(f) {
    stopifnot(is.factor(f))
    list(dtype = "category", categories = levels(f), ordered = TRUE,
         note = paste("Level order is protocol semantics: sites are",
                      "visited in this order and categories[[1]] is the",
                      "lead decryptor."))
}

## ---- 1. DLBCL clinical table ---------------------------------------------

cat("[1/8] DLBCL clinical table\n")
data(DLBCL, package = "homomorpheR")

stopifnot(is.factor(DLBCL$Subgroup), is.integer(DLBCL$ID) || is.numeric(DLBCL$ID))
write_csv_exact(DLBCL, "dlbcl_clinical.csv")

## Declare EVERY column: the loader asserts the manifest rather than
## letting pandas infer. Values are written as quoted 17-digit strings
## (exact float64 round-trip), so the loader must cast explicitly.
.dlbcl_dtypes <- lapply(DLBCL, function(x)
    if (is.factor(x)) factor_spec(x)
    else if (is.integer(x)) "int64"
    else if (is.numeric(x)) "float64"
    else "str")

record("dlbcl_clinical.csv", "table",
       nrow = nrow(DLBCL), ncol = ncol(DLBCL),
       columns = names(DLBCL),
       csv_note = paste("All fields are quoted; numerics are 17-significant-",
                        "digit strings that round-trip float64 exactly.",
                        "Cast per `dtypes`; do not rely on inference."),
       dtypes = .dlbcl_dtypes,
       site_order = levels(DLBCL$Subgroup),
       site_sizes = as.list(table(DLBCL$Subgroup)[levels(DLBCL$Subgroup)]),
       provenance = paste("Rosenwald et al. (2002) LLMPP DLBCL cohort;",
                          "five zero-follow-up patients excluded following",
                          "Bayle et al. (2025)."))

## ---- 2. DLBCL gene expression matrix -------------------------------------

cat("[2/8] DLBCL expression matrix (raw float64 + dimnames)\n")
data(DLBCL_gex, package = "homomorpheR")
stopifnot(is.matrix(DLBCL_gex), is.numeric(DLBCL_gex))

write_f64(DLBCL_gex, "dlbcl_gex.f64")
writeLines(rownames(DLBCL_gex), file.path(OUTDIR, "dlbcl_gex_rownames.txt"))
writeLines(colnames(DLBCL_gex), file.path(OUTDIR, "dlbcl_gex_colnames.txt"))

record("dlbcl_gex.f64", "matrix_f64",
       shape = c(nrow(DLBCL_gex), ncol(DLBCL_gex)),
       order = "C", dtype = "float64", endian = "little",
       rownames_file = "dlbcl_gex_rownames.txt",
       colnames_file = "dlbcl_gex_colnames.txt",
       alignment = paste("as.character(dlbcl$ID) must equal the row names,",
                         "elementwise and in order"))
record("dlbcl_gex_rownames.txt", "lines", n = nrow(DLBCL_gex), dtype = "str")
record("dlbcl_gex_colnames.txt", "lines", n = ncol(DLBCL_gex), dtype = "str")

## Alignment invariant the Cox-lasso vignette asserts.
stopifnot(identical(as.character(DLBCL$ID), rownames(DLBCL_gex)))

## ---- 3. mle.Rmd: Poisson counts ------------------------------------------

cat("[3/8] mle: Poisson counts\n")
set.seed(17822)
mle_y <- rpois(n = 40, lambda = 10)
write_json(list(seed = 17822L, n = 40L, lambda_true = 10,
                y = mle_y,
                sites = list(list(name = "site1", y = mle_y[1:15]),
                             list(name = "site2", y = mle_y[16:28]),
                             list(name = "site3", y = mle_y[29:40]))),
           "mle_poisson.json")
record("mle_poisson.json", "json", generator = "rpois",
       note = "R rpois (Ahrens-Dieter) is not reproducible by numpy.")

## ---- 4. privacy-preserving-aggregation.Rmd -------------------------------

cat("[4/8] aggregation: three site cohorts\n")
set.seed(42)
agg_sites <- lapply(c(1000, 500, 1500), function(n) {
    data.frame(age       = sample(40:70, n, replace = TRUE),
               sex       = sample(c("M", "F"), n, replace = TRUE),
               biomarker = runif(n, 0, 1),
               stringsAsFactors = FALSE)
})
write_json(list(seed = 42L, site_sizes = c(1000L, 500L, 1500L),
                dtypes = list(age = "int64", sex = "str",
                              biomarker = "float64"),
                sites = lapply(agg_sites, as.list)),
           "aggregation_sites.json")
record("aggregation_sites.json", "json", generator = "sample/runif",
       note = "sex is a plain character vector here, not a factor.")

## ---- 5. query-count-threshold.Rmd ----------------------------------------

cat("[5/8] query-count: three site cohorts + the query\n")
set.seed(130)
qc_sizes <- c(60, 15, 25)
qc_data <- local({
    tmp   <- c(0, cumsum(qc_sizes))
    start <- tmp[1:3] + 1
    end   <- tmp[-1]
    id_list <- Map(seq, from = start, to = end)
    lapply(seq_along(qc_sizes), function(i) {
        data.frame(id  = sprintf("P%4d", id_list[[i]]),
                   sex = sample(c("F", "M"), qc_sizes[i], replace = TRUE),
                   age = sample(40:70,      qc_sizes[i], replace = TRUE),
                   bm  = rnorm(qc_sizes[i]),
                   stringsAsFactors = FALSE)
    })
})
qc_counts <- vapply(qc_data,
                    function(d) sum(d$age < 50 & d$sex == "F" & d$bm < 0.2),
                    integer(1))
write_json(list(seed = 130L, site_sizes = as.integer(qc_sizes),
                dtypes = list(id = "str", sex = "str", age = "int64",
                              bm = "float64"),
                query = list(r = 'age < 50 & sex == "F" & bm < 0.2',
                             python = '(age < 50) & (sex == "F") & (bm < 0.2)'),
                expected = list(per_site = as.integer(qc_counts),
                                total = sum(qc_counts),
                                exact = TRUE,
                                plaintext_modulus = 65537L,
                                note = paste("BFV is exact: assert ==,",
                                             "not a tolerance.")),
                sites = lapply(qc_data, as.list)),
           "query_count.json")
record("query_count.json", "json", generator = "sample/rnorm",
       expected_total = sum(qc_counts))

## ---- 6. encrypted-regression.Rmd -----------------------------------------

cat("[6/8] encrypted-regression: logistic training set\n")
set.seed(123)
er_n         <- 500
er_age       <- rnorm(er_n, 55, 10)
er_biomarker <- rnorm(er_n, 0, 1)
er_prob      <- plogis(-2 + 0.03 * er_age + 0.8 * er_biomarker)
er_outcome   <- rbinom(er_n, 1, er_prob)
er_beta      <- coef(glm(er_outcome ~ er_age + er_biomarker,
                         family = binomial))
write_json(list(seed = 123L, n = er_n,
                beta_true = c(intercept = -2, age = 0.03, biomarker = 0.8),
                age = er_age, biomarker = er_biomarker,
                outcome = as.integer(er_outcome),
                expected = list(beta_fit = unname(er_beta),
                                beta_names = c("(Intercept)", "age", "biomarker"),
                                note = paste("glm IRLS vs statsmodels Logit",
                                             "agree to ~1e-8; assert at 1e-6."))),
           "encrypted_regression.json")
record("encrypted_regression.json", "json", generator = "rnorm/rbinom",
       note = "R rbinom (BTPE) is not reproducible by numpy.")

## ---- 7. secure-inference.Rmd (deterministic, no RNG) ---------------------

cat("[7/8] secure-inference: fixed biomarker panel\n")
write_json(list(seed = NULL,
                note = "Hand-specified in the vignette; no RNG involved.",
                biomarkers = list(
                    b1 = c(1.2, 0.8, 1.5, 0.3, 2.1, 0.9, 1.1, 1.8),
                    b2 = c(0.5, 1.1, 0.3, 0.8, 0.2, 1.4, 0.7, 0.6),
                    b3 = c(2.0, 1.5, 2.3, 1.0, 1.8, 2.1, 1.6, 2.5),
                    b4 = c(0.1, 0.4, 0.2, 0.6, 0.3, 0.1, 0.5, 0.2))),
           "secure_inference.json")
record("secure_inference.json", "json", generator = "none")

## ---- 8. cvxr_consensus golden outputs ------------------------------------

cat("[8/8] cvxr_consensus golden fixture\n")
data(cvxr_consensus, package = "homomorpheR")

## Ragged: trajectory is a list of 147 numeric(100). JSON, not parquet.
write_json(list(
    params      = cvxr_consensus$params,
    top_idx     = cvxr_consensus$top_idx,
    sigma_K     = unname(cvxr_consensus$sigma_K),
    sigma_K_names = names(cvxr_consensus$sigma_K),
    agg_beta    = cvxr_consensus$agg_beta,
    z_ref       = cvxr_consensus$z_ref,
    z_enc       = cvxr_consensus$z_enc,
    n_iter_ref  = cvxr_consensus$n_iter_ref,
    n_iter_enc  = cvxr_consensus$n_iter_enc,
    trajectory  = cvxr_consensus$trajectory,
    pool_agree  = cvxr_consensus$pool_agree,
    screen_match = cvxr_consensus$screen_match,
    tolerances = list(
        z_ref_vs_python = list(value = 1e-4, kind = "statistical",
            note = paste("CVXR and cvxpy canonicalize independently and",
                         "ship different CLARABEL builds; this is a",
                         "solver-scale tolerance, NOT CKKS noise.")),
        z_enc_vs_z_ref  = list(value = 1e-5, kind = "ckks",
            note = "Within-language encrypted-vs-plaintext; the real claim."),
        n_iter          = list(value = 5L, kind = "iteration_count",
            note = paste("Absolute stopping rule near a slowly decaying",
                         "dual residual. R reproduces 147 exactly across",
                         "runs and an openfhe.R version change (verified",
                         "2026-08-05); cvxpy may land nearby, not on it.")),
        top_idx         = list(value = 0L, kind = "set_equality",
            note = paste("Screening is deterministic given identical",
                         "input bytes; require exact set equality."))),
    provenance = paste("data(cvxr_consensus) from homomorpheR",
                       as.character(utils::packageVersion("homomorpheR")),
                       "-- generated by inst/scripts/cvxr-consensus.R;",
                       "recompute-verified 2026-08-05 (z_ref/agg_beta/",
                       "sigma_K bit-identical, z_enc 7.8e-08).")),
    "cvxr_consensus_golden.json")
record("cvxr_consensus_golden.json", "golden",
       n_iter_enc = cvxr_consensus$n_iter_enc,
       K = cvxr_consensus$params$K)

## ---- manifest -------------------------------------------------------------

cat("\nwriting manifest\n")
manifest <- list(
    schema_version = 1L,
    generated_by   = "homomorpheR/data-raw/export_parity_fixtures.R",
    homomorpheR    = as.character(utils::packageVersion("homomorpheR")),
    openfhe_R      = as.character(utils::packageVersion("openfhe.R")),
    R_version      = paste(R.version$major, R.version$minor, sep = "."),
    rng            = list(kind = RNGkind(),
                          note = paste("R's rpois/rbinom/sample/rnorm have no",
                                       "numpy equivalent; these fixtures are",
                                       "the only way both languages see the",
                                       "same inputs.")),
    contracts = list(
        forced_categoricals = paste("Categorical codings are forced from the",
                                    "manifest, never inferred. Site order is",
                                    "protocol semantics (lead decryptor)."),
        float_matrices      = paste("Raw little-endian float64 with a sha256",
                                    "over those bytes; never CSV."),
        integrity           = "Every file carries a sha256; verify on load."),
    files = unname(.entries))
write_json(manifest, "manifest.json")
cat(sprintf("  %-46s %9d B\n", "manifest.json",
            file.size(file.path(OUTDIR, "manifest.json"))))

cat("\ndone:", length(.entries), "fixture files + manifest\n")
