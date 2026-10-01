## R-SPECIFIC: roxygen documentation for the shipped DLBCL clinical table,
## the DLBCL_gex expression matrix, and the precomputed result objects.

#' Diffuse Large B-cell Lymphoma Cohort (Rosenwald et al. 2002)
#'
#' Patient-level survival data and gene-expression signature scores
#' from the diffuse large-B-cell lymphoma (DLBCL) cohort of
#' Rosenwald et al. (2002). Used in the Cox-regression vignettes
#' to demonstrate distributed Cox estimation under threshold FHE
#' with sites partitioned by molecular subgroup.
#'
#' @usage data(DLBCL)
#'
#' @format A data frame with 235 observations on the following 12 variables:
#' \describe{
#'   \item{\code{ID}}{LYM patient identifier (integer).}
#'   \item{\code{Set}}{Original analysis set assignment, either
#'     \code{"Training"} or \code{"Validation"}.}
#'   \item{\code{Subgroup}}{Molecular subgroup, a factor with
#'     levels \code{"GCB"} (germinal-center B-cell-like),
#'     \code{"ABC"} (activated B-cell-like), and
#'     \code{"Type III"} (unclassified).}
#'   \item{\code{IPI}}{International Prognostic Index group
#'     (\code{"Low"}, \code{"Medium"}, \code{"High"}, or
#'     \code{NA}).}
#'   \item{\code{time}}{Follow-up time in years.}
#'   \item{\code{status}}{Vital status at last follow-up coded as
#'     \code{1} for death and \code{0} for alive at follow-up.}
#'   \item{\code{GCB_sig}}{Germinal-center B-cell signature score.}
#'   \item{\code{LN_sig}}{Lymph-node signature score.}
#'   \item{\code{Prolif_sig}}{Proliferation signature score.}
#'   \item{\code{BMP6}}{BMP6 expression score.}
#'   \item{\code{MHC2_sig}}{MHC class II signature score.}
#'   \item{\code{Score}}{Outcome predictor score combining the four
#'     signatures and \code{BMP6}, as published.}
#' }
#'
#' @details
#' Each row represents one patient. The four signature columns and
#' \code{BMP6} are carried over as published, without further scaling.
#' \code{GCB_sig}, \code{LN_sig}, \code{Prolif_sig} and
#' \code{MHC2_sig} are averages of median-centered log-ratio
#' expression values over the genes of each signature; \code{BMP6}
#' is the median-centered log ratio of the single gene BMP6
#' (Rosenwald et al. 2002, Supplementary Appendix 1). Following Bayle, Fan
#' and Lou (2025), the five patients with zero follow-up time are
#' excluded, so the cohort spans 235 patients with 133 deaths (event
#' rate 56.6%) over a median follow-up of 2.8 years. The
#' molecular-subgroup partition gives three sites of unequal size:
#' GCB (n=115, 54 deaths), ABC (n=71, 49 deaths), and Type III
#' (n=49, 30 deaths).
#'
#' The vignettes use \code{Subgroup} as the site boundary for
#' distributed Cox estimation; the partition is a choice made for
#' the demonstration. Stratified Cox regression with
#' \code{strata(Subgroup)} factors the partial log-likelihood
#' additively across the three subgroups, which is exactly the
#' decomposition the master/worker protocol exploits.
#'
#' @source The Lymphoma/Leukemia Molecular Profiling Project release of the
#'   Rosenwald et al. (2002) study, file
#'   \file{DLBCL_patient_data_NEW.txt} at
#'   <https://llmpp.ccr.cancer.gov/DLBCL/>; processed by
#'   \file{data-raw/DLBCL.R}.
#'
#' @references
#' Rosenwald, A., Wright, G., Chan, W. C., et al. (2002). The use
#' of molecular profiling to predict survival after chemotherapy
#' for diffuse large-B-cell lymphoma. *New England Journal of
#' Medicine* **346**(25), 1937--1947. \doi{10.1056/NEJMoa012914}
#'
#' Bayle, P., Fan, J., and Lou, Z. (2025). Communication-Efficient
#' Distributed Estimation and Inference for Cox's Model.
#' *Journal of the American Statistical Association*.
#' \doi{10.1080/01621459.2025.2516820}
#'
#' @seealso [DLBCL_gex] for the full Lymphochip gene-expression
#'   matrix (235 x 6416) on the same cohort.
#'
#' @examples
#' data(DLBCL)
#' table(DLBCL$Subgroup, DLBCL$status)
#'
#' ## Stratified Cox fit on the four signatures and BMP6
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   fit <- survival::coxph(
#'     survival::Surv(time, status) ~ GCB_sig + LN_sig + Prolif_sig +
#'       BMP6 + MHC2_sig + survival::strata(Subgroup),
#'     data = DLBCL)
#'   print(fit)
#' }
#' @keywords datasets
"DLBCL"

#' DLBCL Lymphochip gene-expression matrix
#'
#' Gene-expression profiles for the 235-patient DLBCL cohort of
#' Rosenwald et al. (2002), used as the high-dimensional benchmark
#' in the encrypted distributed Cox-lasso demonstration.
#'
#' @format A numeric matrix with 235 rows (patients) and 6416
#'   columns (Lymphochip microarray features). Row names are the
#'   patient LYM identifiers, matching `DLBCL$ID`; column names are
#'   the microarray UNIQIDs. Values are log-ratios on the original
#'   scale.
#'
#' @details
#' Derived from the public Lymphoma/Leukemia Molecular Profiling
#' Project release (<https://llmpp.ccr.cancer.gov/DLBCL/>; files
#' `DLBCL_patient_data_NEW.txt` and `NEJM_Web_Fig1data`). Patients
#' are matched to expression columns by LYM number. Of the 7399
#' Lymphochip features, the 6416 observed in at least 75% of the 240
#' patients are retained, and their remaining missing values are imputed
#' by the per-feature mean (Bayle, Fan and Lou, 2025, impute by the median); the
#' five patients with zero follow-up time are excluded, leaving 235
#' patients. Standardization is deliberately *not* baked into the
#' stored matrix --- it is performed inside the encrypted pipeline
#' so that the demonstration exercises encrypted standardization.
#' The full processing script is `data-raw/DLBCL.R`.
#'
#' @source Rosenwald A, Wright G, Chan WC, et al. (2002). The use of
#'   molecular profiling to predict survival after chemotherapy for
#'   diffuse large-B-cell lymphoma. *New England Journal of Medicine*
#'   346(25):1937--1947. Data: <https://llmpp.ccr.cancer.gov/DLBCL/>.
#'
#' @seealso [DLBCL]
#' @keywords datasets
"DLBCL_gex"

#' Precomputed encrypted Cox-lasso consensus-ADMM results
#'
#' Result objects from the encrypted stratified Cox-lasso
#' consensus-ADMM demonstration on the [DLBCL] / [DLBCL_gex] cohort:
#' a centralized [CVXR][CVXR::CVXR-package] ground-truth fit, the same
#' fit recovered by consensus ADMM in the clear, and the encrypted
#' threshold-FHE fit that swaps only the consensus channel. The
#' iterated ADMM runs are expensive, so they are computed once
#' and shipped here; the manuscript and the `cvxr-cox-lasso-dlbcl`
#' vignette load this object instead of recomputing (see Details).
#'
#' @format A named list with components
#' \describe{
#'   \item{params}{list of the run constants: `K` (screened probes,
#'     100), `LAMBDA` (L1 penalty, 5), `RHO` (ADMM penalty, 50),
#'     `MAX_ITER` (200), `TOL` (5e-3).}
#'   \item{top_idx}{integer vector of length `K`; column indices into
#'     `DLBCL_gex` of the top-`K` univariate-screened probes.}
#'   \item{sigma_K}{numeric vector of length `K`; pooled standard
#'     deviations of the screened probes, for the back-transform to
#'     the original scale.}
#'   \item{agg_beta}{numeric vector of length `K`; centralized CVXR
#'     Cox-lasso coefficients (the ground truth), on the standardized
#'     scale.}
#'   \item{z_ref}{numeric vector of length `K`; consensus-ADMM
#'     coefficients computed in the clear (cleartext reference).}
#'   \item{z_enc}{numeric vector of length `K`; consensus-ADMM
#'     coefficients under threshold FHE.}
#'   \item{trajectory}{list of numeric vectors of length `K`; the
#'     encrypted consensus iterate \eqn{z^t} at each ADMM iteration.}
#'   \item{n_iter_ref, n_iter_enc}{iterations to convergence for the
#'     plaintext and encrypted runs.}
#'   \item{pool_agree}{list `mu`, `sigma`: max absolute disagreement
#'     between the encrypted and plaintext pooled standardization
#'     moments.}
#'   \item{screen_match}{logical; whether the encrypted screen selected
#'     the same probe set as the plaintext screen.}
#' }
#'
#' @details
#' The `cvxr-cox-lasso-dlbcl` vignette is the single source of truth.
#' `data-raw/cvxr_consensus.R` extracts its code chunks with
#' [knitr::purl()] into `inst/scripts/cvxr-consensus.R`, runs that
#' script, and saves the result. The openfhe-jss manuscript reads the
#' labeled chunks of the generated script with [knitr::read_chunk()],
#' so the code displayed there is exactly the code that produced these
#' results. Find the installed copy with
#' `system.file("scripts", "cvxr-consensus.R", package = "homomorpheR")`.
#'
#' @seealso [DLBCL], [DLBCL_gex]
#' @keywords datasets
"cvxr_consensus"

## ---- Precomputed vignette results ------------------------------------
## Each object below holds the displayed results of the encrypted
## chunks of one vignette. The chunks are gated `eval = RECOMPUTE`, so a
## normal build shows the code and reads these results;
## `HOMOMORPHER_RECOMPUTE=true` runs the chunks for real. Each object is
## regenerated by the matching `data-raw/<name>.R`, which purls the
## vignette with RECOMPUTE = TRUE, sources it, and saves the result.
## Encrypted computations use fresh keys and CKKS noise, so a rerun
## reproduces these values up to CKKS approximation error.

#' Precomputed results for the `cox` vignette
#'
#' @format A list with `coef` (estimate and standard-error matrix of
#'   the encrypted single-decrypter `stats4::mle()` fit), `loglik`
#'   (its log-likelihood), and `counts` (function and gradient
#'   evaluation counts).
#' @source `data-raw/cox_results.R`, from `vignettes/cox.Rmd`.
#' @keywords datasets
"cox_results"

#' Precomputed results for the `cox-threshold` vignette
#'
#' @format A list with `coef`, `loglik`, and `counts` for the
#'   threshold-encrypted `stats4::mle()` fit, as in [cox_results], and
#'   `share_check`, a logical vector recording that the master holds
#'   no key share and that a site holds its own.
#' @source `data-raw/cox_threshold_results.R`, from
#'   `vignettes/cox-threshold.Rmd`.
#' @keywords datasets
"cox_threshold_results"

#' Precomputed results for the `cox-threshold-dp` vignette
#'
#' @format A list of the vignette's tables: `clean_check` (the fit at
#'   zero noise against `coxph()`), `bfgs_table` and `nm_table` (BFGS
#'   and Nelder-Mead fits over the noise grid), and `budget` (the zCDP
#'   privacy budget of the fits at the first three noise scales).
#' @source `data-raw/cox_threshold_dp_results.R`, from
#'   `vignettes/cox-threshold-dp.Rmd`.
#' @keywords datasets
"cox_threshold_dp_results"

#' Precomputed results for the `cvxr-consensus-admm-dp` vignette
#'
#' @format A list with `tol`, `rho_sweep` (convergence on the surrogate
#'   cohort), `rho_chosen` and `T_fixed` (the pre-committed constants),
#'   `sigma_grid` (the noise scales), `clean_dev` (deviation from the
#'   centralized fit at zero noise), and `summary_table` (coefficients
#'   at each noise scale).
#' @source `data-raw/cvxr_admm_dp_results.R`, from
#'   `vignettes/cvxr-consensus-admm-dp.Rmd`.
#' @keywords datasets
"cvxr_admm_dp_results"

#' Precomputed results for the `similarity` vignette
#'
#' @format A list of the encrypted walk-through's outputs: the printed
#'   public parameters (`pub_print`), the smoke-test errors (`rot_err`,
#'   `matvec_err`, `ip_err`, `fold_err`, `slots_same`, `slots_dev`),
#'   the site-1 and full-query timings (`site1_n`, `site1_elapsed`,
#'   `query_elapsed`), the encrypted top-k table (`top_result`), and its
#'   agreement with the cleartext reference (`score_err`, `set_match`).
#' @source `data-raw/similarity_results.R`, from
#'   `vignettes/similarity.Rmd`.
#' @keywords datasets
"similarity_results"
