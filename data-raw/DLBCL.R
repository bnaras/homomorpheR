## R-SPECIFIC: provenance and processing for the shipped DLBCL datasets.
##
## Builds data/DLBCL.rda (clinical/survival/signatures, 235 x 12) and
## data/DLBCL_gex.rda (Lymphochip gene expression, 235 x 6416) from the
## public Lymphoma/Leukemia Molecular Profiling Project (LLMPP) release
## of the diffuse large-B-cell lymphoma study of:
##
##   Rosenwald A, Wright G, Chan WC, et al. (2002). The use of molecular
##   profiling to predict survival after chemotherapy for diffuse
##   large-B-cell lymphoma. New England Journal of Medicine 346:1937-1947.
##
## Source files (https://llmpp.nih.gov/DLBCL/):
##   - DLBCL_patient_data_NEW.txt : 240 patients x clinical/survival +
##       the five published expression signatures.
##   - NEJM_Web_Fig1data          : Lymphochip log-ratio matrix,
##       7399 microarray features x 295 columns (UNIQID, NAME, samples).
##
## Processing (kept deliberately simple; an auxiliary detail with no
## bearing on the demonstrations that use the data):
##   1. Patients are matched to expression columns by LYM number: the
##      patient identifier in column 1 of the clinical table equals the
##      LYMxxx tag embedded in each sample column name. All 240 patients
##      match a unique column.
##   2. Of the 7399 features, the 6416 measured across the cohort are
##      retained; the sporadic remaining missing values are imputed by
##      the per-feature median. (Only 434 features are fully complete in
##      the raw data, so imputation is real; cf. the median imputation of
##      Bayle, Fan and Lou, 2025.)
##   3. Following Bayle, Fan and Lou (2025), the five patients with zero
##      follow-up time (LYM 61, 101, 165, 208, 391) are excluded, leaving
##      235 patients.
##   4. Values are left as log-ratios on the original scale.
##      Standardization is performed inside the encrypted pipeline of the
##      demonstrations rather than baked into the stored matrix.
##
## The processed objects are distributed with the package, so end users
## reproduce the analyses with data(DLBCL) / data(DLBCL_gex) and never
## need to re-run this script or re-download the upstream files.

## --- locate the two source files (local copy preferred, else download) ---
src <- function(name, url) {
    local <- file.path("data-raw", name)
    if (file.exists(local)) return(local)
    dest <- file.path(tempdir(), name)
    if (!file.exists(dest))
        utils::download.file(url, dest, mode = "wb")
    dest
}
base <- "https://llmpp.nih.gov/DLBCL/"
pat_file <- src("DLBCL_patient_data_NEW.txt", paste0(base, "DLBCL_patient_data_NEW.txt"))
gex_file <- src("NEJM_Web_Fig1data",          paste0(base, "NEJM_Web_Fig1data"))

## --- clinical / survival / signatures (240 patients) ---
pat <- utils::read.delim(pat_file, check.names = FALSE, stringsAsFactors = FALSE)
DLBCL <- data.frame(
    ID         = pat[[1]],
    Set        = pat[["Analysis Set"]],
    Subgroup   = pat[["Subgroup"]],
    IPI        = pat[["IPI Group"]],
    time       = pat[["Follow-up (years)"]],
    status     = as.integer(pat[["Status at follow-up"]] == "Dead"),
    GCB_sig    = pat[["Germinal center B cell signature"]],
    LN_sig     = pat[["Lymph node signature"]],
    Prolif_sig = pat[["Proliferation signature"]],
    BMP6       = pat[["BMP6"]],
    MHC2_sig   = pat[["MHC class II signature"]],
    Score      = pat[["Outcome predictor score"]],
    stringsAsFactors = FALSE,
    check.names = FALSE)

## --- expression matrix: join by LYM number, curate, impute (240 x 6416) ---
ex     <- utils::read.delim(gex_file, check.names = FALSE, stringsAsFactors = FALSE)
uniqid <- as.character(ex[[1]])
samp   <- colnames(ex)[-(1:2)]
lymnum <- ifelse(grepl("LYM[0-9]+", samp),
                 as.integer(sub("LYM0*", "",
                                regmatches(samp, regexpr("LYM[0-9]+", samp)))),
                 NA_integer_)
M <- as.matrix(ex[, -(1:2)]); storage.mode(M) <- "double"
P <- t(M[, match(DLBCL$ID, lymnum), drop = FALSE])      # 240 x 7399, with NAs
rownames(P) <- as.character(DLBCL$ID)
colnames(P) <- uniqid

## 6416 retained features (measured across the cohort); curated set is the
## one carried by the distributed object.
retained <- readLines(file.path("data-raw", "retained_features.txt"))
P <- P[, retained, drop = FALSE]
## per-feature median imputation of the remaining missing values
for (j in seq_len(ncol(P))) {
    na <- is.na(P[, j])
    if (any(na)) P[na, j] <- stats::median(P[!na, j])
}

## --- exclude the five zero-follow-up patients -> 235 ---
keep      <- DLBCL$time > 0
DLBCL     <- DLBCL[keep, , drop = FALSE]; rownames(DLBCL) <- NULL
DLBCL_gex <- P[as.character(DLBCL$ID), , drop = FALSE]

stopifnot(nrow(DLBCL) == 235L,
          all(dim(DLBCL_gex) == c(235L, 6416L)),
          all(rownames(DLBCL_gex) == as.character(DLBCL$ID)),
          !anyNA(DLBCL_gex))

save(DLBCL,     file = "data/DLBCL.rda",     compress = "xz")
save(DLBCL_gex, file = "data/DLBCL_gex.rda", compress = "xz")
