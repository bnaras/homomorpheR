## FROZEN (Paillier legacy, 2026-08-05) -- do not extend. Kept only
## for the archived Paillier vignettes (paillier-archive/) and the
## API distcomp pins; slated for un-export and eventual removal once
## a revamped distcomp (dropping its homomorpheR imports) reaches
## CRAN ahead of the next homomorpheR release. The package-level help
## page lives in R/homomorpheR-package.R.

ONE  <- gmp::as.bigz(1L)

#' Random big integer
#'
#' Returns a random big integer using the cryptographically secure
#' generator from the `sodium` package.
#'
#' @param nBits number of bits, which must be a multiple of 8 (not
#'   checked, for efficiency).
#' @return a [gmp::bigz] value.
#' @importFrom gmp as.bigz
#' @importFrom sodium random
#' @export
random.bigz <- function(nBits) {
    nBytes <- nBits %/% 8L
    as.bigz(paste0(c("0x", sodium::random(n = nBytes)), collapse = ""))
}
