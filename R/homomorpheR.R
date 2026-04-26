#' homomorpheR: Homomorphic computations in R
#'
#' `homomorpheR` provides homomorphic encryption schemes for
#' privacy-preserving distributed computations: applications of the
#' sort implemented in package `distcomp`. The Paillier cryptosystem
#' is implemented natively in R via the `gmp` package; CKKS, BFV, BGV,
#' and FHEW/TFHE schemes are available through the `openfhe` package.
#'
#' Encrypted values are wrapped in [PaillierCiphertext] objects so that
#' R's arithmetic operators dispatch to the homomorphism. Use
#' [paillier_keypair()] to generate keys, [encrypt()] to encrypt, and
#' [decrypt()] to recover the result.
#'
#' For a quick overview, see the package vignettes.
#'
#' @references [Homomorphic Encryption](https://en.wikipedia.org/wiki/Homomorphic_encryption)
#' @references [Paillier Encryption](https://mhe.github.io/jspaillier/)
#'
#' @examples
#' \dontrun{
#' keys <- paillier_keypair(1024)
#' encrypt_decrypt <- function(x) decrypt(get_private_key(keys),
#'                                        encrypt(keys@pubkey, x))
#' a <- gmp::as.bigz(1273849)
#' identical(a + 10L, encrypt_decrypt(a + 10L))
#' }
#' @name homomorpheR
"_PACKAGE"

ONE  <- gmp::as.bigz(1L)
ZERO <- gmp::as.bigz(0L)

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
