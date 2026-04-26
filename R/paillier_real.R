#' @importFrom S7 new_class method class_any S7_inherits
NULL

## Paillier encryption of real numbers via int + fractional split.
##
## Paillier natively encrypts non-negative integers modulo n.
## Real-valued workloads (negative log-likelihoods, Cox partial
## likelihoods, etc.) are handled by splitting a real x into:
##
##     int  = floor(x)
##     frac = round((x - int) * den)        for some large bigq `den`
##
## and encrypting `int` and `frac` as separate Paillier ciphertexts.
## At decryption the master recovers `decrypt(int) + decrypt(frac)/den`.
## Two `PaillierEncryptedReal` objects with the same denominator add
## and subtract componentwise; this is enough to drive every
## real-valued protocol implemented here.

#' A Paillier-encrypted real number
#'
#' A pair of Paillier ciphertexts representing the integer and
#' fractional parts of a real number, together with the denominator
#' used to scale the fractional part. Two `PaillierEncryptedReal`
#' values encrypted under the same key with the same denominator
#' combine via the standard arithmetic operators.
#'
#' # Signed-arithmetic convention
#'
#' Paillier's plaintext space is `Z_n` (a residue class modulo `n`,
#' the modulus carried by the public key). Negative real numbers and
#' running totals that cross zero are stored in their mod-`n`
#' representation, which lives in the upper half of `[0, n)`. The
#' [decrypt()] method for `PaillierEncryptedReal` re-centres the raw
#' decrypted residues into the interval `(-n/2, n/2)` so that signed
#' values round-trip correctly. This means a `PaillierEncryptedReal`
#' is *correct for signed real arithmetic* as long as the true
#' cleartext stays in `(-n/2, n/2)` — for default 1024-bit keys, that
#' is `> 10^307`, well beyond any plausible statistical workload.
#'
#' This convention applies only to `PaillierEncryptedReal`. The
#' integer-only [PaillierCiphertext] decrypt method preserves raw
#' mod-`n` semantics and does not centre.
#'
#' @param int the [PaillierCiphertext] holding the integer part.
#' @param frac the [PaillierCiphertext] holding the scaled fractional part.
#' @param den the denominator used to scale the fractional part (a [gmp::bigq]).
#' @export
PaillierEncryptedReal <- new_class(
    "PaillierEncryptedReal",
    package = "homomorpheR",
    properties = list(
        int  = PaillierCiphertext,
        frac = PaillierCiphertext,
        den  = class_any
    )
)

#' Encrypt a real number under a Paillier public key
#'
#' Splits `x` into integer and fractional parts, encrypts each part as
#' a separate [PaillierCiphertext], and packages the result as a
#' [PaillierEncryptedReal] so that later additions and subtractions can
#' be performed via R's arithmetic operators.
#'
#' @param public_key a [PaillierPublicKey].
#' @param x a real number.
#' @param den the denominator used to scale the fractional part. The
#'   same denominator must be used at encryption and decryption.
#' @return a [PaillierEncryptedReal].
#' @export
encrypt_real <- function(public_key, x, den) {
    int_part     <- floor(x)
    frac_part    <- x - int_part
    frac_scaled  <- gmp::as.bigz(gmp::numerator(gmp::as.bigq(frac_part) * den))
    PaillierEncryptedReal(
        int  = encrypt(public_key, int_part),
        frac = encrypt(public_key, frac_scaled),
        den  = den
    )
}

# decrypt method for the real-valued ciphertext: recover int + frac/den.
# Plaintext space is [0, n); we re-center into [-n/2, n/2) so negative
# real numbers round-trip correctly.
method(decrypt, list(PaillierPrivateKey, PaillierEncryptedReal)) <-
    function(private_key, ciphertext) {
        n <- private_key@pubkey@n
        half_n <- div.bigz(n, gmp::as.bigz(2L))
        int_raw  <- decrypt(private_key, ciphertext@int)
        frac_raw <- decrypt(private_key, ciphertext@frac)
        int_val  <- if (int_raw  > half_n) sub.bigz(int_raw,  n) else int_raw
        frac_val <- if (frac_raw > half_n) sub.bigz(frac_raw, n) else frac_raw
        as.double(int_val) +
            as.double(gmp::as.bigq(frac_val) / ciphertext@den)
    }

# ---- Operator dispatch ----------------------------------------------------

.paillier_real_add <- function(a, b) {
    if (S7_inherits(a, PaillierEncryptedReal) &&
        S7_inherits(b, PaillierEncryptedReal)) {
        if (!identical(a@den, b@den))
            cli_abort("Cannot add encrypted reals with different denominators.")
        PaillierEncryptedReal(int = a@int + b@int, frac = a@frac + b@frac, den = a@den)
    } else if (S7_inherits(a, PaillierEncryptedReal)) {
        # cleartext + encrypted: encrypt the cleartext under a's pubkey
        .paillier_real_add(a, encrypt_real(a@int@pubkey, b, a@den))
    } else {
        .paillier_real_add(encrypt_real(b@int@pubkey, a, b@den), b)
    }
}

.paillier_real_sub <- function(a, b) {
    if (S7_inherits(a, PaillierEncryptedReal) &&
        S7_inherits(b, PaillierEncryptedReal)) {
        if (!identical(a@den, b@den))
            cli_abort("Cannot subtract encrypted reals with different denominators.")
        PaillierEncryptedReal(int = a@int - b@int, frac = a@frac - b@frac, den = a@den)
    } else if (S7_inherits(a, PaillierEncryptedReal)) {
        .paillier_real_sub(a, encrypt_real(a@int@pubkey, b, a@den))
    } else {
        .paillier_real_sub(encrypt_real(b@int@pubkey, a, b@den), b)
    }
}

.paillier_real_neg <- function(a) {
    PaillierEncryptedReal(int = -a@int, frac = -a@frac, den = a@den)
}

.homomorpheR_real_Ops_handler <- function(e1, e2) {
    if (missing(e2)) {
        switch(.Generic,
            "-" = .paillier_real_neg(e1),
            "+" = e1,
            cli_abort("Unary {.code {(.Generic)}} not supported on {.cls PaillierEncryptedReal}.")
        )
    } else {
        switch(.Generic,
            "+" = .paillier_real_add(e1, e2),
            "-" = .paillier_real_sub(e1, e2),
            cli_abort("Operator {.code {(.Generic)}} not supported on {.cls PaillierEncryptedReal}.")
        )
    }
}

.homomorpheR_real_chooseOpsMethod <- function(x, y, mx, my, cl, reverse) TRUE

method(print, PaillierEncryptedReal) <- function(x, ...) {
    cat("<PaillierEncryptedReal> (under ", x@int@pubkey@bits, "-bit key)\n", sep = "")
    invisible(x)
}
