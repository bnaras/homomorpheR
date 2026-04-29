## Paillier cryptosystem — S7 implementation.
##
## The Paillier scheme provides additive homomorphic encryption over
## integers modulo a large modulus n. Encryption produces a value modulo
## n^2; the homomorphism is:
##
##     enc(a + b) = enc(a) * enc(b)         (mod n^2)
##     enc(k * a) = enc(a) ^ k              (mod n^2)
##
## Encrypted values are wrapped in `PaillierCiphertext` S7 objects so
## that R's arithmetic operators dispatch through the S3 Ops handler
## registered in `zzz.R`.

#' @importFrom S7 new_class new_generic new_object method method<- class_any class_numeric class_integer S7_object S7_inherits
#' @importFrom gmp add.bigz sub.bigz mul.bigz mod.bigz div.bigz powm inv.bigz
#' @importFrom gmp lcm.bigz isprime sizeinbase as.bigz as.bigq numerator
#' @importFrom cli cli_abort
NULL

# ---- Classes ---------------------------------------------------------------

#' Paillier public key
#'
#' Holds the modulus and precomputed values used during encryption.
#' Construct via [paillier_keypair()] rather than directly.
#'
#' @param bits modulus length in bits.
#' @param n the modulus.
#' @export
PaillierPublicKey <- new_class(
    "PaillierPublicKey",
    package = "homomorpheR",
    properties = list(
        bits      = class_integer,
        n         = class_any,
        n_squared = class_any,
        n_plus_one = class_any
    ),
    constructor = function(bits, n) {
        new_object(
            S7_object(),
            bits       = as.integer(bits),
            n          = n,
            n_squared  = mul.bigz(n, n),
            n_plus_one = add.bigz(n, ONE)
        )
    }
)

#' Paillier private key
#'
#' Holds the secret `lambda` and a cached value `x` used during
#' decryption, together with a reference to the matching public key.
#'
#' @param lambda the secret lambda.
#' @param pubkey the matching [PaillierPublicKey].
#' @export
PaillierPrivateKey <- new_class(
    "PaillierPrivateKey",
    package = "homomorpheR",
    properties = list(
        pubkey = PaillierPublicKey,
        lambda = class_any,
        x      = class_any
    ),
    constructor = function(lambda, pubkey) {
        x <- inv.bigz(
            div.bigz(
                sub.bigz(powm(pubkey@n_plus_one, lambda, pubkey@n_squared), ONE),
                pubkey@n),
            pubkey@n)
        new_object(
            S7_object(),
            pubkey = pubkey,
            lambda = lambda,
            x      = x
        )
    }
)

#' Paillier key pair
#'
#' A matched pair of public and private keys. Use [paillier_keypair()]
#' to generate one.
#'
#' @param pubkey a [PaillierPublicKey].
#' @param privkey a [PaillierPrivateKey].
#' @export
PaillierKeyPair <- new_class(
    "PaillierKeyPair",
    package = "homomorpheR",
    properties = list(
        pubkey  = PaillierPublicKey,
        privkey = PaillierPrivateKey
    )
)

#' A Paillier ciphertext
#'
#' Wraps the encrypted big-integer value together with the public key
#' it was encrypted under. Two ciphertexts encrypted under the same
#' public key can be combined with `+` and `-`; a ciphertext can be
#' multiplied by a cleartext integer with `*`.
#'
#' @param value the encrypted big-integer value.
#' @param pubkey the [PaillierPublicKey] under which it was encrypted.
#' @export
PaillierCiphertext <- new_class(
    "PaillierCiphertext",
    package = "homomorpheR",
    properties = list(
        value  = class_any,
        pubkey = PaillierPublicKey
    )
)

# ---- Generics --------------------------------------------------------------

#' Encrypt a value under a Paillier public key
#'
#' @param public_key a [PaillierPublicKey].
#' @param ... method-specific arguments. The Paillier method takes a
#'   single cleartext value `x` (integer or [gmp::bigz]).
#' @return a [PaillierCiphertext].
#' @export
encrypt <- new_generic("encrypt", "public_key")

#' Decrypt a Paillier ciphertext
#'
#' Dispatches on both `private_key` and `ciphertext` so that integer
#' [PaillierCiphertext]s and [PaillierEncryptedReal]s are handled by
#' separate methods.
#'
#' # Return semantics
#'
#' The two cases differ deliberately:
#'
#' - [PaillierCiphertext] (integer) -> a [gmp::bigz] in `[0, n)`. This
#'   preserves raw mod-`n` arithmetic; callers wanting signed
#'   integers should re-center themselves (`if (m > n/2) m - n`).
#' - [PaillierEncryptedReal] -> a `numeric` in `(-n/2, n/2)`. The
#'   method re-centers the raw mod-`n` residues so that negative
#'   real numbers and running totals that cross zero round-trip
#'   correctly. See [PaillierEncryptedReal] for the full convention.
#'
#' @param private_key a [PaillierPrivateKey].
#' @param ciphertext a [PaillierCiphertext] or [PaillierEncryptedReal].
#' @param ... unused.
#' @return the decrypted value.
#' @export
decrypt <- new_generic("decrypt", c("private_key", "ciphertext"))

#' Return the private key from a key pair
#' @param keypair a [PaillierKeyPair].
#' @param ... unused.
#' @return a [PaillierPrivateKey].
#' @export
get_private_key <- new_generic("get_private_key", "keypair")

#' Return the secret lambda from a private key
#' @param private_key a [PaillierPrivateKey].
#' @param ... unused.
#' @return a [gmp::bigz] value.
#' @export
get_lambda <- new_generic("get_lambda", "private_key")

# ---- Methods ---------------------------------------------------------------

method(encrypt, PaillierPublicKey) <- function(public_key, x) {
    m <- as.bigz(x)
    enc <- mod.bigz(
        add.bigz(mul.bigz(public_key@n, m), ONE),
        public_key@n_squared)
    PaillierCiphertext(
        value  = .paillier_randomize(public_key, enc),
        pubkey = public_key)
}

method(decrypt, list(PaillierPrivateKey, PaillierCiphertext)) <-
    function(private_key, ciphertext) {
        pubkey <- private_key@pubkey
        mod.bigz(
            mul.bigz(
                div.bigz(
                    sub.bigz(
                        powm(ciphertext@value, private_key@lambda, pubkey@n_squared),
                        ONE),
                    pubkey@n),
                private_key@x),
            pubkey@n)
    }

method(get_private_key, PaillierKeyPair) <- function(keypair) keypair@privkey

method(get_lambda, PaillierPrivateKey) <- function(private_key) private_key@lambda

# ---- Internal helpers ------------------------------------------------------

# Multiply by a fresh random factor r^n (mod n^2) so that the same
# plaintext encrypts to different ciphertexts each time.
.paillier_randomize <- function(public_key, a) {
    repeat {
        r <- random.bigz(nBits = public_key@bits)
        if (r < public_key@n) break
    }
    rn <- powm(r, public_key@n, public_key@n_squared)
    mod.bigz(mul.bigz(a, rn), public_key@n_squared)
}

# Homomorphic addition: enc(a + b) = enc(a) * enc(b) mod n^2.
.paillier_add <- function(e1, e2) {
    if (S7_inherits(e1, PaillierCiphertext) &&
        S7_inherits(e2, PaillierCiphertext)) {
        if (!identical(e1@pubkey@n, e2@pubkey@n)) {
            cli_abort("Cannot add ciphertexts under different public keys.")
        }
        pubkey <- e1@pubkey
        PaillierCiphertext(
            value  = mod.bigz(mul.bigz(e1@value, e2@value), pubkey@n_squared),
            pubkey = pubkey)
    } else if (S7_inherits(e1, PaillierCiphertext)) {
        # cleartext + ciphertext: encrypt the cleartext under e1's pubkey
        .paillier_add(e1, encrypt(e1@pubkey, e2))
    } else {
        .paillier_add(encrypt(e2@pubkey, e1), e2)
    }
}

# Homomorphic subtraction: enc(a - b) = enc(a) * enc(b)^{-1} mod n^2.
# The modular *multiplicative* inverse of enc(b) is the encryption of
# the additive negation of b — not n^2 - enc(b).
.paillier_sub <- function(e1, e2) {
    if (S7_inherits(e1, PaillierCiphertext) &&
        S7_inherits(e2, PaillierCiphertext)) {
        if (!identical(e1@pubkey@n, e2@pubkey@n)) {
            cli_abort("Cannot subtract ciphertexts under different public keys.")
        }
        pubkey <- e1@pubkey
        neg_b <- inv.bigz(e2@value, pubkey@n_squared)
        PaillierCiphertext(
            value  = mod.bigz(mul.bigz(e1@value, neg_b), pubkey@n_squared),
            pubkey = pubkey)
    } else if (S7_inherits(e1, PaillierCiphertext)) {
        .paillier_sub(e1, encrypt(e1@pubkey, e2))
    } else {
        .paillier_sub(encrypt(e2@pubkey, e1), e2)
    }
}

# Scalar multiplication: enc(k * m) = enc(m)^k mod n^2.
.paillier_mult <- function(e1, e2) {
    if (S7_inherits(e1, PaillierCiphertext) &&
        S7_inherits(e2, PaillierCiphertext)) {
        cli_abort(c(
            "Cannot multiply two Paillier ciphertexts.",
            "i" = "Paillier is additively homomorphic only; scalar multiplication is supported via {.code ct * k} where {.var k} is a cleartext integer."
        ))
    }
    if (S7_inherits(e1, PaillierCiphertext)) {
        ct <- e1; k <- as.bigz(e2)
    } else {
        ct <- e2; k <- as.bigz(e1)
    }
    PaillierCiphertext(
        value  = powm(ct@value, k, ct@pubkey@n_squared),
        pubkey = ct@pubkey)
}

# Unary minus: invert in the multiplicative group mod n^2, which
# corresponds to additive inverse in the plaintext.
.paillier_neg <- function(e1) {
    PaillierCiphertext(
        value  = inv.bigz(e1@value, e1@pubkey@n_squared),
        pubkey = e1@pubkey)
}

# ---- S3 Ops handler --------------------------------------------------------
#
# Registered in zzz.R via registerS3method("Ops", "homomorpheR::PaillierCiphertext", ...).
# We use S3 (not S7's built-in Ops) because S7 evaluates both arguments
# even for unary operators, which fails on `-x`. Same pattern as openfhe
# and CVXR.

.homomorpheR_Ops_handler <- function(e1, e2) {
    if (missing(e2)) {
        switch(.Generic,
            "-" = .paillier_neg(e1),
            "+" = e1,
            cli_abort("Unary {.code {(.Generic)}} not supported on {.cls PaillierCiphertext}.")
        )
    } else {
        switch(.Generic,
            "+" = .paillier_add(e1, e2),
            "-" = .paillier_sub(e1, e2),
            "*" = .paillier_mult(e1, e2),
            cli_abort("Operator {.code {(.Generic)}} not supported on {.cls PaillierCiphertext}.")
        )
    }
}

# Ensure homomorpheR wins Ops dispatch against Matrix / other S3 classes.
.homomorpheR_chooseOpsMethod <- function(x, y, mx, my, cl, reverse) TRUE

# ---- Print methods ---------------------------------------------------------

method(print, PaillierPublicKey) <- function(x, ...) {
    cat("<PaillierPublicKey>\n")
    cat("  bits:", x@bits, "\n")
    cat("  n:   ", format(x@n), "\n", sep = "")
    invisible(x)
}

method(print, PaillierPrivateKey) <- function(x, ...) {
    cat("<PaillierPrivateKey> (paired with public key of",
        x@pubkey@bits, "bits)\n")
    invisible(x)
}

method(print, PaillierKeyPair) <- function(x, ...) {
    cat("<PaillierKeyPair>\n")
    cat("  bits:", x@pubkey@bits, "\n")
    invisible(x)
}

method(print, PaillierCiphertext) <- function(x, ...) {
    cat("<PaillierCiphertext> (under ", x@pubkey@bits, "-bit key)\n", sep = "")
    invisible(x)
}

# ---- Constructor -----------------------------------------------------------

#' Generate a new Paillier key pair
#'
#' Generates two random primes of `modulus_bits / 2` bits each, forms
#' the modulus, and returns a [PaillierKeyPair] containing the matching
#' public and private keys.
#'
#' @param modulus_bits modulus length in bits (e.g. 1024 or 2048).
#' @return a [PaillierKeyPair].
#' @examples
#' \dontrun{
#' keys <- paillier_keypair(1024)
#' ct   <- encrypt(keys@pubkey, gmp::as.bigz(42))
#' decrypt(get_private_key(keys), ct)
#' }
#' @export
paillier_keypair <- function(modulus_bits) {
    repeat {
        repeat {
            p <- random.bigz(nBits = modulus_bits %/% 2)
            if (isprime(n = p, reps = 10)) break
        }
        repeat {
            q <- random.bigz(nBits = modulus_bits %/% 2)
            if (isprime(n = q, reps = 10)) break
        }
        n <- mul.bigz(p, q)
        if (p != q && sizeinbase(a = n, b = 2) == modulus_bits) break
    }
    pubkey  <- PaillierPublicKey(bits = modulus_bits, n = n)
    lambda  <- lcm.bigz(sub.bigz(p, ONE), sub.bigz(q, ONE))
    privkey <- PaillierPrivateKey(lambda = lambda, pubkey = pubkey)
    PaillierKeyPair(pubkey = pubkey, privkey = privkey)
}
