#' @importFrom S7 new_class new_generic new_object method method<- class_any class_character class_integer S7_object S7_inherits
#' @importFrom openfhe.R make_ckks_packed_plaintext get_real_packed_value make_packed_plaintext get_packed_value get_scheme_id
#' @importFrom openfhe.R multiparty_key_gen multiparty_decrypt_lead multiparty_decrypt_main multiparty_decrypt_fusion
NULL

## Site / Master actor classes -- the supported (CKKS / threshold)
## surface.
##
## These are the building blocks for multi-site protocols. A `Site`
## holds local data and a `local_fn(data, theta)` that computes a
## site-level summary; a `Master` owns the keys and orchestrates the
## protocol. The frozen Paillier-era actors (PaillierMaster, the
## NCParty topology, the round-robin chain) live in sites_legacy.R.
##
## Actors carry an environment-backed `state` property for mutable
## bookkeeping (next site in a chain, intermediate results, the master
## reference each site needs in order to signal a failed local
## computation, etc.). S7 properties themselves stay immutable.

# ---- Classes --------------------------------------------------------------

#' A site in a multi-party protocol
#'
#' Holds the site's local data and the function that computes the
#' per-site summary at a given parameter value. Use [make_site()] to
#' construct.
#'
#' @param name short identifier shown in printed output.
#' @param data local dataset.
#' @param local_fn a function with signature `function(data, theta)`
#'   returning the site-level summary at `theta`. May return `NA` to
#'   signal a non-evaluable parameter (an extreme `theta` that breaks
#'   the local solver, for example); the master will propagate `NA`
#'   back to the optimizer.
#' @param state an environment for mutable bookkeeping (next site,
#'   public key, master back-reference). Default: a fresh empty env.
#' @export
Site <- new_class(
    "Site",
    package = "homomorpheR",
    properties = list(
        name     = class_character,
        data     = class_any,
        local_fn = class_any,
        state    = class_any
    )
)

#' Abstract master class
#'
#' Common base for [CKKSMaster] and [ThresholdMaster] (and the frozen
#' legacy [PaillierMaster]). Concrete masters carry whatever keys and
#' context their cryptographic backend needs; the protocol body in
#' [master_aggregate()] uses [master_encrypt()] and [master_decrypt()]
#' generics that dispatch on the concrete master class, so the same
#' protocol runs over any backend.
#'
#' @param name short identifier shown in printed output.
#' @param state an environment for mutable bookkeeping.
#' @export
Master <- new_class(
    "Master",
    abstract = TRUE,
    package = "homomorpheR",
    properties = list(
        name  = class_character,
        state = class_any
    )
)

#' CKKS-backed master
#'
#' A [Master] that drives the protocol over `openfhe.R`'s CKKS encryption.
#' CKKS handles real-valued arithmetic natively, so no `den`
#' denominator is needed. Constructed by [make_ckks_master()].
#'
#' @param name short identifier shown in printed output.
#' @param crypto_context an `openfhe.R` `CryptoContext` configured for
#'   CKKS.
#' @param keypair an `openfhe.R` `KeyPair`.
#' @param state an environment for mutable bookkeeping.
#' @export
CKKSMaster <- new_class(
    "CKKSMaster",
    parent  = Master,
    package = "homomorpheR",
    properties = list(
        crypto_context = class_any,
        keypair        = class_any
    )
)

#' Threshold-CKKS master (n-of-n key generation)
#'
#' A [Master] that drives the protocol over `openfhe.R` with
#' threshold key generation, under whichever scheme the supplied
#' crypto context was built for (CKKS for real-valued work, BFV or
#' BGV for exact integer work). There is no single secret key: each
#' site holds a secret share `sk_i`, and the joint public key
#' `pk_{1..n}` is built by chaining `multiparty_key_gen()` across
#' sites. Encryption goes under `joint_pubkey`. Decryption requires
#' all `n` sites to contribute partial decryptions, which the master
#' then fuses.
#'
#' Constructed by [make_threshold_master()].
#'
#' @param name short identifier.
#' @param crypto_context an `openfhe.R` `CryptoContext` with the
#'   `MULTIPARTY` feature enabled.
#' @param joint_pubkey the joint public key produced by chaining
#'   `multiparty_key_gen()` across the sites.
#' @param secret_keys a list of per-site secret keys, in site order
#'   (the first is the lead site whose `sk` started the chain).
#' @param state an environment for mutable bookkeeping.
#' @export
ThresholdMaster <- new_class(
    "ThresholdMaster",
    parent  = Master,
    package = "homomorpheR",
    properties = list(
        crypto_context = class_any,
        joint_pubkey   = class_any,
        secret_keys    = class_any
    )
)

# ---- Constructors ---------------------------------------------------------

#' Construct a [Site]
#' @inheritParams Site
#' @return a [Site].
#' @export
make_site <- function(name, data, local_fn) {
    Site(name = name, data = data, local_fn = local_fn,
         state = new.env(parent = emptyenv()))
}

#' Construct a CKKS-backed master
#'
#' @inheritParams CKKSMaster
#' @return a [CKKSMaster].
#' @export
make_ckks_master <- function(name, crypto_context, keypair) {
    m <- CKKSMaster(name = name, crypto_context = crypto_context,
                    keypair = keypair,
                    state   = new.env(parent = emptyenv()))
    m@state$pubkey  <- keypair@public
    m@state$privkey <- keypair@secret
    m
}

#' Construct a threshold-CKKS master and the per-site secret shares
#'
#' Runs the chained `multiparty_key_gen()` setup across `n_sites`
#' sites. The first site generates a fresh keypair `(pk_1, sk_1)`;
#' each subsequent site `i` calls `multiparty_key_gen(cc, pk_{i-1})`
#' to produce `(pk_{1..i}, sk_i)`. The final `pk_{1..n}` is the
#' joint public key under which everything is encrypted. Each site
#' keeps its own `sk_i`; no single party holds the joint secret.
#'
#' Decryption is n-of-n: each site contributes a partial
#' decryption (`multiparty_decrypt_lead` for the first, then
#' `multiparty_decrypt_main` for the rest), and the master fuses
#' them via `multiparty_decrypt_fusion`. This happens automatically
#' inside [master_decrypt()] when called on a `ThresholdMaster`.
#'
#' @param name short identifier.
#' @param crypto_context an `openfhe.R` `CryptoContext` (CKKS, BFV,
#'   or BGV) *with* the `MULTIPARTY` feature enabled. Pass
#'   `features = c(Feature$MULTIPARTY)` to `fhe_context()`. The
#'   scheme is read back from the context, so the same master drives
#'   the protocol over real-valued (CKKS) or exact-integer (BFV/BGV)
#'   arithmetic without further configuration.
#' @param n_sites number of participating sites (>= 2).
#' @return a [ThresholdMaster].
#' @export
make_threshold_master <- function(name, crypto_context, n_sites) {
    if (n_sites < 2)
        cli_abort("Threshold key generation requires at least two sites.")

    sks <- vector("list", n_sites)
    pks <- vector("list", n_sites)

    kp1 <- openfhe.R::key_gen(crypto_context)
    sks[[1]] <- kp1@secret
    pks[[1]] <- kp1@public

    for (i in 2:n_sites) {
        kpi <- openfhe.R::multiparty_key_gen(crypto_context, pks[[i - 1]])
        sks[[i]] <- kpi@secret
        pks[[i]] <- kpi@public
    }

    joint_pk <- pks[[n_sites]]

    m <- ThresholdMaster(
        name           = name,
        crypto_context = crypto_context,
        joint_pubkey   = joint_pk,
        secret_keys    = sks,
        state          = new.env(parent = emptyenv()))
    m@state$pubkey <- joint_pk
    m
}

# ---- Generics -------------------------------------------------------------

#' Distribute the public key from the master to a downstream actor
#' @param obj a [Site] (or legacy [NCParty]) to receive the key.
#' @param ... method-specific arguments. The methods take a single
#'   public key `pubkey` of the master's backend type.
#' @export
set_public_key <- new_generic("set_public_key", "obj")

#' Encrypt a real value for a master's protocol
#'
#' Dispatches on the master's class so the same protocol body works
#' over different cryptographic backends.
#'
#' @param master a [Master].
#' @param ... method-specific arguments. Both backends take a single
#'   real-valued `value`.
#' @return the encrypted value (an `openfhe.R` `Ciphertext` for
#'   [CKKSMaster] / [ThresholdMaster]; a [PaillierEncryptedReal] for
#'   the legacy [PaillierMaster]).
#' @export
master_encrypt <- new_generic("master_encrypt", "master")

#' Decrypt the master's protocol result back to a scalar real
#'
#' Dispatches on the master's class.
#'
#' @param master a [Master].
#' @param ... method-specific arguments. Both backends take a single
#'   `ciphertext` of the appropriate type.
#' @return a single numeric value.
#' @export
master_decrypt <- new_generic("master_decrypt", "master")

# ---- Methods --------------------------------------------------------------

method(set_public_key, Site)    <- function(obj, pubkey) {
    obj@state$pubkey <- pubkey; invisible(obj)
}
# ---- Backend-specific master_encrypt / master_decrypt ---------------------

method(master_encrypt, CKKSMaster) <- function(master, value) {
    cc <- master@crypto_context
    pt <- .packed_codec(cc)$encode(value)
    openfhe.R::encrypt(master@keypair@public, pt, cc = cc)
}
method(master_decrypt, CKKSMaster) <- function(master, ciphertext, len = 1L) {
    cc <- master@crypto_context
    pt <- openfhe.R::decrypt(ciphertext, master@keypair@secret, cc = cc)
    openfhe.R::set_length(pt, as.integer(len))
    vals <- .packed_codec(cc)$decode(pt)
    if (len == 1L) vals[1] else vals[seq_len(len)]
}

method(master_encrypt, ThresholdMaster) <- function(master, value) {
    cc <- master@crypto_context
    pt <- .packed_codec(cc)$encode(value)
    openfhe.R::encrypt(master@joint_pubkey, pt, cc = cc)
}
method(master_decrypt, ThresholdMaster) <- function(master, ciphertext, len = 1L) {
    cc  <- master@crypto_context
    sks <- master@secret_keys
    n   <- length(sks)

    ## Each site computes a partial decryption of `ciphertext`. In a
    ## real deployment the partials travel from sites to the master
    ## over the network; here they are ordinary R objects.
    partials      <- vector("list", n)
    partials[[1]] <- openfhe.R::multiparty_decrypt_lead(cc, sks[[1]], ciphertext)
    for (i in 2:n) {
        partials[[i]] <- openfhe.R::multiparty_decrypt_main(cc, sks[[i]], ciphertext)
    }

    ## Fuse to recover the plaintext sum. n-of-n: any subset of the
    ## partials would not suffice.
    pt <- do.call(openfhe.R::multiparty_decrypt_fusion, c(list(cc), partials))
    openfhe.R::set_length(pt, as.integer(len))
    vals <- .packed_codec(cc)$decode(pt)
    if (len == 1L) vals[1] else vals[seq_len(len)]
}

# ---- Helpers --------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

## Plaintext codec matching a crypto context's scheme.
##
## The protocol body (encrypt local summaries, add homomorphically,
## decrypt the total) is identical across schemes; only the
## plaintext encode/decode pair differs. CKKS carries reals and its
## decode returns approximate doubles; BFV and BGV carry exact
## integers. Detecting the scheme from the context keeps it the
## single source of truth -- a master never has to be told which
## scheme its context was built for.
.packed_codec <- function(cc) {
    if (openfhe.R::get_scheme_id(cc) == openfhe.R::SchemeId$CKKSRNS_SCHEME) {
        list(
            encode = function(value) openfhe.R::make_ckks_packed_plaintext(cc, value),
            decode = function(pt)    openfhe.R::get_real_packed_value(pt)
        )
    } else {
        list(
            encode = function(value) openfhe.R::make_packed_plaintext(cc, as.integer(value)),
            decode = function(pt)    openfhe.R::get_packed_value(pt)
        )
    }
}

#' Construct a worker (alias for [make_site()])
#'
#' Provided for naming clarity in master/worker protocols. Returns a
#' [Site] with identical semantics to [make_site()].
#'
#' @inheritParams make_site
#' @return a [Site].
#' @export
make_worker <- function(name, data, local_fn) make_site(name, data, local_fn)

#' Wire a master to a flat list of workers
#'
#' Stashes the workers in the master's state and broadcasts the
#' master's public key to each worker. After this call,
#' [master_aggregate()] can drive an iteration of the protocol.
#'
#' Use this for the realistic master/worker (star) topology that
#' distcomp- and DataSHIELD-style federated analyses follow. For the
#' legacy Paillier round-robin idiom, use [round_robin_chain()] instead.
#'
#' @param master a [Master].
#' @param workers a list of worker [Site]s.
#' @return the master, invisibly.
#' @export
set_workers <- function(master, workers) {
    if (length(workers) < 1)
        cli_abort("Need at least one worker.")
    master@state$workers <- workers
    for (w in workers) {
        set_public_key(w, master@state$pubkey)
        w@state$master <- master
    }
    invisible(master)
}

#' Run one round of the master/worker protocol
#'
#' Backend-agnostic via the [master_encrypt()] / [master_decrypt()]
#' generics: works over [CKKSMaster], [ThresholdMaster], and the
#' legacy [PaillierMaster].
#'
#' The master broadcasts `theta` to each worker. Each worker computes
#' its local summary `local_fn(data, theta)` and the result is
#' encrypted under the master's public key. The master sums the
#' encrypted contributions homomorphically and decrypts the total.
#'
#' This is the topology that mirrors how distcomp, DataSHIELD, and
#' similar federated-analysis frameworks actually deploy: a flat
#' fan-out / fan-in. With a single-decrypter master, the master could
#' in principle decrypt individual contributions; the cryptographic
#' guarantee strengthens when paired with threshold key generation
#' (no single party holds the secret key).
#'
#' If any worker's `local_fn` returns `NA`, this function returns
#' `NA_real_`.
#'
#' @param master a [Master], wired to workers via [set_workers()].
#' @param theta the current parameter value (passed through to each
#'   worker's `local_fn`).
#' @return the aggregated value, or `NA_real_` on failure.
#' @export
master_aggregate <- function(master, theta) {
    workers <- master@state$workers
    if (is.null(workers) || length(workers) == 0)
        cli_abort("Master has no workers; call {.fun set_workers} first.")

    encrypted_locals <- vector("list", length(workers))
    for (i in seq_along(workers)) {
        w <- workers[[i]]
        local_value <- w@local_fn(w@data, theta)
        if (length(local_value) == 1 && is.na(local_value)) return(NA_real_)
        encrypted_locals[[i]] <- master_encrypt(master, local_value)
    }
    total <- Reduce(`+`, encrypted_locals)
    master_decrypt(master, total)
}

# ---- Print methods --------------------------------------------------------

method(print, Site) <- function(x, ...) {
    cat("<Site> ", x@name, "\n", sep = "")
    invisible(x)
}
method(print, Master) <- function(x, ...) {
    cat("<", sub("^homomorpheR::", "", class(x)[[1L]]), "> ", x@name,
        "\n", sep = "")
    invisible(x)
}
