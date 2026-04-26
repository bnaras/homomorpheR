#' @importFrom S7 new_class new_generic new_object method method<- class_any class_character class_integer S7_object S7_inherits
#' @importFrom stats runif
#' @importFrom openfhe make_ckks_packed_plaintext get_real_packed_value
NULL

## Site / Master / NCParty actor classes.
##
## These are the building blocks for multi-site protocols. A `Site`
## holds local data and a `local_fn(data, theta)` that computes a
## site-level summary; a `Master` owns the keys and orchestrates the
## protocol; an `NCParty` sits between master and sites in the
## non-cooperating-parties topology.
##
## All three carry an environment-backed `state` property for mutable
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
#' Common base for [PaillierMaster] and [CKKSMaster]. Concrete masters
#' carry whatever keys and context their cryptographic backend needs;
#' the protocol body in [run_round_robin()] uses [master_encrypt()]
#' and [master_decrypt()] generics that dispatch on the concrete
#' master class, so the same protocol runs over either backend.
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

#' Paillier-backed master
#'
#' A [Master] that drives the protocol over Paillier additive
#' encryption. Constructed by [make_master()].
#'
#' @param name short identifier shown in printed output.
#' @param keypair a [PaillierKeyPair].
#' @param den a [gmp::bigq] denominator used to scale fractional parts
#'   when encrypting real numbers via [encrypt_real()].
#' @param state an environment for mutable bookkeeping.
#' @export
PaillierMaster <- new_class(
    "PaillierMaster",
    parent  = Master,
    package = "homomorpheR",
    properties = list(
        keypair = PaillierKeyPair,
        den     = class_any
    )
)

#' CKKS-backed master
#'
#' A [Master] that drives the protocol over `openfhe`'s CKKS encryption.
#' CKKS handles real-valued arithmetic natively, so no `den`
#' denominator is needed. Constructed by [make_ckks_master()].
#'
#' @param name short identifier shown in printed output.
#' @param crypto_context an `openfhe` `CryptoContext` configured for
#'   CKKS.
#' @param keypair an `openfhe` `KeyPair`.
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

#' A non-cooperating party
#'
#' Sits between the master and the sites in the non-cooperating-parties
#' topology. Two NCPs receive *additive shares* of each site's
#' contribution; each NCP sums its share across sites and ships the
#' result to the master, who combines the two NCP totals and decrypts.
#' No single party — neither master nor an NCP — sees an individual
#' site's contribution. Use [make_ncparty()] to construct.
#'
#' @param name short identifier shown in printed output.
#' @param number which share this NCP receives, `1` or `2`.
#' @param state an environment for mutable bookkeeping (the list of
#'   sites it manages, public key). Default: a fresh empty env.
#' @export
NCParty <- new_class(
    "NCParty",
    package = "homomorpheR",
    properties = list(
        name   = class_character,
        number = class_integer,
        state  = class_any
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

#' Construct a Paillier-backed master
#'
#' @inheritParams PaillierMaster
#' @return a [PaillierMaster].
#' @export
make_master <- function(name, keypair, den = gmp::as.bigq(2)^256) {
    m <- PaillierMaster(name = name, keypair = keypair, den = den,
                        state = new.env(parent = emptyenv()))
    m@state$pubkey  <- keypair@pubkey
    m@state$privkey <- get_private_key(keypair)
    m
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

#' Construct an [NCParty]
#' @inheritParams NCParty
#' @return an [NCParty].
#' @export
make_ncparty <- function(name, number) {
    NCParty(name = name, number = as.integer(number),
            state = new.env(parent = emptyenv()))
}

# ---- Generics -------------------------------------------------------------

#' Wire one site's `next_site` to another
#' @param obj a [Site] or [Master].
#' @param ... method-specific arguments. The Site/Master methods take
#'   a single `next_site`.
#' @export
set_next_site <- new_generic("set_next_site", "obj")

#' Distribute the public key from the master to a downstream actor
#' @param obj an [NCParty] or [Site] to receive the key.
#' @param ... method-specific arguments. The Site/NCParty methods
#'   take a single [PaillierPublicKey] `pubkey`.
#' @export
set_public_key <- new_generic("set_public_key", "obj")

#' Add a site to a non-cooperating party
#' @param ncp an [NCParty].
#' @param ... method-specific arguments. The NCParty method takes a
#'   single [Site].
#' @export
add_site <- new_generic("add_site", "ncp")

#' Internal generic: forward the running encrypted total along the chain
#' @param obj a [Site] or [Master].
#' @param ... method-specific arguments: `theta` (the current parameter
#'   value), `running` (the running encrypted total), and `master`
#'   (so workers can signal failure back to the master).
#' @keywords internal
#' @export
add_local_and_forward <- new_generic("add_local_and_forward", "obj")

#' Encrypt a real value for a master's protocol
#'
#' Dispatches on the master's class so the same protocol body works
#' over different cryptographic backends.
#'
#' @param master a [Master].
#' @param ... method-specific arguments. Both backends take a single
#'   real-valued `value`.
#' @return the encrypted value (a [PaillierEncryptedReal] for
#'   [PaillierMaster]; an `openfhe` `Ciphertext` for [CKKSMaster]).
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

method(set_next_site, Site)   <- function(obj, next_site) {
    obj@state$next_site <- next_site; invisible(obj)
}
method(set_next_site, Master) <- function(obj, next_site) {
    obj@state$next_site <- next_site; invisible(obj)
}

method(set_public_key, Site)    <- function(obj, pubkey) {
    obj@state$pubkey <- pubkey; invisible(obj)
}
method(set_public_key, NCParty) <- function(obj, pubkey) {
    obj@state$pubkey <- pubkey
    for (s in obj@state$sites %||% list()) set_public_key(s, pubkey)
    invisible(obj)
}

method(add_site, NCParty) <- function(ncp, site) {
    ncp@state$sites <- c(ncp@state$sites %||% list(), list(site))
    invisible(ncp)
}

method(add_local_and_forward, Site) <- function(obj, theta, running, master) {
    if (isTRUE(master@state$failed)) return(invisible(NULL))
    local_value <- obj@local_fn(obj@data, theta)
    if (length(local_value) == 1 && is.na(local_value)) {
        master@state$failed <- TRUE
        return(invisible(NULL))
    }
    enc_local <- master_encrypt(master, local_value)
    add_local_and_forward(obj@state$next_site, theta, running + enc_local, master)
}

method(add_local_and_forward, Master) <- function(obj, theta, running, master) {
    obj@state$result <- running
    invisible(NULL)
}

# ---- Backend-specific master_encrypt / master_decrypt ---------------------

method(master_encrypt, PaillierMaster) <- function(master, value) {
    encrypt_real(master@keypair@pubkey, value, master@den)
}
method(master_decrypt, PaillierMaster) <- function(master, ciphertext) {
    decrypt(get_private_key(master@keypair), ciphertext)
}

method(master_encrypt, CKKSMaster) <- function(master, value) {
    cc <- master@crypto_context
    pt <- openfhe::make_ckks_packed_plaintext(cc, value)
    openfhe::encrypt(master@keypair@public, pt, cc = cc)
}
method(master_decrypt, CKKSMaster) <- function(master, ciphertext) {
    cc <- master@crypto_context
    pt <- openfhe::decrypt(ciphertext, master@keypair@secret, cc = cc)
    openfhe::set_length(pt, 1L)
    openfhe::get_real_packed_value(pt)[1]
}

# ---- Helpers --------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Wire a master and a list of sites into a round-robin chain
#'
#' Sets `master -> sites[[1]] -> sites[[2]] -> ... -> sites[[n]] -> master`
#' and broadcasts the master's public key to every site. After this
#' call, [run_round_robin()] can drive an iteration of the protocol.
#'
#' @param master a [Master].
#' @param sites a list of [Site]s.
#' @return the master, invisibly.
#' @export
round_robin_chain <- function(master, sites) {
    n <- length(sites)
    if (n < 1) cli_abort("Need at least one site for a round-robin chain.")
    set_next_site(master, sites[[1]])
    if (n > 1) {
        for (i in seq_len(n - 1)) set_next_site(sites[[i]], sites[[i + 1]])
    }
    set_next_site(sites[[n]], master)
    for (s in sites) set_public_key(s, master@state$pubkey)
    invisible(master)
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
#' [run_master_worker()] can drive an iteration of the protocol.
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
#' Backend-agnostic: works for both [PaillierMaster] and [CKKSMaster]
#' via the [master_encrypt()] / [master_decrypt()] generics.
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
run_master_worker <- function(master, theta) {
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

#' Run one round of the round-robin protocol
#'
#' Backend-agnostic: works for both [PaillierMaster] and [CKKSMaster]
#' via the [master_encrypt()] / [master_decrypt()] generics.
#'
#' The master generates a random real offset, encrypts it under its
#' public key, and sends it around the chain. Each worker site adds its
#' encrypted local summary to the running total and forwards. On
#' return, the master decrypts the running total, subtracts the offset
#' in the clear, and returns the resulting scalar.
#'
#' If any worker's `local_fn` returns `NA`, the chain stops and this
#' function returns `NA_real_`.
#'
#' @param master a [Master], wired to a chain via [round_robin_chain()].
#' @param theta the current parameter value (passed through to each
#'   worker's `local_fn`).
#' @return the aggregated value, or `NA_real_` on failure.
#' @export
run_round_robin <- function(master, theta) {
    master@state$failed <- FALSE
    offset     <- runif(1, -1e6, 1e6)
    enc_offset <- master_encrypt(master, offset)
    add_local_and_forward(master@state$next_site, theta, enc_offset, master)
    if (isTRUE(master@state$failed)) return(NA_real_)
    master_decrypt(master, master@state$result) - offset
}

# ---- Print methods --------------------------------------------------------

method(print, Site) <- function(x, ...) {
    cat("<Site> ", x@name, "\n", sep = "")
    invisible(x)
}
method(print, Master) <- function(x, ...) {
    cat("<Master> ", x@name, " (", x@keypair@pubkey@bits, "-bit key)\n", sep = "")
    invisible(x)
}
method(print, NCParty) <- function(x, ...) {
    cat("<NCParty> ", x@name, " (share ", x@number, ")\n", sep = "")
    invisible(x)
}
