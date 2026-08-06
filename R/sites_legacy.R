## FROZEN (Paillier-era legacy) -- do not extend.
##
## Everything in this file exists to support the archived Paillier
## vignettes (paillier-archive/) and the API that distcomp pins
## (PaillierKeyPair, PaillierPublicKey, random.bigz live in
## paillier.R / homomorpheR.R; the actors here are their protocol
## layer). Frozen as of 2026-08-05: no new code should call into this
## file, and it is slated for un-export and eventual removal once a
## revamped distcomp (dropping its homomorpheR imports) reaches CRAN
## ahead of the next homomorpheR release. The supported actors live
## in sites.R.
##
## Contents: the Paillier-backed master; the non-cooperating-parties
## (NCParty) topology; the round-robin chain protocol. The round-robin
## runner is scheme-agnostic (test_ckks_master.R exercises it over a
## CKKS master) but no current vignette uses it -- the master/worker
## star topology in sites.R is the supported pattern.

#' @importFrom stats runif
NULL

# ---- Paillier-backed master -----------------------------------------------

#' Paillier-backed master
#'
#' A [Master] that drives the protocol over Paillier additive
#' encryption. Constructed by [make_master()]. Part of the frozen
#' Paillier-era legacy surface; new work should use
#' [make_ckks_master()] or [make_threshold_master()].
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

#' Construct a Paillier-backed master
#'
#' Part of the frozen Paillier-era legacy surface.
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

method(master_encrypt, PaillierMaster) <- function(master, value) {
    encrypt_real(master@keypair@pubkey, value, master@den)
}
method(master_decrypt, PaillierMaster) <- function(master, ciphertext) {
    decrypt(get_private_key(master@keypair), ciphertext)
}

method(print, PaillierMaster) <- function(x, ...) {
    cat("<PaillierMaster> ", x@name, " (", x@keypair@pubkey@bits,
        "-bit key)\n", sep = "")
    invisible(x)
}

# ---- Non-cooperating-parties topology -------------------------------------

#' A non-cooperating party
#'
#' Sits between the master and the sites in the non-cooperating-parties
#' topology. Two NCPs receive *additive shares* of each site's
#' contribution; each NCP sums its share across sites and ships the
#' result to the master, who combines the two NCP totals and decrypts.
#' No single party — neither master nor an NCP — sees an individual
#' site's contribution. Use [make_ncparty()] to construct.
#'
#' Part of the frozen Paillier-era legacy surface: the NCP masking
#' construction compensated for Paillier's single decryption key, a
#' role that threshold key generation ([make_threshold_master()]) now
#' fills without extra parties.
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

#' Construct an [NCParty]
#'
#' Part of the frozen Paillier-era legacy surface.
#'
#' @inheritParams NCParty
#' @return an [NCParty].
#' @export
make_ncparty <- function(name, number) {
    NCParty(name = name, number = as.integer(number),
            state = new.env(parent = emptyenv()))
}

#' Add a site to a non-cooperating party
#'
#' Part of the frozen Paillier-era legacy surface.
#'
#' @param ncp an [NCParty].
#' @param ... method-specific arguments. The NCParty method takes a
#'   single [Site].
#' @export
add_site <- new_generic("add_site", "ncp")

method(add_site, NCParty) <- function(ncp, site) {
    ncp@state$sites <- c(ncp@state$sites %||% list(), list(site))
    invisible(ncp)
}

method(set_public_key, NCParty) <- function(obj, pubkey) {
    obj@state$pubkey <- pubkey
    for (s in obj@state$sites %||% list()) set_public_key(s, pubkey)
    invisible(obj)
}

method(print, NCParty) <- function(x, ...) {
    cat("<NCParty> ", x@name, " (share ", x@number, ")\n", sep = "")
    invisible(x)
}

# ---- Round-robin chain protocol -------------------------------------------

#' Wire one site's `next_site` to another
#'
#' Part of the frozen Paillier-era legacy surface (round-robin chain
#' wiring); the supported topology is [set_workers()] +
#' [master_aggregate()].
#'
#' @param obj a [Site] or [Master].
#' @param ... method-specific arguments. The Site/Master methods take
#'   a single `next_site`.
#' @export
set_next_site <- new_generic("set_next_site", "obj")

method(set_next_site, Site)   <- function(obj, next_site) {
    obj@state$next_site <- next_site; invisible(obj)
}
method(set_next_site, Master) <- function(obj, next_site) {
    obj@state$next_site <- next_site; invisible(obj)
}

#' Internal generic: forward the running encrypted total along the chain
#'
#' Part of the frozen Paillier-era legacy surface.
#'
#' @param obj a [Site] or [Master].
#' @param ... method-specific arguments: `theta` (the current parameter
#'   value), `running` (the running encrypted total), and `master`
#'   (so workers can signal failure back to the master).
#' @keywords internal
#' @export
add_local_and_forward <- new_generic("add_local_and_forward", "obj")

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

#' Wire a master and a list of sites into a round-robin chain
#'
#' Sets `master -> sites[[1]] -> sites[[2]] -> ... -> sites[[n]] -> master`
#' and broadcasts the master's public key to every site. After this
#' call, [run_round_robin()] can drive an iteration of the protocol.
#'
#' Part of the frozen Paillier-era legacy surface; the supported
#' topology is [set_workers()] + [master_aggregate()].
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

#' Run one round of the round-robin protocol
#'
#' Backend-agnostic via the [master_encrypt()] / [master_decrypt()]
#' generics, but part of the frozen Paillier-era legacy surface: the
#' random-offset chain idiom compensated for Paillier-era trust
#' assumptions. The supported pattern is [master_aggregate()].
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
