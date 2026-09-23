## Master/worker protocol — the realistic distributed-stats topology.
## Same agreement-with-aggregated-cleartext and NA-propagation tests as
## the round-robin runner.

library(homomorpheR)

keys <- paillier_keypair(512)
local_nll <- function(data, lambda) {
    -sum(stats::dpois(data, lambda, log = TRUE))
}

## ---- Encrypted master/worker matches aggregated cleartext (Paillier) ---------
master <- make_master("Master", keys)
w1 <- make_worker("S1", c(2, 3), local_nll)
w2 <- make_worker("S2", c(4, 5), local_nll)
set_workers(master, list(w1, w2))

direct    <- -sum(stats::dpois(c(2, 3, 4, 5), 3.5, log = TRUE))
encrypted <- master_aggregate(master, 3.5)
expect_true(abs(encrypted - direct) < 1e-9)

## ---- NA propagation -----------------------------------------------------
master2 <- make_master("M2", keys)
w_bad <- make_worker("Sbad", c(2, 3),
                     function(d, lambda) if (lambda < 0.01) NA else
                         -sum(stats::dpois(d, lambda, log = TRUE)))
set_workers(master2, list(w_bad))
expect_true(is.na(master_aggregate(master2, 0.001)))
expect_true(!is.na(master_aggregate(master2, 1.0)))

## ---- mle() converges through the encrypted master/worker channel -------
master3 <- make_master("M3", keys)
w_a <- make_worker("S1", c(2, 3), local_nll)
w_b <- make_worker("S2", c(4, 5), local_nll)
set_workers(master3, list(w_a, w_b))
fit <- stats4::mle(function(lambda) master_aggregate(master3, lambda),
                   start = list(lambda = 5))
expect_true(abs(stats4::coef(fit) - mean(c(2, 3, 4, 5))) < 1e-3)

## ---- CKKS-backed master/worker (skip if openfhe.R not installed) ---------
if (requireNamespace("openfhe.R", quietly = TRUE)) {
    cc <- openfhe.R::fhe_context("CKKS",
                               multiplicative_depth = 1L,
                               scaling_mod_size     = 50L,
                               batch_size           = 8L)
    ckks_keys <- openfhe.R::key_gen(cc)
    cmaster <- make_ckks_master("CMaster", crypto_context = cc, keypair = ckks_keys)
    cw1 <- make_worker("S1", c(2, 3), local_nll)
    cw2 <- make_worker("S2", c(4, 5), local_nll)
    set_workers(cmaster, list(cw1, cw2))
    encrypted_ckks <- master_aggregate(cmaster, 3.5)
    expect_true(abs(encrypted_ckks - direct) < 1e-3)   # CKKS tolerance
}

## ---- Empty worker list errors -----------------------------------------
master4 <- make_master("M4", keys)
expect_error(set_workers(master4, list()),
             pattern = "at least one worker")
expect_error(master_aggregate(master4, 1.0),
             pattern = "no workers")

## ---- Site is abstract; contribute() returns ciphertext, not cleartext ---
expect_error(Site(name = "S", state = new.env()), pattern = "abstract")
expect_error(RemoteSite(name = "R", state = new.env()), pattern = "abstract")

master5 <- make_master("M5", keys)
w5 <- make_worker("S1", c(2, 3), local_nll)
set_workers(master5, list(w5))

## The value leaving the site is encrypted. If it were a plain number
## the aggregator would see this site's individual contribution, which
## is the leak the protocol exists to prevent.
ct <- contribute(w5, 3.5)
expect_false(is.numeric(ct))
expect_true(S7::S7_inherits(ct, PaillierEncryptedReal))

## An unwired site cannot encrypt, and says so rather than leaking.
expect_error(contribute(make_worker("Loose", c(1, 2), local_nll), 1.0),
             pattern = "no public parameters")

## ---- A user-defined RemoteSite participates unchanged -------------------
## The package ships no transport, so the test supplies one. Here the
## "far side" is a closure; the runner cannot tell the difference.
FakeRemote <- S7::new_class("FakeRemote", parent = RemoteSite,
                            properties = list(rows = S7::class_any))
## Setup is a message the far end must receive, so a RemoteSite has to
## implement it; the base method refuses rather than configure this
## proxy and leave the endpoint untold. Here the "far end" is the same
## process, so provisioning is a local store.
S7::method(set_public_params, FakeRemote) <- function(site, params) {
    site@state$params <- params
    invisible(site)
}
S7::method(contribute, FakeRemote) <- function(site, theta) {
    value <- -sum(stats::dpois(site@rows, theta, log = TRUE))
    ## Encrypts with the bundle it was given, before the value would
    ## cross a wire. The site is the only argument it needs.
    encrypt(site, value)
}

master6 <- make_master("M6", keys)
set_workers(master6, list(make_worker("S1", c(2, 3), local_nll),
                          FakeRemote(name = "Far", rows = c(6, 7),
                                     state = new.env(parent = emptyenv()))))
direct6 <- -sum(stats::dpois(c(2, 3, 6, 7), 3.5, log = TRUE))
expect_true(abs(master_aggregate(master6, 3.5) - direct6) < 1e-9)

## ---- Unreachable is not the same event as non-evaluable -----------------
Dead <- S7::new_class("Dead", parent = RemoteSite)
## Reachable at setup, gone by the time the round runs -- which is the
## realistic shape of the failure, and keeps this test about the
## NA/unreachable distinction rather than about provisioning.
S7::method(set_public_params, Dead) <- function(site, params) {
    site@state$params <- params
    invisible(site)
}
S7::method(contribute, Dead) <- function(site, theta)
    site_unavailable("connection refused", site = site)

master7 <- make_master("M7", keys)
set_workers(master7, list(make_worker("S1", c(2, 3), local_nll),
                          Dead(name = "Offline",
                               state = new.env(parent = emptyenv()))))

## NA would be wrong here: it would tell the optimizer to try another
## theta, which does nothing about an offline host. The round aborts,
## and the message names the site.
expect_error(master_aggregate(master7, 3.5),
             pattern = "Offline.*connection refused")
expect_error(master_aggregate(master7, 3.5),
             class = "homomorpheR_site_unavailable")

## Silently dropping the dead site would change the objective; make sure
## we did not accidentally do that.
expect_false(isTRUE(tryCatch(is.numeric(master_aggregate(master7, 3.5)),
                             error = function(e) FALSE)))
