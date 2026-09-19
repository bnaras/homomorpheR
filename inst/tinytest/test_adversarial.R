## Adversarial tests: the failure modes, not the happy path.
##
## Every assertion here corresponds to something that used to succeed
## silently and return a wrong number, or that used to look configured
## while it was not. The arithmetic tests elsewhere pass either way,
## which is exactly why these are separate: a protocol bug that does
## not change the answer on a well-behaved run is invisible to them.

if (!requireNamespace("openfhe.R", quietly = TRUE))
    exit_file("openfhe.R not installed")

library(homomorpheR)

nll <- function(d, lambda) -sum(stats::dpois(d, lambda, log = TRUE))

cc <- openfhe.R::fhe_context(scheme               = "CKKS",
                             multiplicative_depth = 1L,
                             scaling_mod_size     = 50L,
                             batch_size           = 8L)
ccm <- openfhe.R::fhe_context(scheme               = "CKKS",
                              multiplicative_depth = 1L,
                              scaling_mod_size     = 50L,
                              batch_size           = 8L,
                              features = c(openfhe.R::Feature$MULTIPARTY))
ccb <- openfhe.R::fhe_context(scheme               = "BFV",
                              multiplicative_depth = 1L,
                              plaintext_modulus    = 65537L,
                              features = c(openfhe.R::Feature$MULTIPARTY))

## ---- Public parameters carry no secret material -------------------------
## Structural, not a promise in prose: there is no property for a key
## share to occupy.
keys <- openfhe.R::key_gen(cc = cc)
m    <- make_ckks_master(name = "M", crypto_context = cc, keypair = keys)
w1   <- make_worker("S1", c(2, 3), nll)
w2   <- make_worker("S2", c(4, 5), nll)
set_workers(master = m, workers = list(w1, w2))

p <- site_params(w1)
expect_true(S7::S7_inherits(p, PublicParams))
expect_equal(sort(names(S7::props(p))), c("cc", "pk"))
expect_false(any(grepl("sk|secret|private", names(S7::props(p)))))

## A single-decrypter master keeps its secret key in its keypair and
## nowhere else -- in particular not loose in the state environment,
## where it was previously duplicated and never read.
expect_equal(ls(m@state), "workers")

## ---- A site cannot be silently re-wired to a second master --------------
## The site is shared by reference, so the first master would go on
## querying a worker that now encrypts under someone else's key.
keys_b <- openfhe.R::key_gen(cc = cc)
m_b    <- make_ckks_master(name = "B", crypto_context = cc, keypair = keys_b)
expect_error(set_workers(master = m_b, workers = list(w1)),
             pattern = "already holds different public parameters")

## Re-wiring to the same master is the same message twice; allow it.
expect_silent(set_workers(master = m, workers = list(w1, w2)))

## ---- An unconfigured site says so rather than encrypting ----------------
loose <- make_worker("Loose", c(1, 2), nll)
expect_error(site_params(loose), pattern = "no public parameters")
expect_error(contribute(loose, 1), pattern = "no public parameters")

## ---- Remote setup fails closed ------------------------------------------
## The base RemoteSite refuses all three site-side steps. Writing the
## parameters into the proxy would leave it looking configured while
## the far endpoint had never been told anything.
Far <- S7::new_class("Far", parent = RemoteSite)
far <- Far(name = "Far", state = new.env(parent = emptyenv()))

expect_error(set_public_params(far, p),  pattern = "set_public_params")
expect_error(keygen_round(far, ccm),     pattern = "keygen_round")
expect_error(partial_decrypt(far, encrypt_under(p, 1)), pattern = "partial_decrypt")

m_f <- make_ckks_master(name = "F", crypto_context = cc, keypair = keys)
expect_error(set_workers(master = m_f, workers = list(far)),
             pattern = "set_public_params")
## ... and the master is left unwired, not half-wired.
expect_true(is.null(m_f@state$workers))

## A subclass that implements provisioning takes part unchanged.
Near <- S7::new_class("Near", parent = RemoteSite,
                      properties = list(rows = S7::class_any))
S7::method(set_public_params, Near) <- function(site, params) {
    site@state$params <- params; invisible(site)
}
S7::method(contribute, Near) <- function(site, theta)
    encrypt_under(site_params(site), nll(site@rows, theta))

m_n <- make_ckks_master(name = "N", crypto_context = cc, keypair = keys)
set_workers(master  = m_n,
            workers = list(make_worker("S1", c(2, 3), nll),
                           Near(name = "Far", rows = c(6, 7),
                                state = new.env(parent = emptyenv()))))
expect_true(abs(master_aggregate(master = m_n, theta = 3.5) -
                nll(c(2, 3, 6, 7), 3.5)) < 1e-3)

## ---- A cleartext reply never reaches the total --------------------------
## This is the one that returned the *right* answer. A site handed the
## aggregator its individual contribution in the clear, scalar
## addition folded it in, and nothing anywhere noticed.
Leaky <- S7::new_class("Leaky", parent = RemoteSite,
                       properties = list(rows = S7::class_any))
S7::method(set_public_params, Leaky) <- function(site, params) {
    site@state$params <- params; invisible(site)
}
S7::method(contribute, Leaky) <- function(site, theta) nll(site@rows, theta)

m_l <- make_ckks_master(name = "L", crypto_context = cc, keypair = keys)
set_workers(master  = m_l,
            workers = list(make_worker("S1", c(2, 3), nll),
                           Leaky(name = "Leak", rows = c(6, 7),
                                 state = new.env(parent = emptyenv()))))
expect_error(master_aggregate(master = m_l, theta = 3.5),
             class = "homomorpheR_bad_contribution")
expect_error(master_aggregate(master = m_l, theta = 3.5), pattern = "Leak")

## ---- A ciphertext from another key is refused ---------------------------
## Under CKKS this used to surface as a C++ decode error; under BFV and
## BGV as a plausible wrong integer with nothing raised.
p_b <- OpenFHEParams(cc = cc, pk = keys_b@public)
expect_error(master_decrypt(m, encrypt_under(p_b, 1)),
             class = "homomorpheR_key_mismatch")
expect_silent(master_decrypt(m, encrypt_under(p, 1)))

## ---- make_ckks_master is CKKS ------------------------------------------
cc_bfv_single <- openfhe.R::fhe_context(scheme               = "BFV",
                                        multiplicative_depth = 1L,
                                        plaintext_modulus    = 65537L)
expect_error(make_ckks_master(name           = "X",
                              crypto_context = cc_bfv_single,
                              keypair        = openfhe.R::key_gen(cc = cc_bfv_single)),
             pattern = "CKKSRNS_SCHEME")

## ---- Threshold setup rejects a party that is not distinct ---------------
## Listing one site twice is not two parties: its second round
## overwrites the share its first round generated, so the joint key
## depends on a share nobody holds.
d1 <- make_worker("D1", 1, nll)
d2 <- make_worker("D2", 2, nll)
expect_error(make_threshold_master(name = "D", crypto_context = ccm,
                                   sites = list(d1, d2, d1)),
             pattern = "the same party")
## ... and the ceremony left no trace on the sites it had visited.
expect_true(is.null(d1@state$sk))
expect_true(is.null(d2@state$sk))

expect_error(make_threshold_master(name = "D", crypto_context = ccm,
                                   sites = list(make_worker("A", 1, nll),
                                                make_worker("A", 2, nll))),
             pattern = "share the name")

expect_error(make_threshold_master(name = "D", crypto_context = ccm,
                                   sites = list(make_worker("A", 1, nll), 42)),
             pattern = "not a")

## A site already serving a protocol cannot join a second ceremony.
t1 <- make_worker("T1", c(2, 3), nll)
t2 <- make_worker("T2", c(4, 5), nll)
tm <- make_threshold_master(name = "TM", crypto_context = ccm,
                            sites = list(t1, t2))
expect_error(make_threshold_master(name = "TM2", crypto_context = ccm,
                                   sites = list(t1, t2)),
             pattern = "already taking part")

## ---- A ceremony that fails part-way rolls back --------------------------
## Otherwise the sites it had already visited would hold a share
## belonging to a ceremony that never completed, and the check above
## would then refuse them a retry.
r1 <- make_worker("R1", 1, nll)
r2 <- make_worker("R2", 2, nll)
expect_error(make_threshold_master(name = "R", crypto_context = ccm,
                                   sites = list(r1, r2,
                                                Far(name = "RFar",
                                                    state = new.env(parent = emptyenv())))),
             pattern = "keygen_round")
expect_true(is.null(r1@state$sk))
expect_true(is.null(r1@state$cc))
expect_true(is.null(r2@state$sk))
## The same sites work on a retry.
expect_silent(make_threshold_master(name = "R2", crypto_context = ccm,
                                    sites = list(r1, r2)))

## ---- A site refuses to apply its share to a foreign ciphertext ----------
## The site checks for itself, with the joint key it was given at
## setup. It asks no one for anything.
o1 <- make_worker("O1", 1, nll)
o2 <- make_worker("O2", 2, nll)
om <- make_threshold_master(name = "OM", crypto_context = ccm,
                            sites = list(o1, o2))
expect_error(partial_decrypt(o1, encrypt_under(site_params(t1), 1), lead = TRUE),
             class = "homomorpheR_key_mismatch")
expect_silent(partial_decrypt(o1, encrypt_under(site_params(o1), 1), lead = TRUE))

## ---- Exact-integer schemes refuse what they cannot carry ----------------
## BFV/BGV previously coerced with as.integer(): 0.9 became 0, and a
## sum of two of them was reported as 0 with no warning.
bfv_pair <- function(f, ctx = ccb) {
    s <- list(make_worker("X1", NULL, f), make_worker("X2", NULL, f))
    make_threshold_master(name = "B", crypto_context = ctx, sites = s)
}
expect_error(master_aggregate(bfv_pair(function(d, t) 0.9), 1),
             pattern = "not an integer")
expect_error(master_aggregate(bfv_pair(function(d, t) Inf), 1),
             pattern = "cannot represent")
expect_error(master_aggregate(bfv_pair(function(d, t) 4e9), 1),
             pattern = "integer range")
expect_error(master_aggregate(bfv_pair(function(d, t) 40000), 1),
             pattern = "plaintext modulus")
## Integers still go through, exactly.
expect_identical(as.integer(master_aggregate(bfv_pair(function(d, t) 5L), 1)), 10L)

ccg <- openfhe.R::fhe_context(scheme               = "BGV",
                              multiplicative_depth = 1L,
                              plaintext_modulus    = 65537L,
                              features = c(openfhe.R::Feature$MULTIPARTY))
expect_error(master_aggregate(bfv_pair(function(d, t) 2.7, ccg), 1),
             pattern = "not an integer")

## `NA` is still the documented plaintext reply, and reaches the
## caller rather than the codec.
na_sites <- list(make_worker("N1", NULL, function(d, t) NA),
                 make_worker("N2", NULL, function(d, t) 1L))
expect_true(is.na(master_aggregate(
    make_threshold_master(name = "NM", crypto_context = ccb, sites = na_sites), 1)))

## ---- Property validators ------------------------------------------------
## A party's name appears in every message about it.
expect_error(make_worker("", 1, nll),        pattern = "non-empty string")
expect_error(make_worker(NA_character_, 1, nll), pattern = "non-empty string")
expect_error(make_worker(c("a", "b"), 1, nll),   pattern = "non-empty string")
expect_error(make_worker("x", 1, 42),        pattern = "contribution_fn")
expect_error(make_ncparty("x", 3),           pattern = "must be 1 or 2")

## ---- Unrecognized parameter bundles ------------------------------------
expect_error(set_public_params(make_worker("z", 1, nll), list(pk = 1)),
             pattern = "PublicParams")
