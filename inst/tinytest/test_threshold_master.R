## Threshold-CKKS master/worker: n-of-n key generation, no single
## decrypter. Each site generates and keeps its own share; the master
## holds only public material and drives decryption by asking every
## site for a partial and fusing the results.

if (!requireNamespace("openfhe.R", quietly = TRUE))
    exit_file("openfhe.R not installed")

library(homomorpheR)

cc <- openfhe.R::fhe_context("CKKS",
                           multiplicative_depth = 1L,
                           scaling_mod_size     = 50L,
                           batch_size           = 8L,
                           features             = c(openfhe.R::Feature$MULTIPARTY))

local_nll <- function(data, lambda) {
    -sum(stats::dpois(data, lambda, log = TRUE))
}

## Three sites; the key-generation chain runs through them and returns
## a master already wired.
w1 <- make_worker("S1", c(2, 3),    local_nll)
w2 <- make_worker("S2", c(4, 5),    local_nll)
w3 <- make_worker("S3", c(6, 7, 8), local_nll)
master <- make_threshold_master("Master", crypto_context = cc,
                                sites = list(w1, w2, w3))

direct    <- -sum(stats::dpois(c(2, 3, 4, 5, 6, 7, 8), 5, log = TRUE))
encrypted <- master_aggregate(master, 5)
expect_true(abs(encrypted - direct) < 1e-3)

## The master must hold no secret material at all. This is the property
## the whole threshold construction claims, so assert it on the object
## rather than trusting the documentation.
expect_false("secret_keys" %in% names(S7::props(master)))
expect_true(is.null(master@state$privkey))
expect_true(is.null(master@state$sk))

## Every share lives at the site that generated it, and nowhere else.
for (w in list(w1, w2, w3)) expect_false(is.null(w@state$sk))
expect_false(identical(w1@state$sk, w2@state$sk))

## Each site got the joint public key, and they all got the same one.
expect_true(identical(w1@state$params$pk, w3@state$params$pk))

## The published bundle carries no secret material.
expect_true(all(c("scheme", "cc", "pk") %in% names(w1@state$params)))
expect_false(any(grepl("sk|secret|private", names(w1@state$params))))

## NA propagation through the threshold path too.
b1 <- make_worker("Sbad", c(2, 3),
                  function(d, lambda) if (lambda < 0.01) NA else
                      -sum(stats::dpois(d, lambda, log = TRUE)))
b2 <- make_worker("Sok", c(4, 5), local_nll)
master2 <- make_threshold_master("M2", cc, list(b1, b2))
expect_true(is.na(master_aggregate(master2, 0.001)))
expect_true(!is.na(master_aggregate(master2, 1.0)))

## A single site is degenerate: no threshold to speak of.
expect_error(make_threshold_master("M", cc, list(make_worker("S", 1, local_nll))),
             pattern = "at least two sites")

## A threshold master is wired by its constructor; re-wiring would
## break the site order the fusion depends on.
expect_error(set_workers(master, list(w1, w2, w3)),
             pattern = "already wired")

## A site that never took part in key generation cannot fake a partial
## decryption.
stranger <- make_worker("Outsider", c(1, 2), local_nll)
expect_error(partial_decrypt(stranger,
                             encrypt_under(w1@state$params, 1), lead = FALSE),
             pattern = "no secret share")

## keygen_round returns a public key, never a secret one: a share that
## can be returned is a share that can be collected centrally.
fresh <- make_worker("Fresh", c(1, 2), local_nll)
pk1   <- keygen_round(fresh, cc, NULL)
expect_false(is.null(fresh@state$sk))
expect_true(inherits(pk1, "openfhe::PublicKey") ||
            grepl("PublicKey", paste(class(pk1), collapse = " ")))

## print() on a threshold master must not reach Paillier-only
## properties (ThresholdMaster has no keypair property at all).
m3 <- make_threshold_master("M3", cc, list(make_worker("a", 1, local_nll),
                                           make_worker("b", 2, local_nll)))
expect_true(any(grepl("ThresholdMaster", capture.output(print(m3)))))
