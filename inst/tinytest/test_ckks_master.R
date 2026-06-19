## CKKS-backed master: verify the same protocol body that drives the
## Paillier round-robin also drives a CKKS round-robin via
## openfhe.R::CryptoContext + KeyPair.
##
## Skipped if openfhe.R is not installed (e.g., on a CRAN check
## environment without the C++ backend).

if (!requireNamespace("openfhe.R", quietly = TRUE)) exit_file("openfhe.R not installed")

library(homomorpheR)

cc <- openfhe.R::fhe_context("CKKS",
                           multiplicative_depth = 1L,
                           scaling_mod_size     = 50L,
                           batch_size           = 8L)
keys <- openfhe.R::key_gen(cc)

local_nll <- function(data, lambda) {
    -sum(stats::dpois(data, lambda, log = TRUE))
}

site1  <- make_site("S1", c(2, 3), local_fn = local_nll)
site2  <- make_site("S2", c(4, 5), local_fn = local_nll)
master <- make_ckks_master("Master", crypto_context = cc, keypair = keys)
round_robin_chain(master, list(site1, site2))

## Encrypted-channel result agrees with the aggregated cleartext fit.
direct    <- -sum(stats::dpois(c(2, 3, 4, 5), 3.5, log = TRUE))
encrypted <- run_round_robin(master, 3.5)
expect_true(abs(encrypted - direct) < 1e-3)   # CKKS approximation tolerance

## NA propagation through CKKS as well.
master2  <- make_ckks_master("M2", cc, keys)
site_bad <- make_site("Sbad", c(2, 3),
                      function(d, lambda) if (lambda < 0.01) NA else
                          -sum(stats::dpois(d, lambda, log = TRUE)))
round_robin_chain(master2, list(site_bad))
expect_true(is.na(run_round_robin(master2, 0.001)))
expect_true(!is.na(run_round_robin(master2, 1.0)))

## mle() converges through the encrypted CKKS channel.
master3 <- make_ckks_master("M3", cc, keys)
s1 <- make_site("S1", c(2, 3), local_fn = local_nll)
s2 <- make_site("S2", c(4, 5), local_fn = local_nll)
round_robin_chain(master3, list(s1, s2))
fit <- stats4::mle(function(lambda) run_round_robin(master3, lambda),
                   start = list(lambda = 5))
expect_true(abs(stats4::coef(fit) - mean(c(2, 3, 4, 5))) < 1e-2)
