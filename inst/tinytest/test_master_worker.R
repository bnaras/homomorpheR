## Master/worker protocol — the realistic distributed-stats topology.
## Same agreement-with-cleartext-oracle and NA-propagation tests as
## the round-robin runner.

library(homomorpheR)

keys <- paillier_keypair(512)
local_nll <- function(data, lambda) {
    -sum(stats::dpois(data, lambda, log = TRUE))
}

## ---- Encrypted master/worker matches cleartext oracle (Paillier) ---------
master <- make_master("Master", keys)
w1 <- make_worker("S1", c(2, 3), local_nll)
w2 <- make_worker("S2", c(4, 5), local_nll)
set_workers(master, list(w1, w2))

direct    <- -sum(stats::dpois(c(2, 3, 4, 5), 3.5, log = TRUE))
encrypted <- run_master_worker(master, 3.5)
expect_true(abs(encrypted - direct) < 1e-9)

## ---- NA propagation -----------------------------------------------------
master2 <- make_master("M2", keys)
w_bad <- make_worker("Sbad", c(2, 3),
                     function(d, lambda) if (lambda < 0.01) NA else
                         -sum(stats::dpois(d, lambda, log = TRUE)))
set_workers(master2, list(w_bad))
expect_true(is.na(run_master_worker(master2, 0.001)))
expect_true(!is.na(run_master_worker(master2, 1.0)))

## ---- mle() converges through the encrypted master/worker channel -------
master3 <- make_master("M3", keys)
w_a <- make_worker("S1", c(2, 3), local_nll)
w_b <- make_worker("S2", c(4, 5), local_nll)
set_workers(master3, list(w_a, w_b))
fit <- stats4::mle(function(lambda) run_master_worker(master3, lambda),
                   start = list(lambda = 5))
expect_true(abs(stats4::coef(fit) - mean(c(2, 3, 4, 5))) < 1e-3)

## ---- CKKS-backed master/worker (skip if openfhe not installed) ---------
if (requireNamespace("openfhe", quietly = TRUE)) {
    cc <- openfhe::fhe_context("CKKS",
                               multiplicative_depth = 1L,
                               scaling_mod_size     = 50L,
                               batch_size           = 8L)
    ckks_keys <- openfhe::key_gen(cc)
    cmaster <- make_ckks_master("CMaster", crypto_context = cc, keypair = ckks_keys)
    cw1 <- make_worker("S1", c(2, 3), local_nll)
    cw2 <- make_worker("S2", c(4, 5), local_nll)
    set_workers(cmaster, list(cw1, cw2))
    encrypted_ckks <- run_master_worker(cmaster, 3.5)
    expect_true(abs(encrypted_ckks - direct) < 1e-3)   # CKKS tolerance
}

## ---- Empty worker list errors -----------------------------------------
master4 <- make_master("M4", keys)
expect_error(set_workers(master4, list()),
             pattern = "at least one worker")
expect_error(run_master_worker(master4, 1.0),
             pattern = "no workers")
