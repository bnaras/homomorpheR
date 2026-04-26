## Threshold-CKKS master/worker: n-of-n key generation, no single
## decrypter. The master's `master_decrypt` does the partial-decrypt
## fan-in across all sites' secret shares and fuses.

if (!requireNamespace("openfhe", quietly = TRUE))
    exit_file("openfhe not installed")

library(homomorpheR)

cc <- openfhe::fhe_context("CKKS",
                           multiplicative_depth = 1L,
                           scaling_mod_size     = 50L,
                           batch_size           = 8L,
                           features             = c(openfhe::Feature$MULTIPARTY))

local_nll <- function(data, lambda) {
    -sum(stats::dpois(data, lambda, log = TRUE))
}

## Three sites, threshold-keygen master.
master <- make_threshold_master("Master", crypto_context = cc, n_sites = 3)
w1 <- make_worker("S1", c(2, 3),    local_nll)
w2 <- make_worker("S2", c(4, 5),    local_nll)
w3 <- make_worker("S3", c(6, 7, 8), local_nll)
set_workers(master, list(w1, w2, w3))

direct    <- -sum(stats::dpois(c(2, 3, 4, 5, 6, 7, 8), 5, log = TRUE))
encrypted <- run_master_worker(master, 5)
expect_true(abs(encrypted - direct) < 1e-3)

## NA propagation through threshold path too.
master2  <- make_threshold_master("M2", cc, n_sites = 2)
w_bad <- make_worker("Sbad", c(2, 3),
                     function(d, lambda) if (lambda < 0.01) NA else
                         -sum(stats::dpois(d, lambda, log = TRUE)))
w_ok  <- make_worker("Sok", c(4, 5), local_nll)
set_workers(master2, list(w_bad, w_ok))
expect_true(is.na(run_master_worker(master2, 0.001)))
expect_true(!is.na(run_master_worker(master2, 1.0)))

## n_sites = 1 errors (degenerate, no threshold needed).
expect_error(make_threshold_master("M", cc, n_sites = 1),
             pattern = "at least two sites")

## Constructor produces secret_keys list of the right length.
m3 <- make_threshold_master("M3", cc, n_sites = 4)
expect_equal(length(m3@secret_keys), 4L)
