## Threshold-BFV master/worker: the same threshold master, scheme read
## back from the context, driving EXACT integer aggregation. A count is
## an integer, so the decrypted total must equal the plaintext sum
## bit-for-bit -- no CKKS-style tolerance.

if (!requireNamespace("openfhe.R", quietly = TRUE))
    exit_file("openfhe.R not installed")

library(homomorpheR)

cc <- openfhe.R::fhe_context("BFV",
                             plaintext_modulus    = 65537L,
                             multiplicative_depth = 1L,
                             features             = c(openfhe.R::Feature$MULTIPARTY))

## Each site returns a count (an integer summary of local data).
local_count <- function(data, query) sum(eval(query, data))

set.seed(130)
sample_size <- c(60, 15, 25)
query_data <- lapply(sample_size, function(n)
    data.frame(sex = sample(c("F", "M"), n, replace = TRUE),
               age = sample(40:70, n, replace = TRUE),
               bm  = rnorm(n)))
query <- quote(age < 50 & sex == "F" & bm < 0.2)

master  <- make_threshold_master("Coordinator", crypto_context = cc, n_sites = 3)
workers <- Map(function(nm, d) make_worker(nm, d, local_count),
               c("S1", "S2", "S3"), query_data)
set_workers(master, workers)

cleartext <- sum(vapply(query_data, function(d) sum(eval(query, d)), integer(1)))
encrypted <- master_aggregate(master, theta = query)

## EXACT, not approximate: the defining property of BFV vs CKKS.
expect_identical(as.integer(encrypted), as.integer(cleartext))

## A plain integer sum with no query, to isolate the codec from the
## query machinery.
m2 <- make_threshold_master("M2", crypto_context = cc, n_sites = 3)
w  <- Map(function(nm, k) make_worker(nm, k, function(data, theta) data),
          c("A", "B", "C"), list(7L, 3L, 12L))
set_workers(m2, w)
expect_identical(as.integer(master_aggregate(m2, theta = 0)), 22L)
