# homomorpheR: privacy-preserving statistics across sites

`homomorpheR` runs statistical computations across sites that never
share their data, using fully homomorphic encryption through the
`openfhe.R` interface to OpenFHE: CKKS for real-valued arithmetic, BFV
and BGV for exact integers, with *n*-of-*n* threshold key generation so
that no single party can decrypt.

## Details

The protocol actors are a
[Master](https://bnaras.github.io/homomorpheR/reference/Master.md) and
its [Site](https://bnaras.github.io/homomorpheR/reference/Site.md)s. A
[LocalSite](https://bnaras.github.io/homomorpheR/reference/LocalSite.md),
built with
[`make_worker()`](https://bnaras.github.io/homomorpheR/reference/make_worker.md),
holds its data and a `contribution_fn`;
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
runs one round, in which each site returns its contribution already
encrypted and only the aggregate is decrypted. Build the master with
[`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)
when one party may hold the secret key and with
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
when none may. Ordinary R modeling code –
[`stats4::mle()`](https://rdrr.io/r/stats4/mle.html), stratified
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html),
convex programs via `CVXR` – then runs unchanged with the encrypted
round as its objective. The package vignettes develop each protocol in
full.

A frozen implementation of the Paillier additive scheme is kept for
backward compatibility; see
[`paillier_keypair()`](https://bnaras.github.io/homomorpheR/reference/paillier_keypair.md).

## See also

Useful links:

- <https://bnaras.github.io/homomorpheR/>

- Report bugs at <https://github.com/bnaras/homomorpheR/issues>

## Author

**Maintainer**: Balasubramanian Narasimhan <naras@stat.Stanford.EDU>
\[copyright holder\]

Authors:

- Balasubramanian Narasimhan <naras@stat.Stanford.EDU> \[copyright
  holder\]

## Examples

``` r
## A Poisson rate estimated across three sites: each site encrypts
## its negative log-likelihood, and only the sum is decrypted.
local_nll <- function(data, lambda)
    -sum(stats::dpois(data, lambda, log = TRUE))
y <- c(9, 12, 7, 11, 10, 8, 13, 9, 10, 12)

cc   <- openfhe.R::fhe_context("CKKS", multiplicative_depth = 1L,
                               scaling_mod_size = 50L, batch_size = 8L)
keys <- openfhe.R::key_gen(cc)
master <- make_ckks_master("Master", crypto_context = cc, keypair = keys)
set_workers(master, list(
    make_worker("S1", y[1:3],  local_nll),
    make_worker("S2", y[4:6],  local_nll),
    make_worker("S3", y[7:10], local_nll)))

fit <- stats4::mle(function(lambda) master_aggregate(master, lambda),
                   start = list(lambda = 5))
c(encrypted = stats4::coef(fit)[["lambda"]], cleartext = mean(y))
#> encrypted cleartext 
#>      10.1      10.1 
```
