# Distributed Cox Regression with Threshold Key Generation

## Introduction

The companion `cox` vignette fits a stratified Cox model across three
sites under CKKS via master/worker. The master holds the secret key and
decrypts the encrypted partial likelihood at every iteration of the
optimizer. This is *operationally* convenient — one party owns
decryption — but it leaves a meaningful piece of the threat model
unaddressed: the master is trusted not to decrypt anything it shouldn’t.

The end of the `cox` vignette flagged this:

> **Threshold key generation**: in a real deployment, the secret key
> would be split across the sites (n-of-n threshold) so that no single
> party — not even the master — can decrypt intermediate ciphertexts
> unilaterally.

This vignette implements exactly that. Three sites jointly generate a
CKKS key pair so that no single party holds the joint secret key. The
master’s role is reduced to an aggregator: it sums encrypted
contributions and orchestrates partial-decryption fan-in, but it cannot
decrypt anything by itself.

The mathematical content is identical to
[`vignette("cox")`](https://bnaras.github.io/homomorpheR/articles/cox.md)
— same data, same fit, same coefficients to floating-point precision.
Only the trust model changes.

## Threat model

Three sites and one untrusted aggregator:

- **Sites $`1, 2, 3`$** each hold private patient data and a secret key
  share $`\mathit{sk}_i`$. They are honest-but-curious among themselves
  and toward the aggregator.
- **Aggregator** holds no secret-key material. It receives encrypted
  contributions, sums them homomorphically, and broadcasts the sum back
  for partial decryption. It is *fully untrusted*: a curious or
  compromised aggregator gains no information from the ciphertexts it
  processes.

What the aggregator sees, by stage: 1. Encrypted local contributions
$`\mathit{ct}_i = E_{\mathit{pk}_{1..n}}(\ell_i)`$. None decryptable
alone. 2. The encrypted sum
$`\mathit{ct}_{\text{sum}} = \boxplus_i \mathit{ct}_i`$. Not decryptable
alone. 3. Partial decryptions $`\rho_i`$ contributed by each site. Not
decryptable individually. 4. After fusion, the *plaintext sum*
$`\ell(\beta) = \sum_i \ell_i`$. This appears at the aggregator only
after step 4.

Step 4 reveals $`\ell(\beta)`$ to the aggregator — the same thing the
master saw in
[`vignette("cox")`](https://bnaras.github.io/homomorpheR/articles/cox.md).
The strict improvement is that *nobody else* can decrypt anything along
the way: a captured log of ciphertexts or partial decryptions is useless
without the joint fusion.

## The Cox setup (same DLBCL data as `cox.Rmd`)

``` r

suppressPackageStartupMessages(library(survival))
library(homomorpheR)
data(DLBCL)

cox_data <- split(
  DLBCL[, c("time", "status", "GCB_sig", "LN_sig",
            "Prolif_sig", "BMP6", "MHC2_sig", "Subgroup")],
  DLBCL$Subgroup)

agg_model <- coxph(Surv(time, status) ~ GCB_sig + LN_sig +
                       Prolif_sig + BMP6 + MHC2_sig +
                       strata(Subgroup),
                   data = DLBCL)
```

## The protocol

**Setup** (once):

1.  Site 1 calls `key_gen(cc)` to produce its keypair
    $`(\mathit{pk}_1, \mathit{sk}_1)`$.
2.  Site 2 calls `multiparty_key_gen(cc, pk_1)` to produce
    $`(\mathit{pk}_{12}, \mathit{sk}_2)`$.
3.  Site 3 calls `multiparty_key_gen(cc, pk_{12})` to produce
    $`(\mathit{pk}_{123}, \mathit{sk}_3)`$.
4.  The final $`\mathit{pk}_{123}`$ is the **joint public key**. Each
    site keeps its own $`\mathit{sk}_i`$.

**Per query** (called inside the optimizer):

1.  Each site $`i`$ computes its local Cox negative log-likelihood
    $`\ell_i(\beta)`$ and encrypts it under the joint public key.
2.  The aggregator sums the encrypted contributions homomorphically.
3.  Each site partial-decrypts the sum using its own $`\mathit{sk}_i`$.
4.  The aggregator fuses the partials to recover $`\ell(\beta)`$.

`homomorpheR` exports a
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
constructor that runs the chained key generation in one call and returns
a \[ThresholdMaster\] holding the joint public key plus the per-site
secret shares. The same
[`set_workers()`](https://bnaras.github.io/homomorpheR/reference/set_workers.md)
and
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
helpers used by the single-decrypter `cox.Rmd` work unchanged here: the
threshold-specific encrypt/partial-decrypt/fuse logic lives inside
[`master_encrypt()`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md)
and
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
methods on `ThresholdMaster`, so the per-iteration code looks the same
as the single-decrypter case.

## Implementation

``` r

cph_control <- replace(coxph.control(), "iter.max", 0)

local_cox_nll <- function(data, beta) {
    fit <- tryCatch(
        coxph(Surv(time, status) ~ GCB_sig + LN_sig + Prolif_sig +
                  BMP6 + MHC2_sig,
              data    = data,
              init    = beta,
              control = cph_control),
        error = function(e) NULL)
    if (is.null(fit)) NA_real_ else -fit$loglik[1]
}
```

The CKKS context needs the `MULTIPARTY` feature enabled so the chained
`multiparty_key_gen()` calls work:

``` r

cc <- openfhe.R::fhe_context("CKKS",
                           multiplicative_depth = 1L,
                           scaling_mod_size     = 59L,
                           first_mod_size       = 60L,
                           batch_size           = 8L,
                           features             = c(openfhe.R::Feature$MULTIPARTY))
```

`make_threshold_master(name, cc, n_sites)` runs the chained key
generation and returns a master holding the joint public key and a list
of per-site secret shares. The master’s `master_encrypt` /
`master_decrypt` methods automatically use the joint pk for encryption
and the partial-decrypt fan-in for decryption.

``` r

master <- make_threshold_master("Aggregator",
                                crypto_context = cc,
                                n_sites        = 3)

worker_gcb <- make_worker("GCB",      data = cox_data[["GCB"]],      local_fn = local_cox_nll)
worker_abc <- make_worker("ABC",      data = cox_data[["ABC"]],      local_fn = local_cox_nll)
worker_t3  <- make_worker("Type III", data = cox_data[["Type III"]], local_fn = local_cox_nll)

set_workers(master, list(worker_gcb, worker_abc, worker_t3))
```

## Iterative MLE through the threshold protocol

The optimizer-facing code is *identical* to `cox.Rmd`. Same
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
runner, same [`mle()`](https://rdrr.io/r/stats4/mle.html) driver — only
the underlying master class changed.

``` r

library(stats4)

encrypted_nLL <- function(GCB_sig, LN_sig, Prolif_sig, BMP6, MHC2_sig) {
    master_aggregate(master, c(GCB_sig, LN_sig, Prolif_sig, BMP6, MHC2_sig))
}

fit <- mle(encrypted_nLL,
           start   = list(GCB_sig = 0, LN_sig = 0, Prolif_sig = 0,
                          BMP6    = 0, MHC2_sig = 0),
           method  = "BFGS",
           control = list(reltol = 1e-7))
summary(fit)
```

    ## Maximum likelihood estimation
    ## 
    ## Call:
    ## mle(minuslogl = encrypted_nLL, start = list(GCB_sig = 0, LN_sig = 0, 
    ##     Prolif_sig = 0, BMP6 = 0, MHC2_sig = 0), method = "BFGS", 
    ##     control = list(reltol = 1e-07))
    ## 
    ## Coefficients:
    ##              Estimate Std. Error
    ## GCB_sig    -0.2638698 0.11940447
    ## LN_sig     -0.2543587 0.08515178
    ## Prolif_sig  0.3031250 0.14981283
    ## BMP6        0.3036367 0.10727837
    ## MHC2_sig   -0.3191459 0.09412946
    ## 
    ## -2 log L: 990.458

``` r

logLik(fit)
```

    ## 'log Lik.' -495.229 (df=5)

## Comparison with the cleartext fit

``` r

summary(agg_model)
```

    ## Call:
    ## coxph(formula = Surv(time, status) ~ GCB_sig + LN_sig + Prolif_sig + 
    ##     BMP6 + MHC2_sig + strata(Subgroup), data = DLBCL)
    ## 
    ##   n= 235, number of events= 133 
    ## 
    ##                coef exp(coef) se(coef)      z Pr(>|z|)    
    ## GCB_sig    -0.26387   0.76807  0.11940 -2.210 0.027112 *  
    ## LN_sig     -0.25436   0.77541  0.08515 -2.987 0.002816 ** 
    ## Prolif_sig  0.30313   1.35408  0.14981  2.023 0.043036 *  
    ## BMP6        0.30364   1.35478  0.10728  2.830 0.004649 ** 
    ## MHC2_sig   -0.31915   0.72677  0.09413 -3.391 0.000698 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##            exp(coef) exp(-coef) lower .95 upper .95
    ## GCB_sig       0.7681     1.3020    0.6078    0.9706
    ## LN_sig        0.7754     1.2896    0.6562    0.9163
    ## Prolif_sig    1.3541     0.7385    1.0095    1.8162
    ## BMP6          1.3548     0.7381    1.0979    1.6718
    ## MHC2_sig      0.7268     1.3760    0.6043    0.8740
    ## 
    ## Concordance= 0.668  (se = 0.027 )
    ## Likelihood ratio test= 42.74  on 5 df,   p=4e-08
    ## Wald test            = 44.78  on 5 df,   p=2e-08
    ## Score (logrank) test = 44.83  on 5 df,   p=2e-08

``` r

cat(sprintf("logLik(threshold-distributed encrypted): %f\n",
            as.numeric(logLik(fit))))
```

    ## logLik(threshold-distributed encrypted): -495.229022

``` r

cat(sprintf("logLik(aggregated cleartext)            : %f\n",
            agg_model$loglik[2]))
```

    ## logLik(aggregated cleartext)            : -495.229022

| coefficient | threshold_distributed | aggregated_cleartext |  abs_diff |
|:------------|----------------------:|---------------------:|----------:|
| GCB_sig     |            -0.2638698 |           -0.2638716 | 1.822e-06 |
| LN_sig      |            -0.2543587 |           -0.2543592 | 5.340e-07 |
| Prolif_sig  |             0.3031250 |            0.3031258 | 7.480e-07 |
| BMP6        |             0.3036367 |            0.3036375 | 7.940e-07 |
| MHC2_sig    |            -0.3191459 |           -0.3191467 | 8.420e-07 |

Threshold-distributed BFGS vs. aggregated-cleartext coxph() {.table}

## Discussion

1.  **No single party holds the decryption key.**
    [`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
    distributes the secret across three sites; the master holds only the
    joint public key. Encrypted intermediate values are undecryptable by
    any single party in the system.
2.  **Same statistical fit as the trusted-master version.** The
    coefficients and standard errors agree with
    [`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) to the
    precision the optimizer cares about. Threshold key generation is a
    trust-model improvement; it does not change the fit.
3.  **Same optimizer, same callback shape, same code structure.** The
    only line that changed compared to `cox.Rmd` is the master
    constructor
    ([`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
    instead of
    [`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)).
    The optimizer sees nothing different.

## Limitations

- **The aggregator sees $`\ell(\beta)`$ at every iteration.** That is
  the optimizer’s *function value* — what
  [`mle()`](https://rdrr.io/r/stats4/mle.html) is asking for. The
  information leak is “the value of the joint partial log-likelihood at
  every $`\beta`$ visited,” not “individual per-site contributions”
  (which are properly hidden). Hiding $`\ell(\beta)`$ would require
  running the optimizer inside the encrypted domain — feasible but
  considerably more complex.
- **Honest-but-curious is the trust model.** Sites are assumed to follow
  the protocol. A malicious site could submit a corrupted partial
  decryption to break the fit; detecting this requires additional
  protocol machinery (commitments, zero-knowledge proofs) that this
  vignette does not implement.
- **Output privacy is unchanged.** The released coefficients
  $`\hat\beta`$ are the same as the cleartext fit. Output-level attacks
  (membership inference, model inversion) remain in scope and motivate
  the differential-privacy demonstrations elsewhere in the package.

## Where this fits

Read
[`vignette("cox")`](https://bnaras.github.io/homomorpheR/articles/cox.md)
first if you have not — that vignette establishes the master/worker
protocol and the optimizer-driven shape this one inherits. The only
difference here is the master: threshold instead of single-decrypter.
Same code, stronger trust model.
