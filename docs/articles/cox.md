# Distributed Stratified Cox Regression under CKKS

## The statistical problem

The Cox proportional hazards model is one of the workhorses of medical
statistics. Given covariates $`x_i`$ for subject $`i`$, an event time
$`t_i`$, and an event indicator $`\delta_i`$, the hazard is modeled as

``` math
h(t \mid x_i) \;=\; h_0(t) \, \exp(\beta^\top x_i)
```

where $`h_0(t)`$ is an unspecified baseline hazard and $`\beta`$ is the
vector of regression coefficients we want to estimate. The **partial
log-likelihood** depends only on $`\beta`$:

``` math
\ell(\beta) \;=\; \sum_{i: \delta_i = 1}
  \left[\, \beta^\top x_i \;-\; \log\!\!\sum_{j \in R_i} \exp(\beta^\top x_j) \,\right]
```

where $`R_i`$ is the risk set at time $`t_i`$.

For **stratified** Cox regression — when baseline hazards differ across
strata (e.g. across study sites) but the coefficients $`\beta`$ are
shared — the partial log-likelihood becomes a sum over strata:

``` math
\ell(\beta) \;=\; \sum_{s=1}^{S} \ell_s(\beta).
```

This additive decomposition is exactly what we need for a
privacy-preserving distributed protocol. The same decomposition
underlies the master/worker architecture used by `distcomp`,
`DataSHIELD`, and `WebDISCO`: a central master broadcasts the current
$`\beta`$, each site computes its local $`\ell_s(\beta)`$ on its private
data, and the master combines the local contributions. We adopt the same
topology, with the encrypted-aggregation step performed under CKKS so
that the master sees only the *sum* of local contributions and not the
individual addends.

## DLBCL lymphoma cohort

We use the diffuse large-B-cell lymphoma (DLBCL) cohort of Rosenwald et
al. (2002), the same dataset that Bayle et al. (2025) use to motivate
distributed Cox estimation. The `data(DLBCL)` table shipped with
homomorpheR excludes the five patients with zero follow-up time
(following Bayle et al. (2025)), leaving 235 patients with 133 deaths
over a median follow-up of 2.8 years. The published outcome predictor
combines five expression signatures: germinal-center B cell, lymph node,
proliferation, BMP6, and MHC class II. We model the hazard as a function
of those five signatures, stratified by molecular subgroup (GCB, ABC,
Type III) and *use the subgroup itself as the site boundary*. This is
biologically and operationally realistic: GCB, ABC, and Type III tumors
arise from different cells of origin and tend to be diagnosed at
different referral centers. The three sites are imbalanced in size (GCB
$`n=115`$, ABC $`n=71`$, Type III $`n=49`$, with 54, 49, and 30 deaths
respectively), which the master/worker protocol handles transparently.

[`suppressPackageStartupMessages`](https://rdrr.io/r/base/message.html)`(`[`library`](https://rdrr.io/r/base/library.html)`(`[`survival`](https://github.com/therneau/survival)`)``)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`homomorpheR`](https://bnaras.github.io/homomorpheR/)`)`` `[`data`](https://rdrr.io/r/utils/data.html)`(``DLBCL``)`` `` ``cox_data`` ``<-`` `[`split`](https://rdrr.io/r/base/split.html)`(`` `` ``DLBCL``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"time"``, ``"status"``, ``"GCB_sig"``, ``"LN_sig"``,`` `` ``"Prolif_sig"``, ``"BMP6"``, ``"MHC2_sig"``, ``"Subgroup"``)``]``,`` `` ``DLBCL``$``Subgroup``)`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(``cox_data``, ``function``(``df``)`` `[`c`](https://rdrr.io/r/base/c.html)`(``n ``=`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``df``)``, events ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``df``$``status``)``)``)`

    ##        GCB ABC Type III
    ## n      115  71       49
    ## events  54  49       30

## The aggregated fit

If all data were in one place, fitting the stratified Cox model is
trivial:

`agg_model`` ``<-`` `[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html)`(`[`Surv`](https://rdrr.io/pkg/survival/man/Surv.html)`(``time``, ``status``)`` ``~`` ``GCB_sig`` ``+`` ``LN_sig`` ``+`` `` ``Prolif_sig`` ``+`` ``BMP6`` ``+`` ``MHC2_sig`` ``+`` `` `[`strata`](https://rdrr.io/pkg/survival/man/strata.html)`(``Subgroup``)``,`` `` data ``=`` ``DLBCL``)`` ``agg_model`

    ## Call:
    ## coxph(formula = Surv(time, status) ~ GCB_sig + LN_sig + Prolif_sig + 
    ##     BMP6 + MHC2_sig + strata(Subgroup), data = DLBCL)
    ## 
    ##                coef exp(coef) se(coef)      z        p
    ## GCB_sig    -0.26387   0.76807  0.11940 -2.210 0.027112
    ## LN_sig     -0.25436   0.77541  0.08515 -2.987 0.002816
    ## Prolif_sig  0.30313   1.35408  0.14981  2.023 0.043036
    ## BMP6        0.30364   1.35478  0.10728  2.830 0.004649
    ## MHC2_sig   -0.31915   0.72677  0.09413 -3.391 0.000698
    ## 
    ## Likelihood ratio test=42.74  on 5 df, p=4.174e-08
    ## n= 235, number of events= 133

`agg_model``$``loglik`

    ## [1] -516.5986 -495.2290

The first log-likelihood is at $`\beta = 0`$ (the null model); the
second is at the MLE. The privacy goal: reproduce these estimates
*without* the three sites ever pooling their raw data.

## The protocol

We use the same master/worker topology as the MLE vignette: master
broadcasts $`\beta`$, each worker computes its local Cox partial
log-likelihood at $`\beta`$, encrypts it under the master’s public key,
and returns the ciphertext. The master sums the encrypted contributions
homomorphically and decrypts the total. Mathematically nothing changes
from the MLE case; only the local computation differs.

The local computation exploits a well-known feature of
[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html): `iter.max = 0`
returns the partial log-likelihood evaluated at the supplied `init`
*without* taking any Newton-Raphson steps.

`cph_control`` ``<-`` `[`replace`](https://rdrr.io/r/base/replace.html)`(`[`coxph.control`](https://rdrr.io/pkg/survival/man/coxph.control.html)`(``)``, ``"iter.max"``, ``0``)`` `` ``local_cox_nll`` ``<-`` ``function``(``data``, ``beta``)`` ``{`` `` ``fit`` ``<-`` `[`tryCatch`](https://rdrr.io/r/base/conditions.html)`(`` `` `[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html)`(`[`Surv`](https://rdrr.io/pkg/survival/man/Surv.html)`(``time``, ``status``)`` ``~`` ``GCB_sig`` ``+`` ``LN_sig`` ``+`` ``Prolif_sig`` ``+`` `` ``BMP6`` ``+`` ``MHC2_sig``,`` `` data ``=`` ``data``,`` `` init ``=`` ``beta``,`` `` control ``=`` ``cph_control``)``,`` `` error ``=`` ``function``(``e``)`` ``NULL``)`` `` ``if`` ``(`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``fit``)``)`` ``NA_real_`` ``else`` ``-``fit``$``loglik``[``1``]`` ``}`

`tryCatch` returns `NA_real_` if the local fit blows up at extreme
$`\beta`$ — `homomorpheR`’s
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
propagates that `NA` back to the optimizer, which simply backs off.

## Wiring up the protocol

The summed stratified Cox nLL on this cohort has magnitude
$`\approx 5 \times 10^2`$ — comfortably within CKKS precision at the
default scaling parameters. We lift `scaling_mod_size` from 50 to 59 and
set `first_mod_size = 60` for a safety margin.

`cc`` ``<-`` ``openfhe.R``::`[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`` `` multiplicative_depth ``=`` ``1L``,`` `` scaling_mod_size ``=`` ``59L``,`` `` first_mod_size ``=`` ``60L``,`` `` batch_size ``=`` ``8L``)`` ``keys`` ``<-`` ``openfhe.R``::`[`key_gen`](https://openfheorg.github.io/openfhe.R/reference/key_gen.html)`(``cc``)`` `` ``worker_gcb`` ``<-`` `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``"GCB"``, data ``=`` ``cox_data``[[``"GCB"``]``]``, local_fn ``=`` ``local_cox_nll``)`` ``worker_abc`` ``<-`` `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``"ABC"``, data ``=`` ``cox_data``[[``"ABC"``]``]``, local_fn ``=`` ``local_cox_nll``)`` ``worker_t3`` ``<-`` `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``"Type III"``, data ``=`` ``cox_data``[[``"Type III"``]``]``, local_fn ``=`` ``local_cox_nll``)`` ``master`` ``<-`` `[`make_ckks_master`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)`(``"Master"``, crypto_context ``=`` ``cc``, keypair ``=`` ``keys``)`` `[`set_workers`](https://bnaras.github.io/homomorpheR/reference/set_workers.md)`(``master``, `[`list`](https://rdrr.io/r/base/list.html)`(``worker_gcb``, ``worker_abc``, ``worker_t3``)``)`

## Iterative MLE through the encrypted protocol

We hand [`stats4::mle()`](https://rdrr.io/r/stats4/mle.html) a function
that looks like a standard multivariate negative log-likelihood. Each
call drives one master/worker round and returns a single decrypted
scalar.

[`library`](https://rdrr.io/r/base/library.html)`(``stats4``)`` `` ``encrypted_nLL`` ``<-`` ``function``(``GCB_sig``, ``LN_sig``, ``Prolif_sig``, ``BMP6``, ``MHC2_sig``)`` ``{`` `` `[`master_aggregate`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)`(``master``, `[`c`](https://rdrr.io/r/base/c.html)`(``GCB_sig``, ``LN_sig``, ``Prolif_sig``, ``BMP6``, ``MHC2_sig``)``)`` ``}`` `` ``## Parameter order matches the coxph formula above, so the`` ``## side-by-side comparison below is honest. BFGS uses numerical`` ``## finite-difference gradients, the fair comparison given a`` ``## generic optimizer driven through an encrypted channel.`` ``fit`` ``<-`` `[`mle`](https://rdrr.io/r/stats4/mle.html)`(``encrypted_nLL``,`` `` start ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``GCB_sig ``=`` ``0``, LN_sig ``=`` ``0``, Prolif_sig ``=`` ``0``,`` `` BMP6 ``=`` ``0``, MHC2_sig ``=`` ``0``)``,`` `` method ``=`` ``"BFGS"``,`` `` control ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``reltol ``=`` ``1e-7``)``)`` `[`summary`](https://rdrr.io/r/base/summary.html)`(``fit``)`

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

[`logLik`](https://rdrr.io/r/stats/logLik.html)`(``fit``)`

    ## 'log Lik.' -495.229 (df=5)

## Comparison with the aggregated fit

[`summary`](https://rdrr.io/r/base/summary.html)`(``agg_model``)`

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

[`cat`](https://rdrr.io/r/base/cat.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"logLik(distributed encrypted): %f\n"``, `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`logLik`](https://rdrr.io/r/stats/logLik.html)`(``fit``)``)``)``)`

    ## logLik(distributed encrypted): -495.229022

[`cat`](https://rdrr.io/r/base/cat.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"logLik(aggregated cleartext) : %f\n"``, ``agg_model``$``loglik``[``2``]``)``)`

    ## logLik(aggregated cleartext) : -495.229022

The estimates and standard errors agree to high precision: the encrypted
distributed fit reproduces the aggregated cleartext fit without any site
revealing its raw patient data.

| coefficient | encrypted_distributed | aggregated_cleartext |  abs_diff |
|:------------|----------------------:|---------------------:|----------:|
| GCB_sig     |            -0.2638698 |           -0.2638716 | 1.822e-06 |
| LN_sig      |            -0.2543587 |           -0.2543592 | 5.340e-07 |
| Prolif_sig  |             0.3031250 |            0.3031258 | 7.480e-07 |
| BMP6        |             0.3036367 |            0.3036375 | 7.940e-07 |
| MHC2_sig    |            -0.3191459 |           -0.3191467 | 8.420e-07 |

Encrypted-distributed BFGS vs. aggregated-cleartext coxph() {.table}

## What just happened

We took [`stats4::mle()`](https://rdrr.io/r/stats4/mle.html) — a generic
R optimizer with no awareness of cryptography — and handed it a function
that computes its argument through a three-party homomorphic-encryption
protocol. The optimizer ran its normal BFGS loop, asked for the value of
the negative log-likelihood at a handful of points in $`\mathbb{R}^5`$,
and at each point our function performed a CKKS master/worker round
across three hospital sites and returned a single decrypted scalar.

[`mle()`](https://rdrr.io/r/stats4/mle.html) does not know — and does
not need to know — that any of this happened. From its perspective it
called a function and got a number. The result matches the aggregated
[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) fit at the
precision the optimizer cares about.

## A note on the comparison

[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) uses
Newton-Raphson with the **analytic gradient and Hessian** of the Cox
partial log-likelihood — a scheme-specific advantage. We use BFGS with
**numerical finite-difference gradients**, which is the fair comparison
given that we’re driving a generic optimizer through an encrypted
channel. CKKS itself introduces no measurable error at the parameter
sizes we use.

## What this demonstrates

1.  **Existing R optimizers work over encrypted channels.**
    Privacy-preserving distributed statistics does not require rewriting
    `mle`, `optim`, `nlm`, `glm`’s fitter, MCMC samplers, or any other
    established R machinery. As long as the routine accepts a likelihood
    (or loss) as a callback, you can drop in an FHE-aware callback and
    the routine will just work.
2.  **Stratified Cox regression decomposes additively** across strata,
    so the same master/worker scheme used for Poisson MLE
    ([`vignette("mle")`](https://bnaras.github.io/homomorpheR/articles/mle.md))
    works verbatim for survival analysis. Only the local computation
    changes (`coxph` instead of `dpois`).
3.  **CKKS handles real-valued partial log-likelihoods directly.** The
    Paillier-based vignettes in this package have to split each value
    into integer and fractional parts and rationally approximate the
    fractional part with a denominator of $`2^{256}`$. With CKKS, the
    protocol becomes pure ciphertext arithmetic.
4.  **The master/worker classes are reusable.** The same exported `Site`
    / `Master` classes and
    [`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
    runner drive both this Cox vignette and the Poisson MLE vignette —
    only the per-worker `local_fn` differs.

## Caveats and extensions

- **Performance**: each function evaluation requires three CKKS
  encryptions, three ciphertext additions, and one decryption. CKKS
  encrypt/decrypt dominates the wall-clock cost.
- **Information leakage**: the master sees the *value* of the joint
  log-likelihood at each $`\beta`$, which is more information than
  individual contributions but is what
  [`mle()`](https://rdrr.io/r/stats4/mle.html) needs. To hide even this
  would require running Newton-Raphson entirely inside the encrypted
  domain — feasible with CKKS but considerably more complex.
- **Threshold key generation**: in a real deployment, the secret key
  would be split across the sites (n-of-n threshold) so that no single
  party — not even the master — can decrypt intermediate ciphertexts
  unilaterally. The next vignette
  ([`vignette("cox-threshold")`](https://bnaras.github.io/homomorpheR/articles/cox-threshold.md))
  builds on this one and adds threshold key generation.
- **Beyond Cox**: any likelihood whose log decomposes additively across
  data partitions admits this exact protocol — generalized linear
  models, mixed-effects models with site-specific random effects,
  frailty survival models. The structural pattern is identical; only the
  local likelihood evaluation changes.

Bayle, Pierre, Jianqing Fan, and Zhipeng Lou. 2025.
“Communication-Efficient Distributed Estimation and Inference for Cox’s
Model.” *Journal of the American Statistical Association*, ahead of
print. <https://doi.org/10.1080/01621459.2025.2516820>.

Rosenwald, Andreas, George Wright, Wing C. Chan, et al. 2002. “The Use
of Molecular Profiling to Predict Survival After Chemotherapy for
Diffuse Large-B-Cell Lymphoma.” *New England Journal of Medicine* 346
(25): 1937–47. <https://doi.org/10.1056/NEJMoa012914>.
