# Distributed Stratified Cox Regression

## The statistical problem

The Cox proportional hazards model is widely used in medical statistics.
Given covariates \\x_i\\ for subject \\i\\, an event time \\t_i\\, and
an event indicator \\\delta_i\\, the hazard is modeled as

\\ h(t \mid x_i) \\=\\ h_0(t) \\ \exp(\beta^\top x_i) \\

where \\h_0(t)\\ is an unspecified baseline hazard and \\\beta\\ is the
vector of regression coefficients we want to estimate. The **partial
log-likelihood** depends only on \\\beta\\:

\\ \ell(\beta) \\=\\ \sum\_{i: \delta_i = 1} \left\[\\ \beta^\top x_i
\\-\\ \log\\\\\sum\_{j \in R_i} \exp(\beta^\top x_j) \\\right\] \\

where \\R_i\\ is the risk set at time \\t_i\\.

For **stratified** Cox regression — when baseline hazards differ across
strata (e.g. across study sites) but the coefficients \\\beta\\ are
shared — the partial log-likelihood becomes a sum over strata:

\\ \ell(\beta) \\=\\ \sum\_{s=1}^{S} \ell_s(\beta). \\

Because the log-likelihood is a sum over strata, each site can compute
its own term. The master/worker setup used by `distcomp`, `DataSHIELD`,
and `WebDISCO` relies on this: a master sends the current \\\beta\\ to
the sites, each site computes \\\ell_s(\beta)\\ on its own data, and the
master adds the results. We use the same setup, but the sites encrypt
their terms under CKKS, so the master sees only the sum and not the
individual terms.

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
Type III), and we treat each subgroup as a site. The three sites differ
in size (GCB \\n=115\\, ABC \\n=71\\, Type III \\n=49\\, with 54, 49,
and 30 deaths respectively); the protocol does not require equal sizes.

\
[`suppressPackageStartupMessages`](https://rdrr.io/r/base/message.html)`(`[`library`](https://rdrr.io/r/base/library.html)`(`[`survival`](https://github.com/therneau/survival)`)``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`homomorpheR`](https://bnaras.github.io/homomorpheR/)`)`\
[`data`](https://rdrr.io/r/utils/data.html)`(``DLBCL``)`\
\
`cox_data`` ``<-`` `[`split`](https://rdrr.io/r/base/split.html)`(`\
`  ``DLBCL``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"time"``, ``"status"``, ``"GCB_sig"``, ``"LN_sig"``,`\
`            ``"Prolif_sig"``, ``"BMP6"``, ``"MHC2_sig"``, ``"Subgroup"``)``]``,`\
`  ``DLBCL``$``Subgroup``)`\
[`sapply`](https://rdrr.io/r/base/lapply.html)`(``cox_data``, ``function``(``df``)`` `[`c`](https://rdrr.io/r/base/c.html)`(``n ``=`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``df``)``, events ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``df``$``status``)``)``)`

    ##        GCB ABC Type III
    ## n      115  71       49
    ## events  54  49       30

## The aggregated fit

If all data were in one place, we would fit the stratified Cox model
directly:

\
`agg_model`` ``<-`` `[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html)`(`[`Surv`](https://rdrr.io/pkg/survival/man/Surv.html)`(``time``, ``status``)`` ``~`` ``GCB_sig`` ``+`` ``LN_sig`` ``+`\
`                       ``Prolif_sig`` ``+`` ``BMP6`` ``+`` ``MHC2_sig`` ``+`\
`                       `[`strata`](https://rdrr.io/pkg/survival/man/strata.html)`(``Subgroup``)``,`\
`                   data ``=`` ``DLBCL``)`\
`agg_model`

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

\
`agg_model``$``loglik`

    ## [1] -516.5986 -495.2290

The first log-likelihood is at \\\beta = 0\\ (the null model); the
second is at the MLE. The goal is to reproduce these estimates without
the three sites pooling their data.

## The protocol

We use the same master/worker topology as the MLE vignette: master
broadcasts \\\beta\\, each worker computes its local Cox partial
log-likelihood at \\\beta\\, encrypts it under the master’s public key,
and returns the encrypted value. The master sums the encrypted
contributions homomorphically and decrypts the total. Mathematically
nothing changes from the MLE case; only the local computation differs.

The local computation uses a feature of
[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html): with
`iter.max = 0` it returns the partial log-likelihood at the supplied
`init` without taking any Newton-Raphson steps.

\
`cph_control`` ``<-`` `[`replace`](https://rdrr.io/r/base/replace.html)`(`[`coxph.control`](https://rdrr.io/pkg/survival/man/coxph.control.html)`(``)``, ``"iter.max"``, ``0``)`\
\
`local_cox_nll`` ``<-`` ``function``(``data``, ``beta``)`` ``{`\
`    ``fit`` ``<-`` `[`tryCatch`](https://rdrr.io/r/base/conditions.html)`(`\
`        `[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html)`(`[`Surv`](https://rdrr.io/pkg/survival/man/Surv.html)`(``time``, ``status``)`` ``~`` ``GCB_sig`` ``+`` ``LN_sig`` ``+`` ``Prolif_sig`` ``+`\
`                  ``BMP6`` ``+`` ``MHC2_sig``,`\
`              data    ``=`` ``data``,`\
`              init    ``=`` ``beta``,`\
`              control ``=`` ``cph_control``)``,`\
`        error ``=`` ``function``(``e``)`` ``NULL``)`\
`    ``if`` ``(`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``fit``)``)`` ``NA_real_`` ``else`` ``-``fit``$``loglik``[``1``]`\
`}`

`tryCatch` returns `NA_real_` if the local fit fails at an extreme
\\\beta\\.
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
passes that `NA` back to the optimizer, which then tries a shorter step.

## Wiring up the protocol

The summed negative log-likelihood on this cohort is 517 at \\\beta =
0\\ and 495 at the MLE. CKKS represents values of this size at the
default scaling parameters. We raise `scaling_mod_size` from 50 to 59
for extra precision and set `first_mod_size = 60`, the library default,
explicitly.

\
`cc`` ``<-`` ``openfhe.R``::`[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`\
`                           multiplicative_depth ``=`` ``1L``,`\
`                           scaling_mod_size     ``=`` ``59L``,`\
`                           first_mod_size       ``=`` ``60L``,`\
`                           batch_size           ``=`` ``8L``)`\
`keys`` ``<-`` ``openfhe.R``::`[`key_gen`](https://openfheorg.github.io/openfhe.R/reference/key_gen.html)`(``cc``)`\
\
`worker_gcb`` ``<-`` `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``name ``=`` ``"GCB"``,      data ``=`` ``cox_data``[[``"GCB"``]``]``,`\
`                          contribution_fn ``=`` ``local_cox_nll``)`\
`worker_abc`` ``<-`` `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``name ``=`` ``"ABC"``,      data ``=`` ``cox_data``[[``"ABC"``]``]``,`\
`                          contribution_fn ``=`` ``local_cox_nll``)`\
`worker_t3``  ``<-`` `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``name ``=`` ``"Type III"``, data ``=`` ``cox_data``[[``"Type III"``]``]``,`\
`                          contribution_fn ``=`` ``local_cox_nll``)`\
`master``     ``<-`` `[`make_ckks_master`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)`(``"Master"``, crypto_context ``=`` ``cc``, keypair ``=`` ``keys``)`\
[`set_workers`](https://bnaras.github.io/homomorpheR/reference/set_workers.md)`(``master``, `[`list`](https://rdrr.io/r/base/list.html)`(``worker_gcb``, ``worker_abc``, ``worker_t3``)``)`

## Iterative MLE through the encrypted protocol

We hand [`stats4::mle()`](https://rdrr.io/r/stats4/mle.html) a function
that looks like a standard multivariate negative log-likelihood. Each
call drives one master/worker round and returns a single decrypted
scalar.

\
[`library`](https://rdrr.io/r/base/library.html)`(``stats4``)`\
\
`encrypted_nLL`` ``<-`` ``function``(``GCB_sig``, ``LN_sig``, ``Prolif_sig``, ``BMP6``, ``MHC2_sig``)`` ``{`\
`    `[`master_aggregate`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)`(``master``, `[`c`](https://rdrr.io/r/base/c.html)`(``GCB_sig``, ``LN_sig``, ``Prolif_sig``, ``BMP6``, ``MHC2_sig``)``)`\
`}`\
\
`fit`` ``<-`` `[`mle`](https://rdrr.io/r/stats4/mle.html)`(``encrypted_nLL``,`\
`           start   ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``GCB_sig ``=`` ``0``, LN_sig ``=`` ``0``, Prolif_sig ``=`` ``0``,`\
`                          BMP6    ``=`` ``0``, MHC2_sig ``=`` ``0``)``,`\
`           method  ``=`` ``"BFGS"``,`\
`           control ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``reltol ``=`` ``1e-7``)``)`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``fit``)`\
[`logLik`](https://rdrr.io/r/stats/logLik.html)`(``fit``)`

    ##              Estimate Std. Error
    ## GCB_sig    -0.2638698 0.11940447
    ## LN_sig     -0.2543587 0.08515178
    ## Prolif_sig  0.3031250 0.14981284
    ## BMP6        0.3036367 0.10727837
    ## MHC2_sig   -0.3191459 0.09412946

    ## 'log Lik.' -495.229022 (df=5)

## Comparison with the cleartext fit

To check the encrypted fit, we run the identical
[`mle()`](https://rdrr.io/r/stats4/mle.html) objective a second time
with the encrypted aggregation replaced by an ordinary sum of the three
sites’ cleartext values: same likelihood, same optimizer, same starting
values and tolerance, no encryption.

\
[`library`](https://rdrr.io/r/base/library.html)`(``stats4``)`\
\
`plain_nLL`` ``<-`` ``function``(``GCB_sig``, ``LN_sig``, ``Prolif_sig``, ``BMP6``, ``MHC2_sig``)`` ``{`\
`    ``beta`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``GCB_sig``, ``LN_sig``, ``Prolif_sig``, ``BMP6``, ``MHC2_sig``)`\
`    `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`vapply`](https://rdrr.io/r/base/lapply.html)`(``cox_data``, ``local_cox_nll``, `[`numeric`](https://rdrr.io/r/base/numeric.html)`(``1``)``, beta ``=`` ``beta``)``)`\
`}`\
\
`fit_plain`` ``<-`` `[`mle`](https://rdrr.io/r/stats4/mle.html)`(``plain_nLL``,`\
`                 start   ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``GCB_sig ``=`` ``0``, LN_sig ``=`` ``0``, Prolif_sig ``=`` ``0``,`\
`                                BMP6    ``=`` ``0``, MHC2_sig ``=`` ``0``)``,`\
`                 method  ``=`` ``"BFGS"``,`\
`                 control ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``reltol ``=`` ``1e-7``)``)`

| Coefficient | \\\hat\beta\\, [`mle()`](https://rdrr.io/r/stats4/mle.html) encrypted | \\\hat\beta\\, [`mle()`](https://rdrr.io/r/stats4/mle.html) cleartext | \\\lvert \text{difference} \rvert\\ |
|:---|---:|---:|---:|
| GCB_sig | -0.2638698 | -0.2638698 | \\4.76 \times 10^{-10}\\ |
| LN_sig | -0.2543587 | -0.2543587 | \\1.47 \times 10^{-10}\\ |
| Prolif_sig | 0.3031250 | 0.3031250 | \\2.26 \times 10^{-10}\\ |
| BMP6 | 0.3036367 | 0.3036367 | \\7.38 \times 10^{-11}\\ |
| MHC2_sig | -0.3191459 | -0.3191459 | \\1.70 \times 10^{-10}\\ |

Single-decrypter CKKS DLBCL Cox against the same objective evaluated in
the clear. {.table .table .table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

The encrypted fit agrees with the cleartext fit to within 4.76e-10 in
every coefficient.

## What just happened

[`stats4::mle()`](https://rdrr.io/r/stats4/mle.html) ran its usual BFGS
iterations. Each time it asked for the negative log-likelihood at a
point in \\\mathbb{R}^5\\, the function ran one CKKS master/worker round
across the three sites and returned one decrypted number.
[`mle()`](https://rdrr.io/r/stats4/mle.html) was not modified, and its
result matches the cleartext fit of the same objective.

## What this demonstrates

1.  **R optimizers work unchanged.** Any routine that takes the
    objective as a function, such as
    [`mle()`](https://rdrr.io/r/stats4/mle.html),
    [`optim()`](https://rdrr.io/r/stats/optim.html), or
    [`nlm()`](https://rdrr.io/r/stats/nlm.html), can be given one that
    computes its value through the encrypted protocol.
2.  **Stratified Cox regression decomposes additively** across strata,
    so the master/worker scheme used for Poisson MLE
    ([`vignette("mle")`](https://bnaras.github.io/homomorpheR/articles/mle.md))
    works unchanged for survival analysis. Only the local computation
    changes (`coxph` instead of `dpois`).
3.  **CKKS encrypts real numbers directly.** The earlier Paillier code
    had to split each value into integer and fractional parts and
    approximate the fractional part as a fraction with denominator
    \\2^{256}\\. CKKS needs no such encoding.
4.  **The master/worker classes are reusable.** The same exported `Site`
    / `Master` classes and
    [`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
    runner drive both this Cox vignette and the Poisson MLE vignette —
    only the per-worker `contribution_fn` differs.

## Caveats and extensions

- **Performance**: each function evaluation requires three CKKS
  encryptions, three encrypted additions, and one decryption. CKKS
  encrypt/decrypt dominates the wall-clock cost.
- **Information leakage**: the master sees the value of the joint
  log-likelihood at each \\\beta\\. That is less than the individual
  contributions, and it is what
  [`mle()`](https://rdrr.io/r/stats4/mle.html) needs. Hiding it as well
  would require running Newton-Raphson on encrypted values, which CKKS
  allows but which is considerably more complex.
- **Threshold key generation**: in a real deployment the secret key
  would be split across the sites (n-of-n threshold), so that no single
  party, the master included, can decrypt intermediate values on its
  own.
  [`vignette("cox-threshold")`](https://bnaras.github.io/homomorpheR/articles/cox-threshold.md)
  adds this.
- **Beyond Cox**: the same protocol applies to any model whose
  log-likelihood is a sum over data partitions, such as generalized
  linear models, mixed-effects models with site-specific random effects,
  and frailty survival models. Only the local likelihood evaluation
  changes.

Bayle, Pierre, Jianqing Fan, and Zhipeng Lou. 2025.
“Communication-Efficient Distributed Estimation and Inference for Cox’s
Model.” *Journal of the American Statistical Association*, ahead of
print. <https://doi.org/10.1080/01621459.2025.2516820>.

Rosenwald, Andreas, George Wright, Wing C. Chan, et al. 2002. “The Use
of Molecular Profiling to Predict Survival After Chemotherapy for
Diffuse Large-B-Cell Lymphoma.” *New England Journal of Medicine* 346
(25): 1937–47. <https://doi.org/10.1056/NEJMoa012914>.
