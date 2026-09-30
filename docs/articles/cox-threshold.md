# Distributed Cox Regression with Threshold Key Generation

## Introduction

The `cox` vignette fits a stratified Cox model across three sites under
CKKS. There the master holds the secret key and decrypts the encrypted
sum at every iteration of the optimizer, so the master must be trusted
not to decrypt anything else, such as a single site’s contribution.

In this vignette the three sites generate the CKKS key pair jointly, and
each keeps its own share of the secret key. No single party holds the
whole key. The master becomes an aggregator: it adds the encrypted
contributions and collects the sites’ partial decryptions, but it cannot
decrypt anything by itself.

The data and the model are the same as in
[`vignette("cox")`](https://bnaras.github.io/homomorpheR/articles/cox.md).
Only who can decrypt changes.

## Threat model

Three sites and one untrusted aggregator:

- **Sites \\1, 2, 3\\** each hold private patient data and a secret key
  share \\\mathit{sk}\_i\\. They are honest-but-curious among themselves
  and toward the aggregator.
- **Aggregator** holds no secret-key material. It receives the encrypted
  contributions, adds them, and sends the sum back to the sites for
  partial decryption. The encrypted values it handles tell it nothing on
  their own.

What the aggregator sees, by stage:

1.  Encrypted local contributions \\\mathit{ct}\_i =
    E\_{\mathit{pk}\_{1..n}}(\ell_i)\\. None decryptable alone.
2.  The encrypted sum \\\mathit{ct}\_{\text{sum}} = \boxplus_i
    \mathit{ct}\_i\\. Not decryptable alone.
3.  Partial decryptions \\\rho_i\\ contributed by each site. Not
    decryptable individually.
4.  After combining the partial decryptions, the sum \\\ell(\beta) =
    \sum_i \ell_i\\ in the clear.

Step 4 reveals \\\ell(\beta)\\ to the aggregator, which is what the
master saw in
[`vignette("cox")`](https://bnaras.github.io/homomorpheR/articles/cox.md).
The difference is that no single party can decrypt an individual
contribution or any intermediate value. That takes a partial decryption
from every site.

## The Cox setup (same DLBCL data as `cox.Rmd`)

\
[`suppressPackageStartupMessages`](https://rdrr.io/r/base/message.html)`(`[`library`](https://rdrr.io/r/base/library.html)`(`[`survival`](https://github.com/therneau/survival)`)``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`homomorpheR`](https://bnaras.github.io/homomorpheR/)`)`\
[`data`](https://rdrr.io/r/utils/data.html)`(``DLBCL``)`\
\
`cox_data`` ``<-`` `[`split`](https://rdrr.io/r/base/split.html)`(`\
`  ``DLBCL``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"time"``, ``"status"``, ``"GCB_sig"``, ``"LN_sig"``,`\
`            ``"Prolif_sig"``, ``"BMP6"``, ``"MHC2_sig"``, ``"Subgroup"``)``]``,`\
`  ``DLBCL``$``Subgroup``)`

## The protocol

**Setup** (once):

1.  Site 1 calls `key_gen(cc)` to produce its keypair \\(\mathit{pk}\_1,
    \mathit{sk}\_1)\\.
2.  Site 2 calls `multiparty_key_gen(cc, pk_1)` to produce
    \\(\mathit{pk}\_{12}, \mathit{sk}\_2)\\.
3.  Site 3 calls `multiparty_key_gen(cc, pk_{12})` to produce
    \\(\mathit{pk}\_{123}, \mathit{sk}\_3)\\.
4.  The final \\\mathit{pk}\_{123}\\ is the **joint public key**. Each
    site keeps its own \\\mathit{sk}\_i\\.

**Per query** (called inside the optimizer):

1.  Each site \\i\\ computes its local Cox negative log-likelihood
    \\\ell_i(\beta)\\ and encrypts it under the joint public key.
2.  The aggregator sums the encrypted contributions homomorphically.
3.  Each site partial-decrypts the sum using its own \\\mathit{sk}\_i\\.
4.  The aggregator fuses the partials to recover \\\ell(\beta)\\.

[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
runs this chain across the sites in one call and returns a
`ThresholdMaster` holding the joint public key. Each site keeps the
share it generated. To decrypt, the master asks every site for a partial
decryption and combines them. This happens inside the
[`decrypt()`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)
method, so the
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
runner from `cox.Rmd` works unchanged.

## Implementation

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

The CKKS context needs the `MULTIPARTY` feature enabled so the chained
`multiparty_key_gen()` calls work:

\
`cc`` ``<-`` ``openfhe.R``::`[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`\
`                           multiplicative_depth ``=`` ``1L``,`\
`                           scaling_mod_size     ``=`` ``59L``,`\
`                           first_mod_size       ``=`` ``60L``,`\
`                           batch_size           ``=`` ``8L``,`\
`                           features             ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``openfhe.R``::`[`Feature`](https://openfheorg.github.io/openfhe.R/reference/Feature.html)`$``MULTIPARTY``)``)`

The sites come first, because the joint public key is built from them.
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
then takes the sites and returns the master.

\
`worker_gcb`` ``<-`` `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``name ``=`` ``"GCB"``,      data ``=`` ``cox_data``[[``"GCB"``]``]``,`\
`                          contribution_fn ``=`` ``local_cox_nll``)`\
`worker_abc`` ``<-`` `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``name ``=`` ``"ABC"``,      data ``=`` ``cox_data``[[``"ABC"``]``]``,`\
`                          contribution_fn ``=`` ``local_cox_nll``)`\
`worker_t3``  ``<-`` `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``name ``=`` ``"Type III"``, data ``=`` ``cox_data``[[``"Type III"``]``]``,`\
`                          contribution_fn ``=`` ``local_cox_nll``)`\
\
`master`` ``<-`` `[`make_threshold_master`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)`(``"Aggregator"``,`\
`                                crypto_context ``=`` ``cc``,`\
`                                sites ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``worker_gcb``, ``worker_abc``, ``worker_t3``)``)`

The check below confirms that the master has no property holding key
shares and that the GCB site holds its own share:

\
`share_check`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``master_holds_shares ``=`` ``"secret_keys"`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`names`](https://rdrr.io/r/base/names.html)`(``S7``::`[`props`](https://rconsortium.github.io/S7/reference/props.html)`(``master``)``)``,`\
`                 gcb_holds_own_share ``=`` ``!`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``worker_gcb``@``state``$``sk``)``)`\
`share_check`

    ## master_holds_shares gcb_holds_own_share 
    ##               FALSE                TRUE

## Iterative MLE through the threshold protocol

The optimizer code is the same as in `cox.Rmd`. Only the master class
differs.

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
    ## Prolif_sig  0.3031250 0.14981283
    ## BMP6        0.3036367 0.10727837
    ## MHC2_sig   -0.3191459 0.09412946

    ## 'log Lik.' -495.229022 (df=5)

## Comparison with the cleartext fit

As in
[`vignette("cox")`](https://bnaras.github.io/homomorpheR/articles/cox.md),
the check is the identical [`mle()`](https://rdrr.io/r/stats4/mle.html)
objective with the encrypted aggregation replaced by an ordinary sum of
the three sites’ cleartext values.

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

| Coefficient | \\\hat\beta\\, [`mle()`](https://rdrr.io/r/stats4/mle.html) threshold | \\\hat\beta\\, [`mle()`](https://rdrr.io/r/stats4/mle.html) cleartext | \\\lvert \text{difference} \rvert\\ |
|:---|---:|---:|---:|
| GCB_sig | -0.2638698 | -0.2638698 | \\1.73 \times 10^{-12}\\ |
| LN_sig | -0.2543587 | -0.2543587 | \\8.39 \times 10^{-13}\\ |
| Prolif_sig | 0.3031250 | 0.3031250 | \\5.27 \times 10^{-13}\\ |
| BMP6 | 0.3036367 | 0.3036367 | \\1.37 \times 10^{-12}\\ |
| MHC2_sig | -0.3191459 | -0.3191459 | \\2.72 \times 10^{-13}\\ |

Threshold-CKKS DLBCL Cox against the same objective evaluated in the
clear. {.table .table .table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

The threshold fit agrees with the cleartext fit to within 1.73e-12 in
every coefficient.

## Discussion

1.  **No single party holds the decryption key.** Each site generated
    its own share and kept it; the master holds only the joint public
    key, and has no property in which a share could sit. Encrypted
    intermediate values are undecryptable by any single party in the
    system, the aggregator included.
2.  **The fit does not change.** The coefficients agree with the
    cleartext fit of the same objective to CKKS precision.
3.  **Same optimizer, same callback shape, same code structure.**
    Compared to `cox.Rmd` only the setup changed: the workers are built
    first and handed to
    [`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
    instead of being wired to a
    [`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)
    afterwards, because the joint key cannot exist before the sites do.
    The optimizer sees nothing different.

## Limitations

- **The aggregator sees \\\ell(\beta)\\ at every iteration.** That is
  the function value [`mle()`](https://rdrr.io/r/stats4/mle.html) asks
  for. The individual site contributions stay hidden. Hiding
  \\\ell(\beta)\\ as well would require running the optimizer on
  encrypted values, which is possible but considerably more complex.
- **Honest-but-curious is the trust model.** Sites are assumed to follow
  the protocol. A malicious site could submit a corrupted partial
  decryption to break the fit; detecting this requires additional
  protocol machinery (commitments, zero-knowledge proofs) that this
  vignette does not implement.
- **Output privacy is unchanged.** The released coefficients
  \\\hat\beta\\ are the same as the cleartext fit. Output-level attacks
  (membership inference, model inversion) remain in scope and motivate
  the differential-privacy demonstrations elsewhere in the package.
