# Encrypted Logistic Regression Prediction

## Overview

The `secure-inference` vignette showed a *linear* model evaluated on
encrypted patient data. This vignette goes one step further: the
prediction needs the logistic sigmoid

``` math
\sigma(\eta) = \frac{1}{1 + e^{-\eta}},
```

a non-polynomial function that on its face cannot be evaluated under
encryption — both BFV/BGV and CKKS only support polynomial arithmetic.
Using a Chebyshev polynomial approximation of the sigmoid, however, we
can evaluate the entire logistic prediction (linear predictor *and*
sigmoid) under encryption with only addition and multiplication.

## The setting

A hospital holds patient data. A researcher holds a trained
logistic-regression model. The researcher wants to score patients
without seeing their data; the hospital wants predictions without seeing
the model coefficients. The same two-party shape as `secure-inference`,
with a non-linear scorer.

## Step 1: train in the clear

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`` ``n`` ``<-`` ``500`` ``age`` ``<-`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``55``, ``10``)`` ``biomarker`` ``<-`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0``, ``1``)`` ``prob`` ``<-`` `[`plogis`](https://rdrr.io/r/stats/Logistic.html)`(``-``2`` ``+`` ``0.03`` ``*`` ``age`` ``+`` ``0.8`` ``*`` ``biomarker``)`` ``outcome`` ``<-`` `[`rbinom`](https://rdrr.io/r/stats/Binomial.html)`(``n``, ``1``, ``prob``)`` `` ``model`` ``<-`` `[`glm`](https://rdrr.io/r/stats/glm.html)`(``outcome`` ``~`` ``age`` ``+`` ``biomarker``, family ``=`` ``binomial``)`` ``beta`` ``<-`` `[`coef`](https://rdrr.io/r/stats/coef.html)`(``model``)`` `[`cat`](https://rdrr.io/r/base/cat.html)`(``"Coefficients (intercept, age, biomarker):"``, `[`round`](https://rdrr.io/r/base/Round.html)`(``beta``, ``4``)``, ``"\n"``)`

    ## Coefficients (intercept, age, biomarker): -2.2671 0.0348 0.8991

## Step 2: encrypt patient data

CKKS needs enough precision budget for the Chebyshev polynomial. We use
depth 8 and enable `Feature$ADVANCEDSHE` for the polynomial-evaluation
primitives.

[`library`](https://rdrr.io/r/base/library.html)`(`[`openfhe.R`](https://openfheorg.github.io/openfhe.R/)`)`` `` ``cc`` ``<-`` `[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`` `` multiplicative_depth ``=`` ``8L``,`` `` scaling_mod_size ``=`` ``50L``,`` `` batch_size ``=`` ``16L``,`` `` features ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``Feature``$``ADVANCEDSHE``)``)`` ``keys`` ``<-`` `[`key_gen`](https://openfheorg.github.io/openfhe.R/reference/key_gen.html)`(``cc``, eval_mult ``=`` ``TRUE``)`` `` ``new_age`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``45``, ``52``, ``60``, ``38``, ``70``, ``55``, ``48``, ``63``,`` `` ``41``, ``57``, ``66``, ``44``, ``72``, ``50``, ``59``, ``35``)`` ``new_bm`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``0.5``, ``0.3``, ``1.2``, ``-``1.0``, ``0.8``, ``0.1``, ``-``0.3``, ``1.5``,`` `` ``-``0.8``, ``0.6``, ``0.9``, ``-``0.4``, ``1.1``, ``0.0``, ``0.7``, ``-``1.2``)`` `` ``ct_age`` ``<-`` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``keys``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``new_age``)``, cc ``=`` ``cc``)`` ``ct_bm`` ``<-`` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``keys``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``new_bm``)``, cc ``=`` ``cc``)`

## Step 3: evaluate the linear predictor (encrypted)

$`\eta = \beta_0 + \beta_1\, \text{age} + \beta_2\, \text{biomarker}`$,
all arithmetic on encrypted data:

`ct_eta`` ``<-`` ``ct_age`` ``*`` ``beta``[``2``]`` ``ct_eta`` ``<-`` ``ct_eta`` ``+`` ``ct_bm`` ``*`` ``beta``[``3``]`` ``ct_eta`` ``<-`` ``ct_eta`` ``+`` ``beta``[``1``]`

## Step 4: apply the sigmoid (encrypted)

`openfhe.R` exposes Chebyshev approximations of common transcendental
functions, including the logistic sigmoid. The interval $`[a, b]`$ must
cover the realizable range of $`\eta`$; degree 16 gives good precision
with depth modest enough to fit the budget we declared above.

`ct_prob`` ``<-`` `[`eval_logistic`](https://openfheorg.github.io/openfhe.R/reference/eval_logistic.html)`(``ct_eta``, a ``=`` ``-``4``, b ``=`` ``4``, degree ``=`` ``16``)`

## Step 5: decrypt predictions

`result`` ``<-`` `[`decrypt`](https://bnaras.github.io/homomorpheR/reference/decrypt.md)`(``ct_prob``, ``keys``@``secret``, cc ``=`` ``cc``)`` `[`set_length`](https://openfheorg.github.io/openfhe.R/reference/set_length.html)`(``result``, ``16L``)`` ``encrypted_probs`` ``<-`` `[`get_real_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_real_packed_value.html)`(``result``)``[``1``:``16``]`` `` ``cleartext_probs`` ``<-`` `[`plogis`](https://rdrr.io/r/stats/Logistic.html)`(``beta``[``1``]`` ``+`` ``beta``[``2``]`` ``*`` ``new_age`` ``+`` ``beta``[``3``]`` ``*`` ``new_bm``)`` ``max_err`` ``<-`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``encrypted_probs`` ``-`` ``cleartext_probs``)``)`` `[`cat`](https://rdrr.io/r/base/cat.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"Max absolute error vs cleartext sigmoid: %.2e\n"``, ``max_err``)``)`

    ## Max absolute error vs cleartext sigmoid: 5.79e-06

## What this demonstrates

1.  **Model coefficients applied to encrypted data.** The hospital never
    sees the model; the researcher never sees the patient data. Same
    transport guarantee as `secure-inference`.
2.  **The sigmoid evaluated entirely under encryption.** No intermediate
    decryption, no protocol back-and-forth — the whole logistic
    prediction completes while the data stays encrypted.
3.  **CKKS precision is more than enough for medical prediction.** The
    maximum absolute error against the cleartext sigmoid is 5.8^{-6},
    well below any threshold that would matter for a clinical decision.

## Connection to homomorpheR’s Paillier vignettes

The Paillier vignettes (`vignette("homomorphing")`, `vignette("DHCox")`,
`vignette("DHCoxNCP")`) demonstrate distributed Cox regression and MLE
under additive Paillier encryption. That approach requires:

- sending encrypted *sufficient statistics* (sums) between sites rather
  than evaluating the full likelihood under encryption,
- iterative protocols where the master site decrypts intermediate
  aggregates,
- careful real-number encoding via integer / fractional split.

With CKKS via `openfhe.R`:

- real numbers are encrypted directly — no manual scaling,
- multiplication works — enabling polynomial and sigmoid evaluation
  directly under encryption,
- the full computation can happen in the encrypted domain.

## Limitations

- **Precision budget**: each multiplication consumes one level of the
  encrypted value’s budget. Deeper computations require larger
  parameters and more memory.
- **Approximation error**: CKKS is approximate. For deployment, validate
  that the precision is sufficient for your use case.
- **Performance**: encrypted computation is orders of magnitude slower
  than cleartext.
- **Threat model**: this transport-layer demonstration shares the
  model-extraction caveat from
  [`vignette("secure-inference")`](https://bnaras.github.io/homomorpheR/articles/secure-inference.md).
  The hospital holding the secret key can in principle recover the
  coefficients by issuing carefully chosen queries. The Chebyshev
  sigmoid makes the recovery somewhat less trivial than the linear case,
  but does not eliminate the issue. Real deployments still need defences
  (output DP, query budgets, threshold FHE) on top of the FHE transport.
