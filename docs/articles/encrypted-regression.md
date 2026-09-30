# Encrypted Logistic Regression Prediction

## Overview

The `secure-inference` vignette evaluated a linear model on encrypted
patient data. Here the prediction also needs the logistic sigmoid

\\\sigma(\eta) = \frac{1}{1 + e^{-\eta}}.\\

The sigmoid is not a polynomial, and BFV/BGV and CKKS support only
addition and multiplication. We replace it with a Chebyshev polynomial
approximation, so the whole prediction (linear predictor and sigmoid) is
computed on encrypted values.

## The setting

A hospital holds patient data. A researcher holds a trained
logistic-regression model. The researcher wants to score patients
without seeing their data; the hospital wants predictions without seeing
the model coefficients. This is the two-party setting of
`secure-inference` with a nonlinear model.

## Step 1: train in the clear

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
`n`` ``<-`` ``500`\
`age``       ``<-`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``55``, ``10``)`\
`biomarker`` ``<-`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0``, ``1``)`\
`prob``      ``<-`` `[`plogis`](https://rdrr.io/r/stats/Logistic.html)`(``-``2`` ``+`` ``0.03`` ``*`` ``age`` ``+`` ``0.8`` ``*`` ``biomarker``)`\
`outcome``   ``<-`` `[`rbinom`](https://rdrr.io/r/stats/Binomial.html)`(``n``, ``1``, ``prob``)`\
\
`model`` ``<-`` `[`glm`](https://rdrr.io/r/stats/glm.html)`(``outcome`` ``~`` ``age`` ``+`` ``biomarker``, family ``=`` ``binomial``)`\
`beta``  ``<-`` `[`coef`](https://rdrr.io/r/stats/coef.html)`(``model``)`\
[`cat`](https://rdrr.io/r/base/cat.html)`(``"Coefficients (intercept, age, biomarker):"``, `[`round`](https://rdrr.io/r/base/Round.html)`(``beta``, ``4``)``, ``"\n"``)`

    ## Coefficients (intercept, age, biomarker): -2.2671 0.0348 0.8991

## Step 2: encrypt patient data

The Chebyshev polynomial needs enough multiplicative depth. We use depth
8 and enable `Feature$ADVANCEDSHE`, which provides the
polynomial-evaluation functions.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`openfhe.R`](https://openfheorg.github.io/openfhe.R/)`)`\
\
`cc`` ``<-`` `[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`\
`                  multiplicative_depth ``=`` ``8L``,`\
`                  scaling_mod_size     ``=`` ``50L``,`\
`                  batch_size           ``=`` ``16L``,`\
`                  features             ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``Feature``$``ADVANCEDSHE``)``)`\
`keys`` ``<-`` `[`key_gen`](https://openfheorg.github.io/openfhe.R/reference/key_gen.html)`(``cc``, eval_mult ``=`` ``TRUE``)`\
\
`new_age`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``45``, ``52``, ``60``, ``38``, ``70``, ``55``, ``48``, ``63``,`\
`             ``41``, ``57``, ``66``, ``44``, ``72``, ``50``, ``59``, ``35``)`\
`new_bm``  ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``0.5``,  ``0.3``, ``1.2``, ``-``1.0``, ``0.8``, ``0.1``, ``-``0.3``, ``1.5``,`\
`             ``-``0.8``,  ``0.6``, ``0.9``, ``-``0.4``, ``1.1``, ``0.0``,  ``0.7``, ``-``1.2``)`\
\
`ct_age`` ``<-`` `[`encrypt`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)`(``keys``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``new_age``)``, cc ``=`` ``cc``)`\
`ct_bm``  ``<-`` `[`encrypt`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)`(``keys``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``new_bm``)``,  cc ``=`` ``cc``)`

## Step 3: evaluate the linear predictor (encrypted)

\\\eta = \beta_0 + \beta_1\\ \text{age} + \beta_2\\ \text{biomarker}\\,
all arithmetic on encrypted data:

\
`ct_eta`` ``<-`` ``ct_age`` ``*`` ``beta``[``2``]`\
`ct_eta`` ``<-`` ``ct_eta`` ``+`` ``ct_bm`` ``*`` ``beta``[``3``]`\
`ct_eta`` ``<-`` ``ct_eta`` ``+`` ``beta``[``1``]`

## Step 4: apply the sigmoid (encrypted)

`openfhe.R` exposes Chebyshev approximations of common transcendental
functions, including the logistic sigmoid. The interval \\\[a, b\]\\
must cover the range of \\\eta\\ for these patients. A degree-16
approximation fits within the depth set above; its error is measured in
Step 5.

\
`ct_prob`` ``<-`` `[`eval_logistic`](https://openfheorg.github.io/openfhe.R/reference/eval_logistic.html)`(``ct_eta``, a ``=`` ``-``4``, b ``=`` ``4``, degree ``=`` ``16``)`

## Step 5: decrypt predictions

\
`result`` ``<-`` `[`decrypt`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)`(``ct_prob``, ``keys``@``secret``, cc ``=`` ``cc``)`\
[`set_length`](https://openfheorg.github.io/openfhe.R/reference/set_length.html)`(``result``, ``16L``)`\
`encrypted_probs`` ``<-`` `[`get_real_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_real_packed_value.html)`(``result``)``[``1``:``16``]`\
\
`cleartext_probs`` ``<-`` `[`plogis`](https://rdrr.io/r/stats/Logistic.html)`(``beta``[``1``]`` ``+`` ``beta``[``2``]`` ``*`` ``new_age`` ``+`` ``beta``[``3``]`` ``*`` ``new_bm``)`\
`max_err`` ``<-`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``encrypted_probs`` ``-`` ``cleartext_probs``)``)`\
[`cat`](https://rdrr.io/r/base/cat.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"Max absolute error vs cleartext sigmoid: %.2e\n"``, ``max_err``)``)`

    ## Max absolute error vs cleartext sigmoid: 5.79e-06

## Summary

1.  Model coefficients are applied to encrypted data. The researcher
    never sees the patient data. The hospital decrypts the predictions
    but is not sent the coefficients, as in `secure-inference`; see the
    threat-model caveat below.
2.  The sigmoid is evaluated on encrypted values. There is no
    intermediate decryption and no exchange between the parties during
    the computation.
3.  The CKKS error is small. The maximum absolute error against the
    cleartext sigmoid is 5.8 × 10⁻⁶.

## Limitations

- **Multiplicative depth**: each multiplication uses one level of the
  depth set in the context. Deeper computations need larger parameters
  and more memory.
- **Approximation error**: CKKS is approximate. For deployment,
  precision would need to be validated for the use case.
- **Performance**: encrypted computation is orders of magnitude slower
  than cleartext.
- **Threat model**: the model-extraction caveat of
  [`vignette("secure-inference")`](https://bnaras.github.io/homomorpheR/articles/secure-inference.md)
  applies here. The hospital holds the secret key and could recover the
  coefficients from predictions on chosen inputs. The sigmoid makes this
  harder than in the linear case but does not prevent it. A deployment
  would need further protection, such as output differential privacy,
  query limits, or threshold FHE.
