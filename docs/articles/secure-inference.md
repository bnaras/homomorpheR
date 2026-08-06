# Secure Model Inference on Encrypted Data

## The use case

A diagnostic lab develops a proprietary scoring model for disease risk
based on a panel of biomarkers. A hospital wants to score its patients
but cannot send raw patient data to the lab in cleartext. The lab does
not want to send its model coefficients to the hospital in cleartext
either.

With FHE the hospital encrypts patient biomarkers, sends the ciphertexts
to the lab, the lab applies its weighted-sum scoring directly on
ciphertexts, and the hospital decrypts the scores. The biomarker values
never appear in cleartext on the lab’s machine, and the lab’s
coefficients are never sent to the hospital. These are the *transport*
guarantees and they are what the pipeline below illustrates. They are
not the whole story of deploying a model-as-a-service — see *Threat
model: model extraction from the hospital side* below.

This vignette is the two-party companion to the multi-site master/worker
pattern
([`vignette("mle")`](https://bnaras.github.io/homomorpheR/articles/mle.md),
[`vignette("cox")`](https://bnaras.github.io/homomorpheR/articles/cox.md)).
Unlike those, there is no aggregator and no consensus: one client
(hospital) holds the secret key and queries one server (lab) which holds
the model coefficients in cleartext. The homomorphic primitives all come
from the `openfhe.R` package.

## Scenario: weighted biomarker score

The lab’s model computes a weighted score:

``` math
\text{score} = w_1 x_1 + w_2 x_2 + w_3 x_3 + w_4 x_4 + b
```

where $`x_i`$ are biomarker values and $`w_i, b`$ are proprietary
coefficients.

## Setup: hospital’s context and patient data

[`library`](https://rdrr.io/r/base/library.html)`(`[`openfhe.R`](https://openfheorg.github.io/openfhe.R/)`)`` `` ``cc`` ``<-`` `[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`` `` multiplicative_depth ``=`` ``2L``,`` `` scaling_mod_size ``=`` ``50L``,`` `` batch_size ``=`` ``8L``)`` ``keys`` ``<-`` `[`key_gen`](https://openfheorg.github.io/openfhe.R/reference/key_gen.html)`(``cc``, eval_mult ``=`` ``TRUE``)`` `` ``## 8 patients, each with 4 biomarker values. We pack each biomarker`` ``## across patients (SIMD layout): one ciphertext per biomarker, with`` ``## patient values in the slots.`` ``biomarker1`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``1.2``, ``0.8``, ``1.5``, ``0.3``, ``2.1``, ``0.9``, ``1.1``, ``1.8``)`` ``biomarker2`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.5``, ``1.1``, ``0.3``, ``0.8``, ``0.2``, ``1.4``, ``0.7``, ``0.6``)`` ``biomarker3`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.0``, ``1.5``, ``2.3``, ``1.0``, ``1.8``, ``2.1``, ``1.6``, ``2.5``)`` ``biomarker4`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.1``, ``0.4``, ``0.2``, ``0.6``, ``0.3``, ``0.1``, ``0.5``, ``0.2``)`` `` ``ct1`` ``<-`` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``keys``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``biomarker1``)``, cc ``=`` ``cc``)`` ``ct2`` ``<-`` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``keys``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``biomarker2``)``, cc ``=`` ``cc``)`` ``ct3`` ``<-`` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``keys``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``biomarker3``)``, cc ``=`` ``cc``)`` ``ct4`` ``<-`` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``keys``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``biomarker4``)``, cc ``=`` ``cc``)`

## Lab side: apply the model to encrypted data

The lab receives the encrypted biomarkers and applies its proprietary
model — without ever seeing patient values.

`## Lab's proprietary model weights (never shared with the hospital)`` ``w`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.35``, ``-``0.20``, ``0.50``, ``0.15``)`` ``b`` ``<-`` ``1.2`` `` ``## Encrypted score = w1*x1 + w2*x2 + w3*x3 + w4*x4 + b`` ``ct_score`` ``<-`` ``ct1`` ``*`` ``w``[``1``]`` ``+`` ``ct2`` ``*`` ``w``[``2``]`` ``+`` ``ct3`` ``*`` ``w``[``3``]`` ``+`` ``ct4`` ``*`` ``w``[``4``]`` ``+`` ``b`

## Hospital side: decrypt the results

`result`` ``<-`` `[`decrypt`](https://bnaras.github.io/homomorpheR/reference/decrypt.md)`(``ct_score``, ``keys``@``secret``, cc ``=`` ``cc``)`` `[`set_length`](https://openfheorg.github.io/openfhe.R/reference/set_length.html)`(``result``, ``8L``)`` ``scores`` ``<-`` `[`get_real_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_real_packed_value.html)`(``result``)``[``1``:``8``]`` `` ``for`` ``(``i`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``8``)``)`` ``{`` `` ``risk`` ``<-`` ``if`` ``(``scores``[``i``]`` ``>`` ``2.0``)`` ``"HIGH"`` `` ``else`` ``if`` ``(``scores``[``i``]`` ``>`` ``1.5``)`` ``"MODERATE"`` `` ``else`` ``"LOW"`` `` `[`cat`](https://rdrr.io/r/base/cat.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``" Patient %d: %.3f (%s)\n"``, ``i``, ``scores``[``i``]``, ``risk``)``)`` ``}`

    ##   Patient 1: 2.535 (HIGH)
    ##   Patient 2: 2.070 (HIGH)
    ##   Patient 3: 2.845 (HIGH)
    ##   Patient 4: 1.735 (MODERATE)
    ##   Patient 5: 2.840 (HIGH)
    ##   Patient 6: 2.300 (HIGH)
    ##   Patient 7: 2.320 (HIGH)
    ##   Patient 8: 2.990 (HIGH)

## Verification

`cleartext_scores`` ``<-`` ``w``[``1``]`` ``*`` ``biomarker1`` ``+`` ``w``[``2``]`` ``*`` ``biomarker2`` ``+`` `` ``w``[``3``]`` ``*`` ``biomarker3`` ``+`` ``w``[``4``]`` ``*`` ``biomarker4`` ``+`` ``b`` ``max_error`` ``<-`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``scores`` ``-`` ``cleartext_scores``)``)`` `[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"Maximum error vs cleartext: %.2e"``, ``max_error``)`

    ## [1] "Maximum error vs cleartext: 7.19e-14"

CKKS gives essentially the same answer as cleartext, within
floating-point precision.

## Transport guarantees

The pipeline above delivers two concrete protections:

1.  **Biomarker values never appear in cleartext outside the hospital.**
    The lab’s view of the protocol consists of the ciphertexts it
    received and the ciphertext it returned.
2.  **The lab’s coefficients $`w`$ and $`b`$ are never sent to the
    hospital in cleartext.** They are used only to construct the
    returned ciphertext inside the lab’s R session.

| Party | Cleartext view |
|----|----|
| Hospital | Patient biomarkers (local), decrypted scores |
| Lab | Ciphertexts only — no cleartext biomarker values, no cleartext scores |

These are necessary conditions for any model-as-a-service deployment
that does not trust the lab with cleartext patient data. They are not
sufficient conditions, as the next section shows.

## Threat model: model extraction from the hospital side

The hospital holds the secret key and decides what goes into the query
ciphertexts. Nothing in the FHE pipeline restricts the biomarker values
the hospital encrypts. For a linear model with four biomarkers and a
bias, the hospital can recover every coefficient with five queries by
submitting the standard basis:

- $`\mathbf{e}_0 = (0,0,0,0)\ \Rightarrow\ \text{score} = b`$
- $`\mathbf{e}_1 = (1,0,0,0)\ \Rightarrow\ \text{score} = w_1 + b`$
- $`\mathbf{e}_2 = (0,1,0,0)\ \Rightarrow\ \text{score} = w_2 + b`$
- $`\mathbf{e}_3 = (0,0,1,0)\ \Rightarrow\ \text{score} = w_3 + b`$
- $`\mathbf{e}_4 = (0,0,0,1)\ \Rightarrow\ \text{score} = w_4 + b`$

Subtracting the first score from each of the others recovers the four
weights exactly. We can run this attack in the same R session: wrap the
lab’s scoring pipeline as a function that closes over $`w`$ and $`b`$
without revealing them, then pack the five probes across SIMD slots 1–5
of the four biomarker ciphertexts.

`` ## Lab pipeline wrapped as a function. Closes over `w` and `b`; ``` ``## the caller (hospital) never reads either.`` ``lab_score`` ``<-`` ``function``(``ct_bio``)`` ``{`` `` ``ct_bio``[[``1``]``]`` ``*`` ``w``[``1``]`` ``+`` ``ct_bio``[[``2``]``]`` ``*`` ``w``[``2``]`` ``+`` `` ``ct_bio``[[``3``]``]`` ``*`` ``w``[``3``]`` ``+`` ``ct_bio``[[``4``]``]`` ``*`` ``w``[``4``]`` ``+`` ``b`` ``}`` `` ``## Hospital crafts five probes packed across slots 1..5.`` ``## Slot 1 is e_0 (all zeros, probes b). Slot j+1 is e_j (a one in`` ``## position j, probes w_j + b).`` ``probe_bio1`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1``, ``0``, ``0``, ``0``, ``0``, ``0``, ``0``)`` ``probe_bio2`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``1``, ``0``, ``0``, ``0``, ``0``, ``0``)`` ``probe_bio3`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``, ``1``, ``0``, ``0``, ``0``, ``0``)`` ``probe_bio4`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``, ``0``, ``1``, ``0``, ``0``, ``0``)`` `` ``ct_probe`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``keys``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``probe_bio1``)``, cc ``=`` ``cc``)``,`` `` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``keys``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``probe_bio2``)``, cc ``=`` ``cc``)``,`` `` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``keys``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``probe_bio3``)``, cc ``=`` ``cc``)``,`` `` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``keys``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``probe_bio4``)``, cc ``=`` ``cc``)`` ``)`` `` ``ct_probe_score`` ``<-`` ``lab_score``(``ct_probe``)`` ``probe_result`` ``<-`` `[`decrypt`](https://bnaras.github.io/homomorpheR/reference/decrypt.md)`(``ct_probe_score``, ``keys``@``secret``, cc ``=`` ``cc``)`` `[`set_length`](https://openfheorg.github.io/openfhe.R/reference/set_length.html)`(``probe_result``, ``5L``)`` ``probe_scores`` ``<-`` `[`get_real_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_real_packed_value.html)`(``probe_result``)``[``1``:``5``]`` `` ``b_hat`` ``<-`` ``probe_scores``[``1``]`` ``w_hat`` ``<-`` ``probe_scores``[``2``:``5``]`` ``-`` ``b_hat`` `` ``recovered`` ``<-`` `[`rbind`](https://rdrr.io/r/base/cbind.html)`(`` `` true ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``b``, ``w``)``,`` `` recovered ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``b_hat``, ``w_hat``)`` ``)`` `[`colnames`](https://rdrr.io/r/base/colnames.html)`(``recovered``)`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"b"``, ``"w1"``, ``"w2"``, ``"w3"``, ``"w4"``)`` `[`round`](https://rdrr.io/r/base/Round.html)`(``recovered``, ``6``)`

    ##             b   w1   w2  w3   w4
    ## true      1.2 0.35 -0.2 0.5 0.15
    ## recovered 1.2 0.35 -0.2 0.5 0.15

The recovered coefficients match the lab’s true coefficients to CKKS
precision. Five queries — one bias probe plus one per biomarker — are
enough because the scoring function is linear in the biomarkers; a
linear function of $`k`$ inputs is fully specified by any $`k + 1`$
affinely independent point evaluations. FHE does not impede this
extraction: the hospital is the party that decrypts, the scoring
function is the thing being released, and the standard basis is a legal
query vector.

## What this means for deployment

A deployment that needs the scoring function to stay proprietary has to
layer defences *on top of* the FHE transport:

- **Output perturbation.** Adding calibrated noise to the decrypted
  score — a differential-privacy mechanism — turns each query into a
  noisy observation rather than an exact readout. Extraction accuracy
  degrades with the noise variance and a query budget can be accounted
  for explicitly.
- **Query-budget accounting.** Capping the number of queries a client
  can issue in a session bounds how much of the model can leak before
  the budget is exhausted. This composes with output perturbation rather
  than replacing it.
- **Threshold FHE.** Splitting the secret key across several parties
  (see
  [`vignette("cox-threshold")`](https://bnaras.github.io/homomorpheR/articles/cox-threshold.md)
  for the master/worker case) changes who can decrypt and under what
  audit trail. The hospital alone can no longer mount the adaptive-query
  attack; decryption requires cooperation from the other key-holders.
- **Non-linear scoring structure.** A linear model is the trivial
  extraction case. Non-linear scorers
  ([`openfhe.R::eval_logistic`](https://openfheorg.github.io/openfhe.R/reference/eval_logistic.html),
  polynomial compositions) resist closed-form basis-vector attacks,
  though approximation attacks from the model-extraction literature
  still apply.

The vignette above shows the transport mechanics end-to-end. It is a
*toy illustration* in that the linear scorer plus hospital-held secret
key is exactly the configuration where the five-query attack works; a
production model-as-a-service deployment is the transport layer plus at
least one of the defences above.

## Network protocol: serialization

In practice the hospital and lab are on different machines. All objects
serialize for network transport:

`tdir`` ``<-`` `[`tempdir`](https://rdrr.io/r/base/tempfile.html)`(``)`` `[`fhe_serialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_serialize.html)`(``cc``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"context.bin"``)``)`` `[`fhe_serialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_serialize.html)`(``keys``@``public``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"pubkey.bin"``)``)`` `[`fhe_serialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_serialize.html)`(``ct1``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"patient_bm1.bin"``)``)`` `` ``## Lab receives the serialized files`` ``cc_lab`` ``<-`` `[`fhe_deserialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_deserialize.html)`(`[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"context.bin"``)``, ``"CryptoContext"``)`` ``ct_lab`` ``<-`` `[`fhe_deserialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_deserialize.html)`(`[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"patient_bm1.bin"``)``, ``"Ciphertext"``)`` `` ``## Lab applies its weights to the deserialized ciphertext`` ``ct_weighted`` ``<-`` ``ct_lab`` ``*`` ``0.35`` `` ``## Lab returns the result`` `[`fhe_serialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_serialize.html)`(``ct_weighted``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"weighted.bin"``)``)`` `` ``## Hospital receives, deserializes, decrypts`` ``ct_recv`` ``<-`` `[`fhe_deserialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_deserialize.html)`(`[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"weighted.bin"``)``, ``"Ciphertext"``)`` ``result`` ``<-`` `[`decrypt`](https://bnaras.github.io/homomorpheR/reference/decrypt.md)`(``ct_recv``, ``keys``@``secret``, cc ``=`` ``cc``)`` `[`set_length`](https://openfheorg.github.io/openfhe.R/reference/set_length.html)`(``result``, ``8L``)`` `[`get_real_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_real_packed_value.html)`(``result``)``[``1``:``8``]`

    ## [1] 0.420 0.280 0.525 0.105 0.735 0.315 0.385 0.630
