# Privacy-Preserving Count Aggregation under BFV

## The problem

Multiple research sites each hold patient data and want to know the
*total count* of patients matching a criterion (say “`age < 50` and
`sex == 'F'` and `biomarker < 0.2`”) without revealing the individual
per-site counts to anyone, including the coordinator running the
aggregation.

This is the same pattern the Paillier vignette `vignette("QueryNCP")`
demonstrates with non-cooperating parties. Here we use OpenFHE’s **BFV**
scheme via the `openfhe.R` package. BFV operates on *integer vectors*
with both addition and multiplication — well-suited to counting and
other exact integer-valued aggregates.

This is the BFV companion to the CKKS-based real-valued master/worker
vignettes
([`vignette("mle")`](https://bnaras.github.io/homomorpheR/articles/mle.md),
[`vignette("cox")`](https://bnaras.github.io/homomorpheR/articles/cox.md)).
For exact integer aggregation BFV is the right choice; for real-valued
sufficient statistics CKKS is the right choice.

## Setup

[`library`](https://rdrr.io/r/base/library.html)`(`[`openfhe.R`](https://openfheorg.github.io/openfhe.R/)`)`` `` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``42``)`` ``site_data`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1000``, ``500``, ``1500``)``, ``function``(``n``)`` ``{`` `` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`` `` age ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(``40``:``70``, ``n``, replace ``=`` ``TRUE``)``,`` `` sex ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``)``, ``n``, replace ``=`` ``TRUE``)``,`` `` biomarker ``=`` `[`runif`](https://rdrr.io/r/stats/Uniform.html)`(``n``, ``0``, ``1``)`` `` ``)`` ``}``)`

## The coordinator sets up encryption

`## BFV: exact integer arithmetic mod plaintext_modulus.`` ``cc`` ``<-`` `[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"BFV"``,`` `` plaintext_modulus ``=`` ``65537L``,`` `` multiplicative_depth ``=`` ``1L``)`` ``keys`` ``<-`` `[`key_gen`](https://openfheorg.github.io/openfhe.R/reference/key_gen.html)`(``cc``)`` `` ``pk`` ``<-`` ``keys``@``public`` ``sk`` ``<-`` ``keys``@``secret`

In a real deployment the coordinator distributes the public key (and the
serialized context) to each site. The secret key stays with the
coordinator. We simulate this in one R session by sharing `cc` and `pk`
locally.

## Each site computes locally and encrypts

`site_encrypt`` ``<-`` ``function``(``site_df``, ``cc``, ``pk``)`` ``{`` `` ``count`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``site_df``$``age`` ``<`` ``50`` ``&`` `` ``site_df``$``sex`` ``==`` ``"F"`` ``&`` `` ``site_df``$``biomarker`` ``<`` ``0.2``)`` `` ``pt`` ``<-`` `[`make_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_packed_plaintext.html)`(``cc``, `[`as.integer`](https://rdrr.io/r/base/integer.html)`(``count``)``)`` `` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``pk``, ``pt``, cc ``=`` ``cc``)`` ``}`` `` ``ct_site1`` ``<-`` ``site_encrypt``(``site_data``[[``1``]``]``, ``cc``, ``pk``)`` ``ct_site2`` ``<-`` ``site_encrypt``(``site_data``[[``2``]``]``, ``cc``, ``pk``)`` ``ct_site3`` ``<-`` ``site_encrypt``(``site_data``[[``3``]``]``, ``cc``, ``pk``)`

These ciphertexts are opaque to the coordinator — they learn nothing
about individual site counts.

## The coordinator aggregates

`## Homomorphic addition: the coordinator never decrypts intermediate values.`` ``ct_total`` ``<-`` ``ct_site1`` ``+`` ``ct_site2`` ``+`` ``ct_site3`` `` ``## Only the coordinator holds the secret key.`` ``result`` ``<-`` `[`decrypt`](https://bnaras.github.io/homomorpheR/reference/decrypt.md)`(``ct_total``, ``sk``, cc ``=`` ``cc``)`` ``total_count`` ``<-`` `[`get_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_packed_value.html)`(``result``)``[``1``]`` ``total_count`

    ## [1] 105

## Verification

`true_count`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`sapply`](https://rdrr.io/r/base/lapply.html)`(``site_data``, ``function``(``df``)`` ``{`` `` `[`sum`](https://rdrr.io/r/base/sum.html)`(``df``$``age`` ``<`` ``50`` ``&`` ``df``$``sex`` ``==`` ``"F"`` ``&`` ``df``$``biomarker`` ``<`` ``0.2``)`` ``}``)``)`` ``true_count`

    ## [1] 105

[`stopifnot`](https://rdrr.io/r/base/stopifnot.html)`(``total_count`` ``==`` ``true_count``)`

The aggregated count matches the cleartext computation exactly — BFV is
*exact* over its integer plaintext space.

## What just happened

1.  The coordinator created an encryption context and distributed the
    **public key** to all sites.
2.  Each site computed its local count in the clear, encrypted it, and
    sent the ciphertext to the coordinator.
3.  The coordinator added the encrypted counts using `+` — BFV’s
    homomorphic addition makes this equivalent to adding the plaintexts.
4.  Only the coordinator, holding the **secret key**, could decrypt the
    total.

No site revealed its individual count. The coordinator never saw any
patient-level data. The total is exact.

## Serializing for a real distributed protocol

In a real deployment each site needs the coordinator’s context and
public key. `openfhe.R` provides serialization:

`tdir`` ``<-`` `[`tempdir`](https://rdrr.io/r/base/tempfile.html)`(``)`` `[`fhe_serialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_serialize.html)`(``cc``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"context.bin"``)``)`` `[`fhe_serialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_serialize.html)`(``pk``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"pubkey.bin"``)``)`` `` ``cc_remote`` ``<-`` `[`fhe_deserialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_deserialize.html)`(`[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"context.bin"``)``, ``"CryptoContext"``)`` ``pk_remote`` ``<-`` `[`fhe_deserialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_deserialize.html)`(`[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"pubkey.bin"``)``, ``"PublicKey"``)`` `` ``ct`` ``<-`` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``pk_remote``,`` `` `[`make_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_packed_plaintext.html)`(``cc_remote``, ``42L``)``,`` `` cc ``=`` ``cc_remote``)`` `` `[`fhe_serialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_serialize.html)`(``ct``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"site_count.bin"``)``)`` ``ct_received`` ``<-`` `[`fhe_deserialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_deserialize.html)`(`[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"site_count.bin"``)``, ``"Ciphertext"``)`` ``result`` ``<-`` `[`decrypt`](https://bnaras.github.io/homomorpheR/reference/decrypt.md)`(``ct_received``, ``sk``, cc ``=`` ``cc``)`` `[`get_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_packed_value.html)`(``result``)``[``1``]`

    ## [1] 42

## Comparison with the Paillier vignettes

The legacy Paillier vignettes (`vignette("QueryNCP")` in particular)
achieve the same kind of count aggregation using Paillier encryption,
which supports only additive homomorphism. With `openfhe.R`’s BFV:

- **Multiplication** is available — encrypted products, variances, more
  complex integer-valued aggregates.
- **Performance** — OpenFHE’s optimized C++ backend is orders of
  magnitude faster than pure-R Paillier on
  [`gmp::bigz`](https://rdrr.io/pkg/gmp/man/biginteger.html).
- **SIMD packing** — BFV encrypts integer vectors; multiple per-site
  counts can ride in the slots of a single ciphertext for parallel
  aggregation.
- **Threshold FHE** —
  [`multiparty_key_gen()`](https://openfheorg.github.io/openfhe.R/reference/multiparty_key_gen.html)
  lets the secret key be split across parties so no single party can
  decrypt unilaterally. (See
  [`vignette("cox-threshold")`](https://bnaras.github.io/homomorpheR/articles/cox-threshold.md)
  for the master/worker analogue under CKKS.)
