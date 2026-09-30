# Privacy-Preserving Count Aggregation

## The problem

Multiple research sites each hold patient data and want to know the
*total count* of patients matching a criterion (say “`age < 50` and
`sex == 'F'` and `biomarker < 0.2`”) without revealing the individual
per-site counts to anyone, including the aggregator running the
aggregation.

Here we use OpenFHE’s **BFV** scheme via the `openfhe.R` package. BFV
operates on *integer vectors* with both addition and multiplication —
well-suited to counting and other exact integer-valued aggregates.

This is the BFV companion to the CKKS-based real-valued master/worker
vignettes
([`vignette("mle")`](https://bnaras.github.io/homomorpheR/articles/mle.md),
[`vignette("cox")`](https://bnaras.github.io/homomorpheR/articles/cox.md)).
For exact integer aggregation BFV is the right choice; for real-valued
sufficient statistics CKKS is the right choice.

## Setup

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`openfhe.R`](https://openfheorg.github.io/openfhe.R/)`)`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``42``)`\
`site_data`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1000``, ``500``, ``1500``)``, ``function``(``n``)`` ``{`\
`    `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`        age       ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(``40``:``70``, ``n``, replace ``=`` ``TRUE``)``,`\
`        sex       ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``)``, ``n``, replace ``=`` ``TRUE``)``,`\
`        biomarker ``=`` `[`runif`](https://rdrr.io/r/stats/Uniform.html)`(``n``, ``0``, ``1``)`\
`    ``)`\
`}``)`

## The aggregator sets up encryption

\
`## BFV: exact integer arithmetic mod plaintext_modulus.`\
`cc``   ``<-`` `[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"BFV"``,`\
`                    plaintext_modulus    ``=`` ``65537L``,`\
`                    multiplicative_depth ``=`` ``1L``)`\
`keys`` ``<-`` `[`key_gen`](https://openfheorg.github.io/openfhe.R/reference/key_gen.html)`(``cc``)`\
\
`pk`` ``<-`` ``keys``@``public`\
`sk`` ``<-`` ``keys``@``secret`

In a real deployment the aggregator distributes the public key (and the
serialized context) to each site. The secret key stays with the
aggregator. We simulate this in one R session by sharing `cc` and `pk`
locally.

## Each site computes locally and encrypts

\
`site_encrypt`` ``<-`` ``function``(``site_df``, ``cc``, ``pk``)`` ``{`\
`    ``count`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``site_df``$``age`` ``<`` ``50`` ``&`\
`                 ``site_df``$``sex`` ``==`` ``"F"`` ``&`\
`                 ``site_df``$``biomarker`` ``<`` ``0.2``)`\
`    ``pt``    ``<-`` `[`make_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_packed_plaintext.html)`(``cc``, `[`as.integer`](https://rdrr.io/r/base/integer.html)`(``count``)``)`\
`    `[`encrypt`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)`(``pk``, ``pt``, cc ``=`` ``cc``)`\
`}`\
\
`ct_site1`` ``<-`` ``site_encrypt``(``site_data``[[``1``]``]``, ``cc``, ``pk``)`\
`ct_site2`` ``<-`` ``site_encrypt``(``site_data``[[``2``]``]``, ``cc``, ``pk``)`\
`ct_site3`` ``<-`` ``site_encrypt``(``site_data``[[``3``]``]``, ``cc``, ``pk``)`

These encrypted counts are opaque to the aggregator — it learns nothing
about any individual site’s count.

## The aggregator aggregates

\
`## Homomorphic addition: the aggregator never decrypts intermediate values.`\
`ct_total`` ``<-`` ``ct_site1`` ``+`` ``ct_site2`` ``+`` ``ct_site3`\
\
`## Only the aggregator holds the secret key.`\
`result``      ``<-`` `[`decrypt`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)`(``ct_total``, ``sk``, cc ``=`` ``cc``)`\
`total_count`` ``<-`` `[`get_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_packed_value.html)`(``result``)``[``1``]`\
`total_count`

    ## [1] 105

## Verification

\
`true_count`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`sapply`](https://rdrr.io/r/base/lapply.html)`(``site_data``, ``function``(``df``)`` ``{`\
`    `[`sum`](https://rdrr.io/r/base/sum.html)`(``df``$``age`` ``<`` ``50`` ``&`` ``df``$``sex`` ``==`` ``"F"`` ``&`` ``df``$``biomarker`` ``<`` ``0.2``)`\
`}``)``)`\
`true_count`

    ## [1] 105

\
[`stopifnot`](https://rdrr.io/r/base/stopifnot.html)`(``total_count`` ``==`` ``true_count``)`

The aggregated count matches the cleartext computation exactly. BFV is
an *exact* scheme over the integers: unlike the real-valued arithmetic
used elsewhere in this package, it introduces no approximation error at
all.

## The details of what happened

1.  The aggregator created an encryption context and distributed the
    **public key** to all sites.
2.  Each site computed its local count in the clear, encrypted it, and
    sent the encrypted count to the aggregator.
3.  The aggregator added the encrypted counts using `+`, which gives the
    same answer as adding the counts themselves.
4.  Only the aggregator, holding the **secret key**, could decrypt the
    total.

No site revealed its individual count. The aggregator never saw any
patient-level data. The total is exact.

## Serializing for a real distributed protocol

In a real deployment each site needs the aggregator’s context and public
key. `openfhe.R` provides serialization:

\
`tdir`` ``<-`` `[`tempdir`](https://rdrr.io/r/base/tempfile.html)`(``)`\
[`fhe_serialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_serialize.html)`(``cc``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"context.bin"``)``)`\
[`fhe_serialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_serialize.html)`(``pk``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"pubkey.bin"``)``)`\
\
`cc_remote`` ``<-`` `[`fhe_deserialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_deserialize.html)`(`[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"context.bin"``)``, ``"CryptoContext"``)`\
`pk_remote`` ``<-`` `[`fhe_deserialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_deserialize.html)`(`[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"pubkey.bin"``)``, ``"PublicKey"``)`\
\
`ct`` ``<-`` `[`encrypt`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)`(``pk_remote``,`\
`              `[`make_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_packed_plaintext.html)`(``cc_remote``, ``42L``)``,`\
`              cc ``=`` ``cc_remote``)`\
\
[`fhe_serialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_serialize.html)`(``ct``, `[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"site_count.bin"``)``)`\
`ct_received`` ``<-`` `[`fhe_deserialize`](https://openfheorg.github.io/openfhe.R/reference/fhe_deserialize.html)`(`[`file.path`](https://rdrr.io/r/base/file.path.html)`(``tdir``, ``"site_count.bin"``)``, ``"Ciphertext"``)`\
`result``      ``<-`` `[`decrypt`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)`(``ct_received``, ``sk``, cc ``=`` ``cc``)`\
[`get_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_packed_value.html)`(``result``)``[``1``]`

    ## [1] 42
