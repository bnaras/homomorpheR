# Abstract master class

Common base for
[CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md)
and
[ThresholdMaster](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md)
(and the frozen legacy
[PaillierMaster](https://bnaras.github.io/homomorpheR/reference/PaillierMaster.md)).
Concrete masters carry whatever context and public keys their
cryptographic backend needs; the protocol body in
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
reaches sites through
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
and recovers the total through the
[`decrypt()`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)
generic, which dispatches on the concrete master class, so the same
protocol runs over any backend.

## Usage

``` r
Master(name = character(0), state = new.env(parent = emptyenv()))
```

## Arguments

- name:

  short identifier shown in printed output.

- state:

  an environment for mutable bookkeeping.

## Value

nothing — this class is abstract, so calling it raises an error instead
of returning an object. It exists so that
[`decrypt()`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)
and
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
dispatch on a common parent. Construct a concrete master with
[`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)
or
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md).

## Details

A master never encrypts site data, and has no encryption entry point at
all. Each party encrypts its own values with
[`encrypt()`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html),
which for a
[Site](https://bnaras.github.io/homomorpheR/reference/Site.md) takes
nothing but the site itself: it encrypts with the public parameters it
was handed when it was wired. The asymmetry is deliberate and worth
reading off the API: decryption is privileged — it needs secret
material, or the standing to convene every site — while encryption needs
only public material and is available to anyone.
