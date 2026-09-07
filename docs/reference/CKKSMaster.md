# CKKS-backed master

A [Master](https://bnaras.github.io/homomorpheR/reference/Master.md)
that drives the protocol over `openfhe.R`'s CKKS encryption. CKKS
handles real-valued arithmetic natively, so no `den` denominator is
needed. Constructed by
[`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md).

## Usage

``` r
CKKSMaster(
  name = character(0),
  state = NULL,
  crypto_context = NULL,
  keypair = NULL
)
```

## Arguments

- name:

  short identifier shown in printed output.

- state:

  an environment for mutable bookkeeping.

- crypto_context:

  an `openfhe.R` `CryptoContext` configured for CKKS.

- keypair:

  an `openfhe.R` `KeyPair`.

## Value

an S7 object of class `CKKSMaster`, inheriting from
[Master](https://bnaras.github.io/homomorpheR/reference/Master.md), with
properties `name`, `crypto_context`, `keypair` and `state`. It holds a
single CKKS key pair, so it is the appropriate master when one party is
allowed to hold the secret key; when no party may, use
[ThresholdMaster](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md).
Construct with
[`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md).
