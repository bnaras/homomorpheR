# Construct a CKKS-backed master

The context must be a CKKS one. A
[CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md)
built over BFV or BGV would work arithmetically but every sentence of
its documentation, and the class name a user reads in printed output,
would be wrong about which scheme is in use; exact-integer work goes
through
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md),
which is scheme-agnostic by design and says so.

## Usage

``` r
make_ckks_master(name, crypto_context, keypair)
```

## Arguments

- name:

  short identifier shown in printed output.

- crypto_context:

  an `openfhe.R` `CryptoContext` configured for CKKS.

- keypair:

  an `openfhe.R` `KeyPair`.

## Value

a
[CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md).
