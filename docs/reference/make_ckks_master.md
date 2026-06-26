# Construct a CKKS-backed master

Construct a CKKS-backed master

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
