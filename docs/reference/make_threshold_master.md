# Construct a threshold-CKKS master and the per-site secret shares

Runs the chained `multiparty_key_gen()` setup across `n_sites` sites.
The first site generates a fresh keypair `(pk_1, sk_1)`; each subsequent
site `i` calls `multiparty_key_gen(cc, pk_{i-1})` to produce
`(pk_{1..i}, sk_i)`. The final `pk_{1..n}` is the joint public key under
which everything is encrypted. Each site keeps its own `sk_i`; no single
party holds the joint secret.

## Usage

``` r
make_threshold_master(name, crypto_context, n_sites)
```

## Arguments

- name:

  short identifier.

- crypto_context:

  an `openfhe.R` `CryptoContext` configured for CKKS *with* the
  `MULTIPARTY` feature enabled. Pass `features = c(Feature$MULTIPARTY)`
  to `fhe_context()`.

- n_sites:

  number of participating sites (\>= 2).

## Value

a
[ThresholdMaster](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md).

## Details

Decryption is n-of-n: each site contributes a partial decryption
(`multiparty_decrypt_lead` for the first, then `multiparty_decrypt_main`
for the rest), and the master fuses them via
`multiparty_decrypt_fusion`. This happens automatically inside
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
when called on a `ThresholdMaster`.
