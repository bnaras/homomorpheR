# Threshold-CKKS master (n-of-n key generation)

A [Master](https://bnaras.github.io/homomorpheR/reference/Master.md)
that drives the protocol over `openfhe.R` with threshold key generation,
under whichever scheme the supplied crypto context was built for (CKKS
for real-valued work, BFV or BGV for exact integer work). There is no
single secret key: each site generates and keeps its own share `sk_i`,
and the joint public key `pk_{1..n}` is built by chaining
[`keygen_round()`](https://bnaras.github.io/homomorpheR/reference/keygen_round.md)
across the sites. Encryption goes under `joint_pubkey`. Decryption
requires all `n` sites to return partial decryptions, which the master
then fuses.

## Usage

``` r
ThresholdMaster(
  name = character(0),
  state = NULL,
  crypto_context = NULL,
  joint_pubkey = NULL
)
```

## Arguments

- name:

  short identifier.

- state:

  an environment for mutable bookkeeping (the wired sites, in the order
  the key-generation chain visited them).

- crypto_context:

  an `openfhe.R` `CryptoContext` with the `MULTIPARTY` feature enabled.

- joint_pubkey:

  the joint public key produced by chaining
  [`keygen_round()`](https://bnaras.github.io/homomorpheR/reference/keygen_round.md)
  across the sites.

## Value

an S7 object of class `ThresholdMaster`, inheriting from
[Master](https://bnaras.github.io/homomorpheR/reference/Master.md), with
properties `name`, `crypto_context`, `joint_pubkey` and `state`. It
carries no secret key and no secret shares: decryption is driven by
asking each site for a partial decryption and fusing the results, so no
party — the master included — can decrypt alone. Construct with
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md).

## Details

**The master holds no secret material.** Its properties are the crypto
context and the joint public key, both public; the shares live at the
sites that generated them and never travel. That is what makes the
n-of-n claim true of the objects and not merely of the prose — see
[`partial_decrypt()`](https://bnaras.github.io/homomorpheR/reference/partial_decrypt.md)
for the decryption seam.

Constructed by
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md).
