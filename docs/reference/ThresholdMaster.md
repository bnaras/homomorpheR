# Threshold-CKKS master (n-of-n key generation)

A [Master](https://bnaras.github.io/homomorpheR/reference/Master.md)
that drives the protocol over `openfhe.R` with threshold key generation,
under whichever scheme the supplied crypto context was built for (CKKS
for real-valued work, BFV or BGV for exact integer work). There is no
single secret key: each site holds a secret share `sk_i`, and the joint
public key `pk_{1..n}` is built by chaining `multiparty_key_gen()`
across sites. Encryption goes under `joint_pubkey`. Decryption requires
all `n` sites to contribute partial decryptions, which the master then
fuses.

## Usage

``` r
ThresholdMaster(
  name = character(0),
  state = NULL,
  crypto_context = NULL,
  joint_pubkey = NULL,
  secret_keys = NULL
)
```

## Arguments

- name:

  short identifier.

- state:

  an environment for mutable bookkeeping.

- crypto_context:

  an `openfhe.R` `CryptoContext` with the `MULTIPARTY` feature enabled.

- joint_pubkey:

  the joint public key produced by chaining `multiparty_key_gen()`
  across the sites.

- secret_keys:

  a list of per-site secret keys, in site order (the first is the lead
  site whose `sk` started the chain).

## Value

an S7 object of class `ThresholdMaster`, inheriting from
[Master](https://bnaras.github.io/homomorpheR/reference/Master.md), with
properties `name`, `crypto_context`, `joint_pubkey`, `secret_keys` and
`state`. There is no single secret key: `secret_keys` holds one share
per site and decryption fuses partial decryptions from all of them, so
no party can decrypt alone. Construct with
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md).

## Details

Constructed by
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md).
