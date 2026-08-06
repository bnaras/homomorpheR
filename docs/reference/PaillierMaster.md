# Paillier-backed master

A [Master](https://bnaras.github.io/homomorpheR/reference/Master.md)
that drives the protocol over Paillier additive encryption. Constructed
by
[`make_master()`](https://bnaras.github.io/homomorpheR/reference/make_master.md).
Part of the frozen Paillier-era legacy surface; new work should use
[`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)
or
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md).

## Usage

``` r
PaillierMaster(
  name = character(0),
  state = NULL,
  keypair = PaillierKeyPair(),
  den = NULL
)
```

## Arguments

- name:

  short identifier shown in printed output.

- state:

  an environment for mutable bookkeeping.

- keypair:

  a
  [PaillierKeyPair](https://bnaras.github.io/homomorpheR/reference/PaillierKeyPair.md).

- den:

  a [gmp::bigq](https://rdrr.io/pkg/gmp/man/bigrational.html)
  denominator used to scale fractional parts when encrypting real
  numbers via
  [`encrypt_real()`](https://bnaras.github.io/homomorpheR/reference/encrypt_real.md).
