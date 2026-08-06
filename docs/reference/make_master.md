# Construct a Paillier-backed master

Part of the frozen Paillier-era legacy surface.

## Usage

``` r
make_master(name, keypair, den = gmp::as.bigq(2)^256)
```

## Arguments

- name:

  short identifier shown in printed output.

- keypair:

  a
  [PaillierKeyPair](https://bnaras.github.io/homomorpheR/reference/PaillierKeyPair.md).

- den:

  a [gmp::bigq](https://rdrr.io/pkg/gmp/man/bigrational.html)
  denominator used to scale fractional parts when encrypting real
  numbers via
  [`encrypt_real()`](https://bnaras.github.io/homomorpheR/reference/encrypt_real.md).

## Value

a
[PaillierMaster](https://bnaras.github.io/homomorpheR/reference/PaillierMaster.md).
