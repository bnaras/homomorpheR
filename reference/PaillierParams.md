# Public parameters for the frozen Paillier backend

Part of the frozen Paillier-era legacy surface. Paillier's public setup
is a public key plus the fixed-point denominator its real-valued
encoding needs; both are public, so a Paillier site encrypts its own
contribution in
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
exactly like an OpenFHE one. The supported backends use
[OpenFHEParams](https://bnaras.github.io/homomorpheR/reference/OpenFHEParams.md).

## Usage

``` r
PaillierParams(pk = PaillierPublicKey(), den = NULL)
```

## Arguments

- pk:

  a
  [PaillierPublicKey](https://bnaras.github.io/homomorpheR/reference/PaillierPublicKey.md).

- den:

  a [gmp::bigq](https://rdrr.io/pkg/gmp/man/bigrational.html)
  denominator used to scale fractional parts.

## Value

an S7 object of class `PaillierParams`, inheriting from
[PublicParams](https://bnaras.github.io/homomorpheR/reference/PublicParams.md),
with properties `pk` and `den`.
