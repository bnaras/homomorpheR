# Paillier public key

Holds the modulus and precomputed values used during encryption.
Construct via
[`paillier_keypair()`](https://bnaras.github.io/homomorpheR/reference/paillier_keypair.md)
rather than directly.

## Usage

``` r
PaillierPublicKey(bits, n)
```

## Arguments

- bits:

  modulus length in bits.

- n:

  the modulus.
