# Encrypt a value under a Paillier public key

Encrypt a value under a Paillier public key

## Usage

``` r
encrypt(public_key, ...)
```

## Arguments

- public_key:

  a
  [PaillierPublicKey](https://bnaras.github.io/homomorpheR/reference/PaillierPublicKey.md).

- ...:

  method-specific arguments. The Paillier method takes a single
  cleartext value `x` (integer or
  [gmp::bigz](https://rdrr.io/pkg/gmp/man/biginteger.html)).

## Value

a
[PaillierCiphertext](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md).
