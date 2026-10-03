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

## Value

an S7 object of class `PaillierPublicKey` with properties `bits`, `n`,
`n_squared` and `n_plus_one`: the modulus length, the modulus itself,
and the two values precomputed from it that encryption needs. Obtain one
as the `pubkey` component of the pair returned by
[`paillier_keypair()`](https://bnaras.github.io/homomorpheR/reference/paillier_keypair.md)
rather than constructing it directly.
