# Paillier private key

Holds the secret `lambda` and a cached value `x` used during decryption,
together with a reference to the matching public key.

## Usage

``` r
PaillierPrivateKey(lambda, pubkey)
```

## Arguments

- lambda:

  the secret lambda.

- pubkey:

  the matching
  [PaillierPublicKey](https://bnaras.github.io/homomorpheR/reference/PaillierPublicKey.md).

## Value

an S7 object of class `PaillierPrivateKey` with properties `pubkey`,
`lambda` and `x`: the matching
[PaillierPublicKey](https://bnaras.github.io/homomorpheR/reference/PaillierPublicKey.md),
the secret lambda, and a value cached from it so that decryption does
not recompute a modular inverse each time. Obtain one with
[`get_private_key()`](https://bnaras.github.io/homomorpheR/reference/get_private_key.md)
on the pair returned by
[`paillier_keypair()`](https://bnaras.github.io/homomorpheR/reference/paillier_keypair.md).
