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
