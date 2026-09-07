# Paillier key pair

A matched pair of public and private keys. Use
[`paillier_keypair()`](https://bnaras.github.io/homomorpheR/reference/paillier_keypair.md)
to generate one.

## Usage

``` r
PaillierKeyPair(pubkey = PaillierPublicKey(), privkey = PaillierPrivateKey())
```

## Arguments

- pubkey:

  a
  [PaillierPublicKey](https://bnaras.github.io/homomorpheR/reference/PaillierPublicKey.md).

- privkey:

  a
  [PaillierPrivateKey](https://bnaras.github.io/homomorpheR/reference/PaillierPrivateKey.md).

## Value

an S7 object of class `PaillierKeyPair` with properties `pubkey` (a
[PaillierPublicKey](https://bnaras.github.io/homomorpheR/reference/PaillierPublicKey.md))
and `privkey` (a
[PaillierPrivateKey](https://bnaras.github.io/homomorpheR/reference/PaillierPrivateKey.md))
— the two halves of one generated key. Returned by
[`paillier_keypair()`](https://bnaras.github.io/homomorpheR/reference/paillier_keypair.md).
