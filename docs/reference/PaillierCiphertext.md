# A Paillier ciphertext

Wraps the encrypted big-integer value together with the public key it
was encrypted under. Two ciphertexts encrypted under the same public key
can be combined with `+` and `-`; a ciphertext can be multiplied by a
cleartext integer with `*`.

## Usage

``` r
PaillierCiphertext(value = NULL, pubkey = PaillierPublicKey())
```

## Arguments

- value:

  the encrypted big-integer value.

- pubkey:

  the
  [PaillierPublicKey](https://bnaras.github.io/homomorpheR/reference/PaillierPublicKey.md)
  under which it was encrypted.
