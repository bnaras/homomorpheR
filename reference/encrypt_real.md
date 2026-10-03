# Encrypt a real number under a Paillier public key

Splits `x` into integer and fractional parts, encrypts each part as a
separate
[PaillierCiphertext](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md),
and packages the result as a
[PaillierEncryptedReal](https://bnaras.github.io/homomorpheR/reference/PaillierEncryptedReal.md)
so that later additions and subtractions can be performed via R's
arithmetic operators.

## Usage

``` r
encrypt_real(public_key, x, den)
```

## Arguments

- public_key:

  a
  [PaillierPublicKey](https://bnaras.github.io/homomorpheR/reference/PaillierPublicKey.md).

- x:

  a real number.

- den:

  the denominator used to scale the fractional part. The same
  denominator must be used at encryption and decryption.

## Value

a
[PaillierEncryptedReal](https://bnaras.github.io/homomorpheR/reference/PaillierEncryptedReal.md).
