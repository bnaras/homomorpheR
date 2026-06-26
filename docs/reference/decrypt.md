# Decrypt a Paillier ciphertext

Dispatches on both `private_key` and `ciphertext` so that integer
[PaillierCiphertext](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md)s
and
[PaillierEncryptedReal](https://bnaras.github.io/homomorpheR/reference/PaillierEncryptedReal.md)s
are handled by separate methods.

## Usage

``` r
decrypt(private_key, ciphertext, ...)
```

## Arguments

- private_key:

  a
  [PaillierPrivateKey](https://bnaras.github.io/homomorpheR/reference/PaillierPrivateKey.md).

- ciphertext:

  a
  [PaillierCiphertext](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md)
  or
  [PaillierEncryptedReal](https://bnaras.github.io/homomorpheR/reference/PaillierEncryptedReal.md).

- ...:

  unused.

## Value

the decrypted value.

## Return semantics

The two cases differ deliberately:

- [PaillierCiphertext](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md)
  (integer) -\> a
  [gmp::bigz](https://rdrr.io/pkg/gmp/man/biginteger.html) in `[0, n)`.
  This preserves raw mod-`n` arithmetic; callers wanting signed integers
  should re-center themselves (`if (m > n/2) m - n`).

- [PaillierEncryptedReal](https://bnaras.github.io/homomorpheR/reference/PaillierEncryptedReal.md)
  -\> a `numeric` in `(-n/2, n/2)`. The method re-centers the raw
  mod-`n` residues so that negative real numbers and running totals that
  cross zero round-trip correctly. See
  [PaillierEncryptedReal](https://bnaras.github.io/homomorpheR/reference/PaillierEncryptedReal.md)
  for the full convention.
