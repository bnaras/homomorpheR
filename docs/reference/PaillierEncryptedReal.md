# A Paillier-encrypted real number

A pair of Paillier ciphertexts representing the integer and fractional
parts of a real number, together with the denominator used to scale the
fractional part. Two `PaillierEncryptedReal` values encrypted under the
same key with the same denominator combine via the standard arithmetic
operators.

## Usage

``` r
PaillierEncryptedReal(
  int = PaillierCiphertext(),
  frac = PaillierCiphertext(),
  den = NULL
)
```

## Arguments

- int:

  the
  [PaillierCiphertext](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md)
  holding the integer part.

- frac:

  the
  [PaillierCiphertext](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md)
  holding the scaled fractional part.

- den:

  the denominator used to scale the fractional part (a
  [gmp::bigq](https://rdrr.io/pkg/gmp/man/bigrational.html)).

## Value

an S7 object of class `PaillierEncryptedReal` with properties `int`,
`frac` and `den`: the
[PaillierCiphertext](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md)
carrying the integer part, the
[PaillierCiphertext](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md)
carrying the fractional part scaled by `den`, and the denominator
itself. It adds and subtracts with `+` and `-`;
[`decrypt()`](https://bnaras.github.io/homomorpheR/reference/decrypt.md)
recombines the two parts and re-centers the result into `(-n/2, n/2)` so
that signed values round-trip. Produced by
[`encrypt_real()`](https://bnaras.github.io/homomorpheR/reference/encrypt_real.md).

## Signed-arithmetic convention

Paillier's plaintext space is `Z_n` (a residue class modulo `n`, the
modulus carried by the public key). Negative real numbers and running
totals that cross zero are stored in their mod-`n` representation, which
lives in the upper half of `[0, n)`. The
[`decrypt()`](https://bnaras.github.io/homomorpheR/reference/decrypt.md)
method for `PaillierEncryptedReal` re-centers the raw decrypted residues
into the interval `(-n/2, n/2)` so that signed values round-trip
correctly. This means a `PaillierEncryptedReal` is *correct for signed
real arithmetic* as long as the true cleartext stays in `(-n/2, n/2)` —
for default 1024-bit keys, that is `> 10^307`, well beyond any plausible
statistical workload.

This convention applies only to `PaillierEncryptedReal`. The
integer-only
[PaillierCiphertext](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md)
decrypt method preserves raw mod-`n` semantics and does not center.
