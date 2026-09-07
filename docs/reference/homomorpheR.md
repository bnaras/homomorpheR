# homomorpheR: Homomorphic computations in R

`homomorpheR` provides homomorphic encryption schemes for
privacy-preserving distributed computations: applications of the sort
implemented in package `distcomp`. The Paillier cryptosystem is
implemented natively in R via the `gmp` package; CKKS, BFV, BGV, and
FHEW/TFHE schemes are available through the `openfhe.R` package.

## Details

Encrypted values are wrapped in
[PaillierCiphertext](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md)
objects so that R's arithmetic operators dispatch to the homomorphism.
Use
[`paillier_keypair()`](https://bnaras.github.io/homomorpheR/reference/paillier_keypair.md)
to generate keys,
[`encrypt()`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)
to encrypt, and
[`decrypt()`](https://bnaras.github.io/homomorpheR/reference/decrypt.md)
to recover the result.

For a quick overview, see the package vignettes.

## References

[Homomorphic
Encryption](https://en.wikipedia.org/wiki/Homomorphic_encryption)

[Paillier Encryption](https://mhe.github.io/jspaillier/)

## See also

Useful links:

- <https://bnaras.github.io/homomorpheR/>

- Report bugs at <https://github.com/bnaras/homomorpheR/issues>

## Author

**Maintainer**: Balasubramanian Narasimhan <naras@stat.Stanford.EDU>
\[copyright holder\]

Authors:

- Balasubramanian Narasimhan <naras@stat.Stanford.EDU> \[copyright
  holder\]

## Examples

``` r
keys <- paillier_keypair(1024)
encrypt_decrypt <- function(x) decrypt(get_private_key(keys),
                                       encrypt(keys@pubkey, x))

## The additive homomorphism: adding in the clear and adding under
## encryption give the same answer.
a <- gmp::as.bigz(1273849)
identical(a + 10L, encrypt_decrypt(a + 10L))
#> [1] TRUE
```
