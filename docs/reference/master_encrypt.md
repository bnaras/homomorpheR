# Encrypt a real value for a master's protocol

Dispatches on the master's class so the same protocol body works over
different cryptographic backends.

## Usage

``` r
master_encrypt(master, ...)
```

## Arguments

- master:

  a [Master](https://bnaras.github.io/homomorpheR/reference/Master.md).

- ...:

  method-specific arguments. Both backends take a single real-valued
  `value`.

## Value

the encrypted value (a
[PaillierEncryptedReal](https://bnaras.github.io/homomorpheR/reference/PaillierEncryptedReal.md)
for
[PaillierMaster](https://bnaras.github.io/homomorpheR/reference/PaillierMaster.md);
an `openfhe.R` `Ciphertext` for
[CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md)).
