# Abstract master class

Common base for
[PaillierMaster](https://bnaras.github.io/homomorpheR/reference/PaillierMaster.md)
and
[CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md).
Concrete masters carry whatever keys and context their cryptographic
backend needs; the protocol body in
[`run_round_robin()`](https://bnaras.github.io/homomorpheR/reference/run_round_robin.md)
uses
[`master_encrypt()`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md)
and
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
generics that dispatch on the concrete master class, so the same
protocol runs over either backend.

## Usage

``` r
Master(name = character(0), state = NULL)
```

## Arguments

- name:

  short identifier shown in printed output.

- state:

  an environment for mutable bookkeeping.
