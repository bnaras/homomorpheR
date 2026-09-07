# Abstract master class

Common base for
[CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md)
and
[ThresholdMaster](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md)
(and the frozen legacy
[PaillierMaster](https://bnaras.github.io/homomorpheR/reference/PaillierMaster.md)).
Concrete masters carry whatever keys and context their cryptographic
backend needs; the protocol body in
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
uses
[`master_encrypt()`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md)
and
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
generics that dispatch on the concrete master class, so the same
protocol runs over any backend.

## Usage

``` r
Master(name = character(0), state = NULL)
```

## Arguments

- name:

  short identifier shown in printed output.

- state:

  an environment for mutable bookkeeping.

## Value

nothing — this class is abstract, so calling it raises an error instead
of returning an object. It exists so that
[`master_encrypt()`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md),
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
and
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
dispatch on a common parent. Construct a concrete master with
[`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)
or
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md).
