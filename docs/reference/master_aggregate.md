# Run one round of the master/worker protocol

Backend-agnostic via the
[`master_encrypt()`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md)
/
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
generics: works over
[CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md),
[ThresholdMaster](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md),
and the legacy
[PaillierMaster](https://bnaras.github.io/homomorpheR/reference/PaillierMaster.md).

## Usage

``` r
master_aggregate(master, theta)
```

## Arguments

- master:

  a [Master](https://bnaras.github.io/homomorpheR/reference/Master.md),
  wired to workers via
  [`set_workers()`](https://bnaras.github.io/homomorpheR/reference/set_workers.md).

- theta:

  the current parameter value (passed through to each worker's
  `local_fn`).

## Value

the aggregated value, or `NA_real_` on failure.

## Details

The master broadcasts `theta` to each worker. Each worker computes its
local summary `local_fn(data, theta)` and the result is encrypted under
the master's public key. The master sums the encrypted contributions
homomorphically and decrypts the total.

This is the topology that mirrors how distcomp, DataSHIELD, and similar
federated-analysis frameworks actually deploy: a flat fan-out / fan-in.
With a single-decrypter master, the master could in principle decrypt
individual contributions; the cryptographic guarantee strengthens when
paired with threshold key generation (no single party holds the secret
key).

If any worker's `local_fn` returns `NA`, this function returns
`NA_real_`.
