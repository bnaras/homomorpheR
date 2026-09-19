# Run one round of the master/worker protocol

Backend-agnostic: sites are reached through
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
and the total is recovered through the
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
generic, so the same body works over
[CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md)
and
[ThresholdMaster](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md).

## Usage

``` r
master_aggregate(master, theta)
```

## Arguments

- master:

  a [Master](https://bnaras.github.io/homomorpheR/reference/Master.md),
  wired to its workers — with
  [`set_workers()`](https://bnaras.github.io/homomorpheR/reference/set_workers.md)
  for a
  [CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md),
  or by
  [`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md),
  which returns one already wired.

- theta:

  the current parameter value (passed through to each worker).

## Value

the aggregated value, or `NA_real_` if some site found `theta`
non-evaluable.

## Details

The master broadcasts `theta` to each worker — and only `theta`; each
worker supplies its own `data`. Each worker returns
`contribution_fn(data, theta)` and the result is encrypted under the
master's public key. The master sums the encrypted contributions
homomorphically and decrypts the total.

This is the topology that mirrors how distcomp, DataSHIELD, and similar
federated-analysis frameworks actually deploy: a flat fan-out / fan-in.
With a single-decrypter master, the master could in principle decrypt
individual contributions; the cryptographic guarantee strengthens when
paired with threshold key generation (no single party holds the secret
key).

Each worker returns an *already encrypted* contribution (see
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)),
so no individual site's cleartext value reaches the master. Only the
aggregate is decrypted.

Two failure modes, deliberately distinct. If a worker returns `NA`,
`theta` is non-evaluable there and this function returns `NA_real_`,
which optimizers read as "back off and try elsewhere". If a worker
signals
[`site_unavailable()`](https://bnaras.github.io/homomorpheR/reference/site_unavailable.md),
it could not be reached at all; that condition propagates and aborts the
round, because continuing would sum over a different set of sites and
silently change the objective between optimizer iterations.

`NA` is the one value that travels in the clear, since CKKS cannot
represent it. A master that chooses `theta` adaptively therefore learns
which parameter values break which site — a residual side channel that
no amount of encryption here removes.
