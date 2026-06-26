# Wire a master to a flat list of workers

Stashes the workers in the master's state and broadcasts the master's
public key to each worker. After this call,
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
can drive an iteration of the protocol.

## Usage

``` r
set_workers(master, workers)
```

## Arguments

- master:

  a [Master](https://bnaras.github.io/homomorpheR/reference/Master.md).

- workers:

  a list of worker
  [Site](https://bnaras.github.io/homomorpheR/reference/Site.md)s.

## Value

the master, invisibly.

## Details

Use this for the realistic master/worker (star) topology that distcomp-
and DataSHIELD-style federated analyses follow. For the legacy Paillier
round-robin idiom, use
[`round_robin_chain()`](https://bnaras.github.io/homomorpheR/reference/round_robin_chain.md)
instead.
