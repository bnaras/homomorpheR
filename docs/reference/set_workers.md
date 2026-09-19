# Wire a master to a flat list of workers

Stashes the workers in the master's state and publishes its public
parameters to each one. That bundle is public: it is the setup broadcast
a coordinator would send over the wire, and it is all a site needs in
order to encrypt. After this call,
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
can drive an iteration of the protocol.

## Usage

``` r
set_workers(master, workers)
```

## Arguments

- master:

  a
  [CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md),
  or the frozen legacy
  [PaillierMaster](https://bnaras.github.io/homomorpheR/reference/PaillierMaster.md).

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

A
[ThresholdMaster](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md)
does **not** use this function: its joint public key does not exist
until key generation has run through every site, so
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
takes the sites and returns a master already wired to them, in the order
the chain fixed.
