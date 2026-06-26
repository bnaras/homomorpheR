# Wire a master and a list of sites into a round-robin chain

Sets `master -> sites[[1]] -> sites[[2]] -> ... -> sites[[n]] -> master`
and broadcasts the master's public key to every site. After this call,
[`run_round_robin()`](https://bnaras.github.io/homomorpheR/reference/run_round_robin.md)
can drive an iteration of the protocol.

## Usage

``` r
round_robin_chain(master, sites)
```

## Arguments

- master:

  a [Master](https://bnaras.github.io/homomorpheR/reference/Master.md).

- sites:

  a list of
  [Site](https://bnaras.github.io/homomorpheR/reference/Site.md)s.

## Value

the master, invisibly.
