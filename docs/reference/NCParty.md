# A non-cooperating party

Sits between the master and the sites in the non-cooperating-parties
topology. Two NCPs receive *additive shares* of each site's
contribution; each NCP sums its share across sites and ships the result
to the master, who combines the two NCP totals and decrypts. No single
party — neither master nor an NCP — sees an individual site's
contribution. Use
[`make_ncparty()`](https://bnaras.github.io/homomorpheR/reference/make_ncparty.md)
to construct.

## Usage

``` r
NCParty(name = character(0), number = integer(0), state = NULL)
```

## Arguments

- name:

  short identifier shown in printed output.

- number:

  which share this NCP receives, `1` or `2`.

- state:

  an environment for mutable bookkeeping (the list of sites it manages,
  public key). Default: a fresh empty env.
