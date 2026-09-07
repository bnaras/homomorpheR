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

## Value

an S7 object of class `NCParty` with properties `name`, `number` and
`state`. `number` (1 or 2) records which of the two additive shares this
party receives; `state` holds the sites it manages and the public key.
Construct with
[`make_ncparty()`](https://bnaras.github.io/homomorpheR/reference/make_ncparty.md).

## Details

Part of the frozen Paillier-era legacy surface: the NCP masking
construction compensated for Paillier's single decryption key, a role
that threshold key generation
([`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md))
now fills without extra parties.
