# Signal that a site could not be reached

The condition a
[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md)
implementation raises when a transport, authentication, or timeout
failure stops it from answering. This is **not** the same event as
returning `NA`, which means the requested `theta` is non-evaluable at a
site that answered perfectly well; see the contract in
[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md).
Raising it aborts the round rather than silently changing the set of
sites being summed over.

## Usage

``` r
site_unavailable(message, site = NULL, parent = NULL)
```

## Arguments

- message:

  what went wrong, for the caller.

- site:

  optionally, the
  [Site](https://bnaras.github.io/homomorpheR/reference/Site.md) that
  was unreachable; its name is added to the message by
  [`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md).

- parent:

  optionally, the underlying condition (an `httr2` error, say) to chain
  for debugging.

## Value

nothing — called for its side effect of signalling a condition of class
`homomorpheR_site_unavailable`.
