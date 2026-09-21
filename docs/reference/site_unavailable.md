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

nothing — called for its side effect of signaling a condition of class
`homomorpheR_site_unavailable`.

## What the re-raised condition carries

When
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
or
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
re-raise this, the condition they signal carries a `site_name` field and
**not** the site object. A
[LocalSite](https://bnaras.github.io/homomorpheR/reference/LocalSite.md)
would drag its data, and under threshold keys its key share, into
anything that logs or serializes the condition. Catch on the class and
read `cnd$site_name`.
