# A site's encrypted contribution at a parameter value

The single call the protocol runner makes on a site. Implementations
return the site's contribution **already encrypted**, using the public
parameters the site was given when it was wired, so an individual site's
cleartext contribution never reaches the aggregator — that is the
property the whole protocol rests on.

## Usage

``` r
contribute(site, ...)
```

## Arguments

- site:

  a
  [LocalSite](https://bnaras.github.io/homomorpheR/reference/LocalSite.md),
  or a user-defined subclass of
  [RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md).

- ...:

  method-specific arguments; both built-in methods take `theta`, the
  parameter value being queried.

## Value

an encrypted contribution, of whatever type the site's own public
parameters imply, or `NA` if `theta` is non-evaluable here.

## Details

The computation is entirely local. A site needs nothing at call time
beyond `theta`, its own data, and what it already holds.

The only permitted plaintext reply is `NA`, signalling that `theta` is
non-evaluable at this site; CKKS has no representation for it, so it
cannot be encrypted. A site that cannot be *reached* must signal
[`site_unavailable()`](https://bnaras.github.io/homomorpheR/reference/site_unavailable.md)
instead of returning `NA`.

## See also

[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md)
for the contract a remote implementation must honor.
