# The public parameters a party holds

Reads back what
[`set_public_params()`](https://bnaras.github.io/homomorpheR/reference/set_public_params.md)
delivered. Encryption needs only this, so a party that has it is
self-sufficient, and any other party that will encrypt under the same
key — a querier that is not itself a site, say — can be handed a copy.

## Usage

``` r
site_params(site, ...)
```

## Arguments

- site:

  a [Site](https://bnaras.github.io/homomorpheR/reference/Site.md), or a
  user-defined subclass of
  [RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md).

- ...:

  method-specific arguments; the built-in method takes none.

## Value

a
[PublicParams](https://bnaras.github.io/homomorpheR/reference/PublicParams.md)
object.

## Details

Aborts if the site was never configured, rather than returning `NULL`
for a caller to encrypt with.

## See also

[`set_public_params()`](https://bnaras.github.io/homomorpheR/reference/set_public_params.md),
[`encrypt_under()`](https://bnaras.github.io/homomorpheR/reference/encrypt_under.md)
