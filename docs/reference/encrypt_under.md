# Encrypt a value under the public parameters a party holds

The one encryption entry point. It takes only public material, so a
party handed that material at setup encrypts entirely on its own, with
nothing to consult and no one to ask — which is what makes
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
a purely local computation. It names no party, because encryption
privileges none.

## Usage

``` r
encrypt_under(params, value)
```

## Arguments

- params:

  the
  [PublicParams](https://bnaras.github.io/homomorpheR/reference/PublicParams.md)
  this party holds — for a
  [Site](https://bnaras.github.io/homomorpheR/reference/Site.md),
  `site_params(site)`.

- value:

  a numeric vector.

## Value

an encrypted value of the backend's type.

## Details

For the `openfhe` backends the encoding follows whatever the context was
built for, read back from the context itself: packed reals under CKKS,
packed integers under BFV and BGV. The exact schemes reject a value they
cannot represent rather than round it; see
[OpenFHEParams](https://bnaras.github.io/homomorpheR/reference/OpenFHEParams.md).

## See also

[`site_params()`](https://bnaras.github.io/homomorpheR/reference/site_params.md),
and
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md),
which is how a
[Site](https://bnaras.github.io/homomorpheR/reference/Site.md) uses this
on its own data.
