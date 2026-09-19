# Encrypt a value under the public parameters a party holds

The one encryption entry point. It takes only public material, so a
party that was handed that material at setup encrypts entirely on its
own, with nothing to consult and no one to ask. A
[Site](https://bnaras.github.io/homomorpheR/reference/Site.md) keeps its
copy in `state$params` from the moment it is wired, which is what makes
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
a purely local computation.

## Usage

``` r
encrypt_under(params, value)
```

## Arguments

- params:

  the public parameters this party holds — for a
  [Site](https://bnaras.github.io/homomorpheR/reference/Site.md),
  `site@state$params`, installed when it was wired.

- value:

  a numeric vector.

## Value

an encrypted value of the backend's type.

## Details

For the `openfhe` schemes the plaintext encoding follows whatever the
context was built for (packed CKKS reals, or packed integers for BFV and
BGV), read back from the context itself.

## See also

[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md),
which is how a
[Site](https://bnaras.github.io/homomorpheR/reference/Site.md) uses this
on its own data.
