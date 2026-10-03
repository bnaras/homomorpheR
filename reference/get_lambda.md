# Return the secret lambda from a private key

Return the secret lambda from a private key

## Usage

``` r
get_lambda(private_key, ...)
```

## Arguments

- private_key:

  a
  [PaillierPrivateKey](https://bnaras.github.io/homomorpheR/reference/PaillierPrivateKey.md).

- ...:

  unused.

## Value

a [gmp::bigz](https://rdrr.io/pkg/gmp/man/biginteger.html) value.
