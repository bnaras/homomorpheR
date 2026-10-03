# Random big integer

Returns a random big integer using the cryptographically secure
generator from the `sodium` package.

## Usage

``` r
random.bigz(nBits)
```

## Arguments

- nBits:

  number of bits, which must be a multiple of 8 (not checked, for
  efficiency).

## Value

a [gmp::bigz](https://rdrr.io/pkg/gmp/man/biginteger.html) value.
