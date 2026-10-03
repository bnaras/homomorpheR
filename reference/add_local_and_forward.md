# Internal generic: forward the running encrypted total along the chain

Part of the frozen Paillier-era legacy surface.

## Usage

``` r
add_local_and_forward(obj, ...)
```

## Arguments

- obj:

  a [Site](https://bnaras.github.io/homomorpheR/reference/Site.md) or
  [Master](https://bnaras.github.io/homomorpheR/reference/Master.md).

- ...:

  method-specific arguments: `theta` (the current parameter value),
  `running` (the running encrypted total), and `master` (so workers can
  signal failure back to the master).

## Value

`NULL`, invisibly. Called for its side effect: the
[Site](https://bnaras.github.io/homomorpheR/reference/Site.md) method
adds this site's encrypted local contribution to the running total and
forwards it to the next link in the chain, while the
[Master](https://bnaras.github.io/homomorpheR/reference/Master.md)
method terminates the chain by storing the total in the master's
`state`. If a site's local function returns `NA`, the master is flagged
as failed and the chain stops early.
