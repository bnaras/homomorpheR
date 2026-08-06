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
