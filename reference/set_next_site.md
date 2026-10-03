# Wire one site's `next_site` to another

Part of the frozen Paillier-era legacy surface (round-robin chain
wiring); the supported topology is
[`set_workers()`](https://bnaras.github.io/homomorpheR/reference/set_workers.md) +
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md).

## Usage

``` r
set_next_site(obj, ...)
```

## Arguments

- obj:

  a [Site](https://bnaras.github.io/homomorpheR/reference/Site.md) or
  [Master](https://bnaras.github.io/homomorpheR/reference/Master.md).

- ...:

  method-specific arguments. The Site/Master methods take a single
  `next_site`.

## Value

the object `obj`, invisibly. Called for its side effect: `next_site` is
recorded in `obj`'s `state` environment, forming one link of the
round-robin chain.
