# Construct a worker (alias for [`make_site()`](https://bnaras.github.io/homomorpheR/reference/make_site.md))

Provided for naming clarity in master/worker protocols. Returns a
[Site](https://bnaras.github.io/homomorpheR/reference/Site.md) with
identical semantics to
[`make_site()`](https://bnaras.github.io/homomorpheR/reference/make_site.md).

## Usage

``` r
make_worker(name, data, local_fn)
```

## Arguments

- name:

  short identifier shown in printed output.

- data:

  local dataset.

- local_fn:

  a function with signature `function(data, theta)` returning the
  site-level summary at `theta`. May return `NA` to signal a
  non-evaluable parameter (an extreme `theta` that breaks the local
  solver, for example); the master will propagate `NA` back to the
  optimizer.

## Value

a [Site](https://bnaras.github.io/homomorpheR/reference/Site.md).
