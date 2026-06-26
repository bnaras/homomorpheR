# A site in a multi-party protocol

Holds the site's local data and the function that computes the per-site
summary at a given parameter value. Use
[`make_site()`](https://bnaras.github.io/homomorpheR/reference/make_site.md)
to construct.

## Usage

``` r
Site(name = character(0), data = NULL, local_fn = NULL, state = NULL)
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

- state:

  an environment for mutable bookkeeping (next site, public key, master
  back-reference). Default: a fresh empty env.
