# A site whose data lives in this R session

The ordinary case: the records are here, and `contribution_fn` is
evaluated in-process.
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
computes the contribution and **encrypts it** with the public parameters
the site was given when it was wired, so what leaves is already a
ciphertext.

## Usage

``` r
LocalSite(
  name = character(0),
  state = NULL,
  data = NULL,
  contribution_fn = NULL
)
```

## Arguments

- name:

  short identifier shown in printed output.

- state:

  an environment for mutable bookkeeping — the public key the site
  encrypts under, the capability
  [`set_workers()`](https://bnaras.github.io/homomorpheR/reference/set_workers.md)
  installs, and, on the frozen legacy path, the next link in the
  round-robin chain. Default: a fresh empty env.

- data:

  whatever `contribution_fn` needs in order to answer — a dataset, a
  database connection, a cohort identifier.

- contribution_fn:

  a function with signature `function(data, theta)` returning this
  site's contribution at `theta` as a plain numeric value. It does
  **not** encrypt;
  [`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
  does that. May return `NA` to signal that `theta` is non-evaluable
  here.

## Value

an S7 object of class `LocalSite`. Construct with
[`make_worker()`](https://bnaras.github.io/homomorpheR/reference/make_worker.md).
