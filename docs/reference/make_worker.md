# Construct a worker

Builds the
[Site](https://bnaras.github.io/homomorpheR/reference/Site.md) one party
contributes to a multi-party protocol. A `Site` becomes a *worker* once
it has been wired and given its public parameters; from that point it is
autonomous, computing and encrypting on its own.

## Usage

``` r
make_worker(name, data, contribution_fn)
```

## Arguments

- name:

  short identifier shown in printed output. A single non-empty string;
  it names the site in every error message, so an empty or vectorized
  name is rejected at construction.

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

a
[LocalSite](https://bnaras.github.io/homomorpheR/reference/LocalSite.md).

## See also

[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md)
for a site whose contribution is produced outside this R session.
