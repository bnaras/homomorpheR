# A site whose data lives in this R session

The ordinary case: the records are here, and `contribution_fn` is
evaluated in-process.
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
computes the contribution and **encrypts it** with the public parameters
the site was given when it was configured, so what leaves is already a
ciphertext.

## Usage

``` r
LocalSite(
  name = character(0),
  state = new.env(parent = emptyenv()),
  data = NULL,
  contribution_fn = function() NULL
)
```

## Arguments

- name:

  short identifier shown in printed output. A single non-empty string;
  it names the site in every error message, so an empty or vectorized
  name is rejected at construction.

- state:

  an environment for mutable bookkeeping — the public parameters the
  site was given when it was configured, its own key share under
  threshold keys, and, on the frozen legacy path, the next link in the
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

## Details

A `LocalSite` demonstrates the protocol's roles inside one R session. It
is not a deployment boundary: its data, and under threshold keys its key
share, are objects in this process, and anything else in this process
can reach them. Separating the parties for real means separately
controlled processes, which is what
[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md)
is for.
