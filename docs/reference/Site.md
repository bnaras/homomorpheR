# A site in a multi-party protocol

Abstract base for the two kinds of participating party: a
[LocalSite](https://bnaras.github.io/homomorpheR/reference/LocalSite.md),
whose data is in this R session, and a
[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md),
whose contribution is produced elsewhere. Both answer the same generic,
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md),
and are indistinguishable to whoever asks: each returns an *encrypted*
contribution at the requested parameter.

## Usage

``` r
Site(name = character(0), state = NULL)
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

## Value

nothing — this class is abstract, so calling it raises an error instead
of returning an object. It is the common parent of
[LocalSite](https://bnaras.github.io/homomorpheR/reference/LocalSite.md)
and
[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md),
and the dispatch target for
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md).
Construct a co-located site with
[`make_worker()`](https://bnaras.github.io/homomorpheR/reference/make_worker.md).

## Details

A site is autonomous once constructed. It is given public parameters
once, when it is wired, and from then on it computes and encrypts
entirely on its own — it holds no reference to the party that aggregates
its answers, and needs none.

## See also

[LocalSite](https://bnaras.github.io/homomorpheR/reference/LocalSite.md),
[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md),
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
