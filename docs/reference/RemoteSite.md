# A site whose contribution is produced outside this R session

Abstract.
[homomorpheR](https://bnaras.github.io/homomorpheR/reference/homomorpheR.md)
deliberately ships **no** implementation: transports differ too much,
and a crypto package has no business carrying an HTTP client. Subclass
it, add whatever properties your transport needs, and register a
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
method:

## Usage

``` r
RemoteSite(name = character(0), state = NULL)
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

nothing — this class is abstract. Subclass it as shown above.

## Details

    HttpSite <- S7::new_class("HttpSite", parent = RemoteSite,
                              properties = list(url = S7::class_character))
    S7::method(contribute, HttpSite) <- function(site, theta) {
        ## ... call site@url with theta; the far end encrypts ...
    }

## The contract an implementation must honor

- Return a ciphertext, never a plain number.:

  The remote end was given the public parameters when it was wired, so
  it encrypts *before* the value crosses the wire. A `RemoteSite` that
  returns cleartext hands the aggregator an individual per-site
  contribution, which is precisely what the protocol exists to prevent.
  `NA` is the one permitted plaintext reply, because CKKS has no
  representation for it; the aggregator consequently learns which
  `theta` a site could not evaluate, and that residual side channel is
  documented in
  [`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md).

- Distinguish "non-evaluable" from "unreachable".:

  `NA` means *this `theta` broke my solver* — the optimizer will back
  off and try a different parameter, which is the right response. A
  network, authentication, or timeout failure is a different event, and
  backing off to another `theta` does nothing about it. Signal
  [`site_unavailable()`](https://bnaras.github.io/homomorpheR/reference/site_unavailable.md)
  instead. Never return `NA` for an unreachable service.

- Do not drop out silently.:

  A round sums over all sites. A site that quietly returns nothing
  changes the objective function between optimizer iterations, so the
  fit converges to something that is not the estimand, with no error
  raised anywhere. Aborting the round is always preferable.

- Be deterministic in `theta`.:

  The same `theta` must give the same contribution. Optimizers estimate
  gradients by finite differences, so a service that re-samples or
  jitters its answer turns the gradient into noise — with
  [`optim()`](https://rdrr.io/r/stats/optim.html)'s default
  `ndeps = 1e-3` the amplification is roughly 700-fold. Determinism also
  makes retries safe.

- Budget timeouts against call count.:

  A single `mle()` fit may query every site hundreds of times. A
  per-call timeout that looks reasonable in isolation is not.

- With a
  [ThresholdMaster](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md),
  availability is not optional.:

  Decryption is n-of-n, so an unreachable site withholds a partial
  decryption and the round cannot be decrypted at all. Under a
  [CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md)
  an unavailable site costs you a summand; under threshold keys it costs
  you the entire result.

## See also

[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md),
[`site_unavailable()`](https://bnaras.github.io/homomorpheR/reference/site_unavailable.md),
[LocalSite](https://bnaras.github.io/homomorpheR/reference/LocalSite.md)
