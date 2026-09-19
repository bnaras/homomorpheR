# A site whose contribution is produced outside this R session

Abstract.
[homomorpheR](https://bnaras.github.io/homomorpheR/reference/homomorpheR.md)
deliberately ships **no** implementation: transports differ too much,
and a crypto package has no business carrying an HTTP client. Subclass
it, add whatever properties your transport needs, and register methods:

## Usage

``` r
RemoteSite(name = character(0), state = new.env(parent = emptyenv()))
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

## Value

nothing — this class is abstract. Subclass it as shown above.

## Details

    HttpSite <- S7::new_class("HttpSite", parent = RemoteSite,
                              properties = list(url = S7::class_character))
    S7::method(set_public_params, HttpSite) <- function(site, params) {
        ## ... POST the public context and key to site@url; the far end
        ##     stores them. Nothing secret travels.
    }
    S7::method(contribute, HttpSite) <- function(site, theta) {
        ## ... call site@url with theta; the far end encrypts ...
    }

## What this class is, and is not

A `RemoteSite` is an **architectural seam with a documented contract**,
not a trust boundary the package establishes. Three cases are worth
keeping apart:

- A
  [LocalSite](https://bnaras.github.io/homomorpheR/reference/LocalSite.md)
  demonstration.:

  Data, key shares, sites, and the aggregating party are all objects in
  one R process. The classes model the protocol's *roles*; they do not
  create a process or trust boundary, and nothing prevents one object
  from reaching another. This is the right scope for a vignette.

- A single-decrypter deployment.:

  Each site returns a ciphertext, but a
  [CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md)
  holds the secret key and could decrypt an individual contribution.
  "Only the aggregate is decrypted" describes what
  [`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
  does, not something the cryptography enforces.

- A remote threshold deployment.:

  Separately controlled endpoints keep their own shares and return
  ciphertexts or partial decryptions. Here the party boundary is real —
  provided *your* transport, authentication, endpoint code, and key
  storage implement it.
  [homomorpheR](https://bnaras.github.io/homomorpheR/reference/homomorpheR.md)
  supplies none of those, and detects no deliberately dishonest reply.

What the package does enforce: a site cannot be configured except
through
[`set_public_params()`](https://bnaras.github.io/homomorpheR/reference/set_public_params.md),
which the base `RemoteSite` method refuses, so an endpoint that was
never provisioned fails closed rather than looking wired; the base class
likewise refuses
[`keygen_round()`](https://bnaras.github.io/homomorpheR/reference/keygen_round.md)
and
[`partial_decrypt()`](https://bnaras.github.io/homomorpheR/reference/partial_decrypt.md)
rather than performing a remote party's secret-key operation in this
process; and
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
checks that a reply is an encrypted value under this protocol's key
before adding it to a total.

## The contract an implementation must honor

- Provision the far end at setup.:

  Implement
  [`set_public_params()`](https://bnaras.github.io/homomorpheR/reference/set_public_params.md)
  to send the public context and key to the endpoint and have it retain
  them. This is one of only two moments anything passes between the
  parties — the other being a round. Only public material travels.

- Return a ciphertext, never a plain number.:

  The remote end was given the public parameters when it was wired, so
  it encrypts *before* the value crosses the wire. A `RemoteSite` that
  returns cleartext hands the aggregator an individual per-site
  contribution, which is precisely what the protocol exists to prevent —
  [`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
  now refuses such a reply, but an honest implementation should not
  produce one. `NA` is the one permitted plaintext reply, because CKKS
  has no representation for it; the aggregator consequently learns which
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

## What the package leaves to you

Transport, identity, authentication, attestation, remote key storage,
serialization of the parameter bundle, retry and timeout policy — and
any defense against a party that deviates from the protocol rather than
merely observing it. The trust model throughout is honest-but-curious.

## See also

[`set_public_params()`](https://bnaras.github.io/homomorpheR/reference/set_public_params.md),
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md),
[`keygen_round()`](https://bnaras.github.io/homomorpheR/reference/keygen_round.md),
[`partial_decrypt()`](https://bnaras.github.io/homomorpheR/reference/partial_decrypt.md),
[`site_unavailable()`](https://bnaras.github.io/homomorpheR/reference/site_unavailable.md),
[LocalSite](https://bnaras.github.io/homomorpheR/reference/LocalSite.md)
