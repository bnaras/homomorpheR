# Changelog

## Version 1.3.0

### Encryption backends

- New OpenFHE backend, via the `openfhe.R` package. CKKS carries
  real-valued data natively, so the fixed-point denominator the Paillier
  path needs is gone; BFV and BGV carry exact integers. The scheme is
  read back from the crypto context, so one master drives any of them.
- The Paillier implementation remains and is frozen: it is still
  exported and tested, but new work should use the OpenFHE backends.

### Multi-party actors

- `Site` is now abstract, with two concrete kinds. `LocalSite` holds its
  data in the current R session. `RemoteSite` is for a party whose
  contribution is produced elsewhere; no transport ships with the
  package, so subclass it and register a
  [`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
  method.
- `contribute(site, theta)` is the single call a master makes on a site.
  It returns the site’s contribution **already encrypted**, so no
  individual cleartext value reaches the aggregator.
- Two failure modes are deliberately distinct: returning `NA` means
  `theta` is non-evaluable at a site that answered, while
  [`site_unavailable()`](https://bnaras.github.io/homomorpheR/reference/site_unavailable.md)
  signals that the site could not be reached and aborts the round rather
  than silently changing the set of sites being summed over.
- `make_worker(name, data, contribution_fn)` replaces
  `make_site(name, data, local_fn)`.
- Masters:
  [`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)
  when one party may hold the secret key,
  [`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
  when none may, and the frozen
  [`make_master()`](https://bnaras.github.io/homomorpheR/reference/make_master.md)
  for Paillier.
- `master_aggregate(master, theta)` runs the star (master/worker)
  topology that distcomp- and DataSHIELD-style analyses use.
  [`round_robin_chain()`](https://bnaras.github.io/homomorpheR/reference/round_robin_chain.md)
  /
  [`run_round_robin()`](https://bnaras.github.io/homomorpheR/reference/run_round_robin.md)
  remain for the frozen Paillier chain idiom.

### Threshold key generation

- `make_threshold_master(name, cc, sites)` runs the key-generation chain
  *through* the sites: each generates its own secret share, keeps it,
  and passes on only a public key. The master holds the crypto context
  and the joint public key, and no secret material at all.
- Sites must therefore be constructed before the master, since the joint
  public key is a function of all of them.
- [`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
  recovers a value by asking every site for a partial decryption and
  fusing the results. No party, the master included, can decrypt alone.
- New generics
  [`keygen_round()`](https://bnaras.github.io/homomorpheR/reference/keygen_round.md)
  and
  [`partial_decrypt()`](https://bnaras.github.io/homomorpheR/reference/partial_decrypt.md),
  dispatching on `Site`. A `RemoteSite` subclass must implement both;
  the defaults refuse rather than generate a remote party’s share
  locally.
- The help page states what the construction does not defend against:
  participants are assumed to follow the protocol, and a deviating site
  can return a dishonest contribution or a malformed partial that
  corrupts the result silently.

### Setup, and what a remote site must implement

- `set_public_params(site, params)` is the setup seam: the one moment a
  coordinating party hands a site anything, apart from a round itself.
  The `Site` method stores the bundle; the `RemoteSite` method
  **refuses**, because storing it would configure the local proxy and
  leave the far endpoint untold. Wiring a remote subclass that has not
  implemented provisioning now fails immediately instead of producing a
  site that looks configured and is not.
- `site_params(site)` reads back what a site holds, and errors if it was
  never configured. Use it instead of reaching into `site@state$params`.
- Public parameters are now typed S7 objects — `PublicParams` with
  `OpenFHEParams` and the frozen `PaillierParams` — rather than a list
  with a scheme string. “The bundle carries no secret material” is now a
  property of the class rather than a promise about a list, and the
  objects print to show it.
- [`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
  checks each reply before adding it to a total: a site that answered in
  cleartext, or with a value produced under some other key, is refused.
  Previously a cleartext reply was folded in by ordinary scalar addition
  and the round returned the right answer, having been handed the one
  quantity the protocol exists to hide.
- [`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
  and site-side
  [`partial_decrypt()`](https://bnaras.github.io/homomorpheR/reference/partial_decrypt.md)
  likewise verify that a value belongs to this protocol’s key, using
  OpenFHE’s key tag. A site therefore refuses to apply its own share to
  a ciphertext from a protocol it did not join.
- [`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
  rejects a site listed twice, two sites with the same name, and a site
  already serving another protocol; a ceremony that fails part-way rolls
  back, leaving the sites it had visited clean enough to retry.
- [`?RemoteSite`](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md)
  now separates three cases that were previously run together: a
  `LocalSite` demonstration models the protocol’s roles in one R session
  and is not a trust boundary; a single-decrypter deployment relies on
  honest execution rather than cryptography; only a remote threshold
  deployment gives a real party boundary, and only if your transport,
  authentication, and key storage provide one.

### Encryption surface

- `encrypt_under(params, value)` is the one encryption entry point. It
  takes public parameters and no party at all, because encryption needs
  only public material and belongs to no one in particular. A site
  passes the parameters it holds, from `site_params(site)`.
- **A site is autonomous once configured.** It is given its public
  parameters once, and from then on computes and encrypts without
  consulting anyone. There is deliberately no exported function that
  reaches from a site back to a master; the master-side
  `public_params()` is not exported either, since fetching it at
  encryption time would mean asking for something already held.
- Consequently there is **no `master_encrypt()`**. Naming an encryption
  entry point after one party would advertise a privilege that does not
  exist, and would invite site-side code to hold a master it has no use
  for.
- [`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
  does take a master, and that asymmetry is the point: decryption is
  privileged, requiring secret material or the standing to convene every
  site, while encryption is not. It is vector-aware via `len`.
- Under BFV and BGV a value the scheme cannot carry is now refused
  rather than coerced: a non-integer, a non-finite value, one outside
  R’s integer range, or one at or beyond half the plaintext modulus.
  [`as.integer()`](https://rdrr.io/r/base/integer.html) previously
  turned a contribution of `0.9` into `0` and reported a total of zero
  without a warning. The one thing no party can check is the *total*,
  which still wraps if it exceeds the modulus;
  [`?ThresholdMaster`](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md)
  says so.
- [`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)
  requires a CKKS context. Exact-integer work goes through
  [`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md),
  which is scheme-agnostic by design.
- Crypto contexts, key pairs, public keys, `state` environments, and
  `contribution_fn` are now typed S7 properties rather than `class_any`,
  and a party’s `name` must be a single non-empty string.

### Data

- `DLBCL` (235 patients: survival, subgroup, gene-expression
  signatures), `DLBCL_gex` (235 x 6416 Lymphochip probes), and
  `cvxr_consensus` (the recorded encrypted Cox-lasso consensus-ADMM
  fit).

### Vignettes

- Fourteen vignettes, all executing at build time with computed output,
  covering queries and aggregation, model fitting across sites,
  prediction and retrieval, and differential privacy; the Paillier-era
  vignettes have moved to `paillier-archive/` and are no longer built.

### Infrastructure

- Migrated from R6 to S7 throughout. The `R6` dependency is removed.
- New `PaillierCiphertext` class wraps encrypted values together with
  the public key they were encrypted under. R’s arithmetic operators
  (`+`, `-`, `*` against a cleartext scalar) now dispatch directly on
  encrypted values via an S3 `Ops` group handler, so computations on
  encrypted data read like ordinary R arithmetic.
- Paillier API renames:
  - `PaillierKeyPair$new(bits)` → `paillier_keypair(modulus_bits)`
  - `pubkey$encrypt(m)` → `encrypt(pubkey, m)` (S7 generic)
  - `privkey$decrypt(ct)` → `decrypt(privkey, ct)` (S7 generic)
  - `pubkey$add(a, b)` / `pubkey$sub(a, b)` → `a + b` / `a - b`
  - `pubkey$mult(ct, k)` → `ct * k` (cleartext scalar)
  - `keys$getPrivateKey()` → `get_private_key(keys)`
  - `privkey$getLambda()` → `get_lambda(privkey)`
- Field access: `keys$pubkey` → `keys@pubkey`, etc.
- Frozen Paillier-era code is separated into its own source file from
  the supported actor surface.
- Minimum R version bumped to 4.3.0 (required for S7 `@` and
  `chooseOpsMethod`). `digest` dropped from Suggests. Messaging goes
  through `cli`.

## Version 0.3

CRAN release: 2025-04-09

- Added MPC vignettes
