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
  package, so subclass it and register a `contribute()` method.
- `contribute(site, theta)` is the single call a master makes on a site.
  It returns the site's contribution **already encrypted**, so no
  individual cleartext value reaches the aggregator.
- Two failure modes are deliberately distinct: returning `NA` means
  `theta` is non-evaluable at a site that answered, while
  `site_unavailable()` signals that the site could not be reached and
  aborts the round rather than silently changing the set of sites being
  summed over.
- `make_worker(name, data, contribution_fn)` replaces
  `make_site(name, data, local_fn)`.
- Masters: `make_ckks_master()` when one party may hold the secret key,
  `make_threshold_master()` when none may, and the frozen
  `make_master()` for Paillier.
- `master_aggregate(master, theta)` runs the star (master/worker)
  topology that distcomp- and DataSHIELD-style analyses use.
  `round_robin_chain()` / `run_round_robin()` remain for the frozen
  Paillier chain idiom.

### Threshold key generation

- `make_threshold_master(name, cc, sites)` runs the key-generation chain
  *through* the sites: each generates its own secret share, keeps it,
  and passes on only a public key. The master holds the crypto context
  and the joint public key, and no secret material at all.
- Sites must therefore be constructed before the master, since the joint
  public key is a function of all of them.
- `master_decrypt()` recovers a value by asking every site for a partial
  decryption and fusing the results. No party, the master included, can
  decrypt alone.
- New generics `keygen_round()` and `partial_decrypt()`, dispatching on
  `Site`. A `RemoteSite` subclass must implement both; the defaults
  refuse rather than generate a remote party's share locally.
- The help page states what the construction does not defend against:
  participants are assumed to follow the protocol, and a deviating site
  can return a dishonest contribution or a malformed partial that
  corrupts the result silently.

### Encryption surface

- `encrypt_under(params, value)` is the one encryption entry point. It
  takes public parameters and no party at all, because encryption needs
  only public material and belongs to no one in particular. A site
  passes the parameters it holds, `site@state$params`.
- **A site is autonomous once constructed.** It is given its public
  parameters once, when it is wired, and from then on computes and
  encrypts without consulting anyone. There is deliberately no exported
  function that reaches from a site back to a master; the public
  parameters themselves are not exported either, since fetching them at
  encryption time would mean asking for something already held.
- Consequently there is **no `master_encrypt()`**. Naming an encryption
  entry point after one party would advertise a privilege that does not
  exist, and would invite site-side code to hold a master it has no use
  for.
- `master_decrypt()` does take a master, and that asymmetry is the point:
  decryption is privileged, requiring secret material or the standing to
  convene every site, while encryption is not. It is vector-aware via
  `len`.

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
  the public key they were encrypted under. R's arithmetic operators
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

- Added MPC vignettes

## Versions up to 0.2.x

- Initial versions
