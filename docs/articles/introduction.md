# Introduction to homomorpheR

## The problem this package addresses

A recurring situation in biomedical research: several sites hold patient
records that cannot leave the institutions that collected them, and you
would like to fit a single model to the pooled data.

For a large class of models this is less hopeless than it sounds. The
log-likelihood, the score, and the information matrix are all *sums over
observations*, so they are also sums over sites. An optimizer fitting
such a model never needs an individual record — it needs the total of
the per-site contributions at whatever parameter value it is currently
considering. If the sites can compute that total without revealing their
individual contributions, the fit proceeds exactly as it would on pooled
data.

Homomorphic encryption is what makes the total computable. It is a form
of encryption under which certain arithmetic still works: you can add
two encrypted numbers and get an encryption of their sum, without
decrypting either one. The party doing the adding learns nothing.

`homomorpheR` supplies the coordination layer — the sites, the
aggregator, and the protocol connecting them — on top of the encryption
provided by the `openfhe.R` package.

The point worth keeping in view is that **the statistical machinery does
not change**. The vignettes here fit Cox models with `survival`, maximum
likelihood with [`stats4::mle()`](https://rdrr.io/r/stats4/mle.html),
and penalized regression with `CVXR`, all unmodified. Only the function
that returns the summary at each iteration is different.

## Terms you will meet

Encryption brings its own vocabulary, and a few of those words appear in
the function names and in the vignettes that follow. None of them
require a cryptography background. Where a word has a standard
cryptographic name that you will meet in the `openfhe.R` documentation
or in the literature, it is given in parentheses on first mention and
then set aside in favor of the plainer term.

- **Encrypted value** (*ciphertext*). The result of encrypting a number
  or a vector of numbers. Encrypted values can be added to each other,
  and multiplied, without being decrypted first. Every encrypted
  quantity in these vignettes is one of these.

- **Cleartext value** (*plaintext*). The ordinary, unencrypted number —
  what you started with, and what you get back after decryption. Note
  that in the encryption API this word also names an intermediate
  *encoded* form, produced just before encryption; that is why function
  names such as
  [`make_ckks_packed_plaintext()`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)
  contain it.

- **Slot.** An encrypted value is not a single number but a vector with
  a fixed number of positions, typically thousands. Each position is a
  slot. One arithmetic operation acts on every slot at once, which is
  what makes encrypted vector arithmetic affordable. You generally use
  the first few slots and ignore the rest.

- **Site.** A party holding data that must not leave its institution. In
  this package a site is an object carrying its local data and a
  function that returns the site’s summary at a given parameter value.
  Sites never share their data, and never see each other’s
  contributions.

- **Aggregator** (*computing party*, *evaluator*). The party that
  collects the encrypted per-site summaries, adds them together while
  they are still encrypted, and obtains the total. It sees only
  encrypted quantities in transit.

- **Public and secret key.** The standard public-key pair. Encryption
  uses the public key, which everyone may hold; decryption uses the
  secret key. Who holds the secret key is the central design question in
  these protocols, and the answer distinguishes the two kinds of
  aggregator below.

- **Evaluation keys.** Additional keys that *authorize* particular
  operations on encrypted values — multiplying two of them, summing
  across slots, rotating a vector. They are generated alongside the key
  pair and travel with the public key. They permit computation; they do
  not permit decryption.

- **Threshold keys.** A key arrangement in which no single party holds
  the secret key. Each party holds only a share, the public key is built
  jointly from all of them, and decryption requires every party to
  contribute. No proper subset can decrypt anything. This removes the
  residual trust that an ordinary key pair leaves with whoever holds the
  secret key.

- **Precision budget** (*multiplicative depth*, *levels*). Real-valued
  encrypted arithmetic is approximate, and each multiplication consumes
  part of a finite budget of precision fixed when the encryption
  parameters are chosen. Additions are essentially free; multiplications
  are not. When the budget is exhausted, further multiplications return
  values that are simply wrong. The aggregation patterns in these
  vignettes are deliberately shallow — mostly sums — so a small budget
  suffices.

- **Scheme.** The particular encryption construction in use. Two appear
  here. **CKKS** handles real numbers approximately and is what nearly
  every vignette uses, since statistical quantities are real-valued.
  **BFV** handles integers exactly, and is used where the answer is a
  count and no approximation is acceptable. The choice is an
  implementation detail of each vignette, stated in its opening
  paragraph.

That is the whole vocabulary. The vignettes that follow describe the
protocols using only these terms.

## How the pieces fit together

Three objects carry the protocol.

A **site** is built with
[`make_site()`](https://bnaras.github.io/homomorpheR/reference/make_site.md).
It holds the site’s data and a function `local_fn(data, theta)`
returning that site’s summary at the parameter value `theta`. If a
parameter value breaks the local computation — an extreme value that the
local solver cannot handle — the function returns `NA`, and that signal
propagates back to the optimizer rather than corrupting the fit.

An **aggregator** is built with either
[`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)
or
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md).
The two differ in exactly one respect, and it is the respect that
matters:

- [`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)
  creates an aggregator holding an ordinary key pair. It is appropriate
  when one party is permitted to hold the secret key.
- [`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
  creates an aggregator using threshold keys, where no single party —
  including the aggregator itself — can decrypt alone.

Because both are the same kind of object underneath, the protocol body
is identical for either. Choosing a trust model means choosing a
constructor, not rewriting the analysis.

**`master_aggregate(master, theta)`** runs one round. The aggregator
sends `theta` to every site; each site computes its local summary and
encrypts it; the aggregator adds the encrypted summaries together and
decrypts only the total. An optimizer calls this once per iteration, and
the fit proceeds.

The topology is a flat fan-out and fan-in, which is how federated
analysis frameworks such as `distcomp` and DataSHIELD are actually
deployed.

### Sites that are not in this R session

Every vignette here runs all parties inside one R session, so that each
is reproducible by running it. That is a simulation of the deployment,
not the deployment itself, and it is worth being explicit about what
changes when the sites are genuinely remote.

The structure does not change. You still construct one `Site` object per
site, in the aggregator’s own session. The object is a *local handle*
for a remote party, not the party itself. What changes is what its
function does: instead of computing a summary from data held in memory,
it issues a request to that site’s service and returns what comes back.
The `data` property then carries whatever the handle needs to make the
call — an endpoint, a credential, a connection object — rather than
patient records. Nothing in
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
needs to know the difference; it calls the function once per iteration
either way.

One detail matters if you build this. As the aggregation body is
currently written, the aggregator encrypts each site’s summary *after*
receiving it:
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
calls the site’s function, gets an ordinary number back, and encrypts
that. Inside a single R session nothing travels between parties, so this
is harmless and keeps the vignettes readable. Across a network it would
not be harmless — the summary would leave the site unencrypted, which is
the one thing the protocol exists to prevent. A real deployment must
encrypt at the site, before anything is transmitted. `Site` objects can
already be given the aggregator’s public key with
[`set_public_key()`](https://bnaras.github.io/homomorpheR/reference/set_public_key.md),
so the pieces are present; routing the aggregation body through them is
a change this package has not yet made.

## What “the right answer” means

*Precision* is the companion to this page. It sets out what an encrypted
result being correct actually means — exact for integer counting,
approximate within a measurable bound for real-valued arithmetic, and
merely statistical when an optimizer is involved — and which comparisons
are not meaningful at all. Worth reading before interpreting any number
in the vignettes below.

## Which vignette to read

**Queries and aggregation.** Counting across sites without revealing who
contributed what. These are the simplest complete protocols in the
package and the best place to begin: the statistical content is a sum,
so nothing distracts from the mechanics.

- *Privacy-Preserving Count Aggregation* — a single encrypted total,
  with one party holding the secret key.
- *Distributed Query Count with Threshold Keys* — the same count with no
  single party able to decrypt.

**Fitting models across sites.** Fitting a model to data you cannot
pool. Each of these wraps an ordinary R fitting routine that is used
unmodified.

- *Distributed Maximum Likelihood Estimation* — the smallest complete
  model fit, and the place to start in this group.
- *Distributed Stratified Cox Regression* — survival analysis across
  sites, using `survival` unchanged.
- *Distributed Cox Regression with Threshold Key Generation* — the same
  fit with no single party able to decrypt.
- *Federated Consensus ADMM with CVXR* — convex optimization across
  sites, using `CVXR` unchanged.
- *Federated Cox-Lasso via Consensus ADMM on DLBCL* — the above at
  realistic scale on gene expression data.

**Prediction and retrieval.** Two parties, one holding a model and one
holding data, neither willing to reveal theirs.

- *Secure Model Inference on Encrypted Data*
- *Encrypted Logistic Regression Prediction*
- *Federated Cosine Similarity with Site-Private Fine-Tuned Models*

**Adding differential privacy.** What changes if you also want to bound
what the sequence of released totals reveals. These are demonstrations
of the composition mechanics, not a recommendation.

- *Threshold Cox + Output Differential Privacy (Demonstration)*
- *Consensus ADMM + Output Differential Privacy (Demonstration)*

For the encryption layer itself — contexts, key generation, encrypted
arithmetic, serialization — see the `openfhe.R` package and its own
vignettes.

## A note on Paillier

Earlier versions of this package implemented the Paillier cryptosystem
natively in R. That code is frozen: it remains exported for the packages
that depend on it, but it is not extended, and the protocols documented
here use the schemes provided through `openfhe.R` instead. The Paillier
vignettes are retained separately for reference.
