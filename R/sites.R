#' @importFrom S7 new_class new_generic new_object method method<- class_any class_character class_environment class_function class_integer S7_object S7_inherits S7_dispatch
NULL

## Site / Master actor classes -- the supported (CKKS / threshold)
## surface.
##
## These are the building blocks for multi-site protocols. A `Site`
## holds whatever it needs to answer a query and a
## `contribution_fn(data, theta)` that returns its contribution to the
## round; a `Master` owns the keys and orchestrates the
## protocol. The frozen Paillier-era actors (PaillierMaster, the
## NCParty topology, the round-robin chain) live in sites_legacy.R.
##
## Actors carry an environment-backed `state` property for mutable
## bookkeeping (the public parameters a site was given, its own key
## share, the next site in a legacy chain, intermediate results).
##
## The environment is there because S7 objects have copy-on-modify
## value semantics: `obj@prop <- x` yields a *modified copy*, so a
## function cannot record anything in an actor its caller passed in.
## An environment is shared by reference, which is what an actor with
## a lifetime needs. The properties themselves are writable, as
## ordinary S7 properties are -- it is the copy that makes them
## useless for bookkeeping, not any enforced immutability.

# ---- Validators shared across the actor classes ---------------------------

## A party's name appears in every error message about it, so a name
## that is empty, missing, or a vector makes those messages useless
## exactly when they matter. Rejecting at construction is cheaper than
## a puzzling message three rounds into a protocol.
.check_name <- function(x) {
    if (length(x) != 1L || is.na(x) || !nzchar(x))
        "@name must be a single non-empty string"
}

# ---- Classes --------------------------------------------------------------

#' A site in a multi-party protocol
#'
#' Abstract base for the two kinds of participating party: a
#' [LocalSite], whose data is in this \R session, and a [RemoteSite],
#' whose contribution is produced elsewhere. Both answer the same
#' generic, [contribute()], and are indistinguishable to whoever asks:
#' each returns an *encrypted* contribution at the requested parameter.
#'
#' A site is autonomous once constructed. It is given public parameters
#' once, when it is wired, and from then on it computes and encrypts
#' entirely on its own — it holds no reference to the party that
#' aggregates its answers, and needs none.
#'
#' @param name short identifier shown in printed output. A single
#'   non-empty string; it names the site in every error message, so an
#'   empty or vectorized name is rejected at construction.
#' @param state an environment for mutable bookkeeping — the public
#'   parameters the site was given when it was configured, its own key
#'   share under threshold keys, and, on the frozen legacy path, the
#'   next link in the round-robin chain. Default: a fresh empty env.
#' @return nothing — this class is abstract, so calling it raises an
#'   error instead of returning an object. It is the common parent of
#'   [LocalSite] and [RemoteSite], and the dispatch target for
#'   [contribute()]. Construct a co-located site with [make_worker()].
#' @seealso [LocalSite], [RemoteSite], [contribute()]
#' @export
Site <- new_class(
    "Site",
    abstract  = TRUE,
    package   = "homomorpheR",
    properties = list(
        name  = class_character,
        state = class_environment
    ),
    validator = function(self) .check_name(self@name)
)

#' A site whose data lives in this \R session
#'
#' The ordinary case: the records are here, and `contribution_fn` is
#' evaluated in-process. [contribute()] computes the contribution and
#' **encrypts it** with the public parameters the site was given when
#' it was configured, so what leaves is already a ciphertext.
#'
#' A `LocalSite` demonstrates the protocol's roles inside one \R
#' session. It is not a deployment boundary: its data, and under
#' threshold keys its key share, are objects in this process, and
#' anything else in this process can reach them. Separating the
#' parties for real means separately controlled processes, which is
#' what [RemoteSite] is for.
#'
#' @inheritParams Site
#' @param data whatever `contribution_fn` needs in order to answer — a
#'   dataset, a database connection, a cohort identifier.
#' @param contribution_fn a function with signature `function(data, theta)`
#'   returning this site's contribution at `theta` as a plain numeric
#'   value. It does **not** encrypt; [contribute()] does that. May
#'   return `NA` to signal that `theta` is non-evaluable here.
#' @return an S7 object of class `LocalSite`. Construct with
#'   [make_worker()].
#' @export
LocalSite <- new_class(
    "LocalSite",
    parent  = Site,
    package = "homomorpheR",
    properties = list(
        ## `data` is deliberately untyped: it is whatever the site's
        ## own function needs -- a data frame, a DBI connection, a
        ## cohort identifier. `contribution_fn` is not, because a
        ## non-function there fails at the first round rather than at
        ## construction.
        data            = class_any,
        contribution_fn = class_function
    )
)

#' A site whose contribution is produced outside this \R session
#'
#' Abstract. [homomorpheR] deliberately ships **no** implementation:
#' transports differ too much, and a crypto package has no business
#' carrying an HTTP client. Subclass it, add whatever properties your
#' transport needs, and register methods:
#'
#' ```
#' HttpSite <- S7::new_class("HttpSite", parent = RemoteSite,
#'                           properties = list(url = S7::class_character))
#' S7::method(set_public_params, HttpSite) <- function(site, params) {
#'     ## ... POST the public context and key to site@url; the far end
#'     ##     stores them. Nothing secret travels.
#' }
#' S7::method(contribute, HttpSite) <- function(site, theta) {
#'     ## ... call site@url with theta; the far end encrypts ...
#' }
#' ```
#'
#' @section What this class is, and is not:
#'
#' A `RemoteSite` is an **architectural seam with a documented
#' contract**, not a trust boundary the package establishes. Three
#' cases are worth keeping apart:
#'
#' \describe{
#'   \item{A [LocalSite] demonstration.}{Data, key shares, sites, and
#'     the aggregating party are all objects in one \R process. The
#'     classes model the protocol's *roles*; they do not create a
#'     process or trust boundary, and nothing prevents one object from
#'     reaching another. This is the right scope for a vignette.}
#'   \item{A single-decrypter deployment.}{Each site returns a
#'     ciphertext, but a [CKKSMaster] holds the secret key and could
#'     decrypt an individual contribution. "Only the aggregate is
#'     decrypted" describes what [master_aggregate()] does, not
#'     something the cryptography enforces.}
#'   \item{A remote threshold deployment.}{Separately controlled
#'     endpoints keep their own shares and return ciphertexts or
#'     partial decryptions. Here the party boundary is real — provided
#'     *your* transport, authentication, endpoint code, and key
#'     storage implement it. [homomorpheR] supplies none of those, and
#'     detects no deliberately dishonest reply.}
#' }
#'
#' What the package does enforce: a site cannot be configured except
#' through [set_public_params()], which the base `RemoteSite` method
#' refuses, so an endpoint that was never provisioned fails closed
#' rather than looking wired; the base class likewise refuses
#' [contribute()], [keygen_round()] and [partial_decrypt()] rather than
#' evaluating a remote party's data or performing its secret-key
#' operation in this process; and
#' [master_aggregate()] checks that a reply is an encrypted value
#' under this protocol's key before adding it to a total.
#'
#' @section The contract an implementation must honor:
#'
#' \describe{
#'   \item{Provision the far end at setup.}{Implement
#'     [set_public_params()] to send the public context and key to the
#'     endpoint and have it retain them. This is one of only two
#'     moments anything passes between the parties — the other being
#'     a round. Only public material travels.}
#'   \item{Return a ciphertext, never a plain number.}{The remote end
#'     was given the public parameters when it was wired, so it
#'     encrypts *before* the value crosses the wire. A `RemoteSite`
#'     that returns cleartext hands the aggregator an individual
#'     per-site contribution, which is precisely what the protocol
#'     exists to prevent — [master_aggregate()] now refuses such a
#'     reply, but an honest implementation should not produce one.
#'     `NA` is the one permitted plaintext reply, because CKKS has no
#'     representation for it; the aggregator consequently learns which
#'     `theta` a site could not evaluate, and that residual side
#'     channel is documented in [master_aggregate()].}
#'   \item{Distinguish "non-evaluable" from "unreachable".}{`NA` means
#'     *this `theta` broke my solver* — the optimizer will back off and
#'     try a different parameter, which is the right response. A
#'     network, authentication, or timeout failure is a different
#'     event, and backing off to another `theta` does nothing about it.
#'     Signal [site_unavailable()] instead. Never return `NA` for an
#'     unreachable service.}
#'   \item{Do not drop out silently.}{A round sums over
#'     all sites. A site that quietly returns nothing changes the
#'     objective function between optimizer iterations, so the fit
#'     converges to something that is not the estimand, with no error
#'     raised anywhere. Aborting the round is always preferable.}
#'   \item{Be deterministic in `theta`.}{The same `theta` must give the
#'     same contribution. Optimizers estimate gradients by finite
#'     differences, so a service that re-samples or jitters its answer
#'     turns the gradient into noise — with `optim()`'s default
#'     `ndeps = 1e-3` the amplification is roughly 700-fold.
#'     Determinism also makes retries safe.}
#'   \item{Budget timeouts against call count.}{A single `mle()` fit may
#'     query every site hundreds of times. A per-call timeout that
#'     looks reasonable in isolation is not.}
#'   \item{With a [ThresholdMaster], availability is not optional.}{
#'     Decryption is n-of-n, so an unreachable site withholds a partial
#'     decryption and the round cannot be decrypted at all. Under a
#'     [CKKSMaster] an unavailable site costs you a summand; under
#'     threshold keys it costs you the entire result.}
#' }
#'
#' @section What the package leaves to you:
#'
#' Transport, identity, authentication, attestation, remote key
#' storage, serialization of the parameter bundle, retry and timeout
#' policy — and any defense against a party that deviates from the
#' protocol rather than merely observing it. The trust model
#' throughout is honest-but-curious.
#'
#' @inheritParams Site
#' @return nothing — this class is abstract. Subclass it as shown above.
#' @seealso [set_public_params()], [contribute()], [keygen_round()],
#'   [partial_decrypt()], [site_unavailable()], [LocalSite]
#' @export
RemoteSite <- new_class(
    "RemoteSite",
    parent   = Site,
    abstract = TRUE,
    package  = "homomorpheR"
)

#' Abstract master class
#'
#' Common base for [CKKSMaster] and [ThresholdMaster] (and the frozen
#' legacy [PaillierMaster]). Concrete masters carry whatever context
#' and public keys their cryptographic backend needs; the protocol body
#' in [master_aggregate()] reaches sites through [contribute()] and
#' recovers the total through the [decrypt()] generic, which
#' dispatches on the concrete master class, so the same protocol runs
#' over any backend.
#'
#' A master never encrypts site data, and has no encryption entry point
#' at all. Each party encrypts its own values with [encrypt()], which
#' for a [Site] takes nothing but the site itself: it encrypts with
#' the public parameters it was handed when it was wired. The
#' asymmetry is deliberate and worth reading off the API: decryption is
#' privileged — it needs secret material, or the standing to convene
#' every site — while encryption needs only public material and is
#' available to anyone.
#'
#' @param name short identifier shown in printed output.
#' @param state an environment for mutable bookkeeping.
#' @return nothing — this class is abstract, so calling it raises an error
#'   instead of returning an object. It exists so that [decrypt()]
#'   and [master_aggregate()] dispatch on a common parent. Construct a
#'   concrete master with [make_ckks_master()] or
#'   [make_threshold_master()].
#' @export
Master <- new_class(
    "Master",
    abstract = TRUE,
    package = "homomorpheR",
    properties = list(
        name  = class_character,
        state = class_environment
    ),
    validator = function(self) .check_name(self@name)
)

#' CKKS-backed master
#'
#' A [Master] that drives the protocol over `openfhe.R`'s CKKS encryption.
#' CKKS handles real-valued arithmetic natively, so no `den`
#' denominator is needed. Constructed by [make_ckks_master()].
#'
#' @param name short identifier shown in printed output.
#' @param crypto_context an `openfhe.R` `CryptoContext` configured for
#'   CKKS.
#' @param keypair an `openfhe.R` `KeyPair`.
#' @param state an environment for mutable bookkeeping.
#' @return an S7 object of class `CKKSMaster`, inheriting from [Master], with
#'   properties `name`, `crypto_context`, `keypair` and `state`. It holds
#'   a single CKKS key pair, so it is the appropriate master when one
#'   party is allowed to hold the secret key; when no party may, use
#'   [ThresholdMaster]. Construct with [make_ckks_master()].
#' @export
CKKSMaster <- new_class(
    "CKKSMaster",
    parent  = Master,
    package = "homomorpheR",
    properties = list(
        crypto_context = openfhe.R::CryptoContext,
        keypair        = openfhe.R::KeyPair
    )
)

#' Threshold-CKKS master (n-of-n key generation)
#'
#' A [Master] that drives the protocol over `openfhe.R` with
#' threshold key generation, under whichever scheme the supplied
#' crypto context was built for (CKKS for real-valued work, BFV or
#' BGV for exact integer work). There is no single secret key: each
#' site generates and keeps its own share `sk_i`, and the joint public
#' key `pk_{1..n}` is built by chaining [keygen_round()] across the
#' sites. Encryption goes under `joint_pubkey`. Decryption requires
#' all `n` sites to return partial decryptions, which the master then
#' fuses.
#'
#' **The master has no secret-key or secret-share property, and its
#' methods use no secret material.** Its properties are the crypto
#' context and the joint public key, both public; the shares live at
#' the sites that generated them and never travel. That is what makes
#' the n-of-n claim true of the objects and not merely of the prose —
#' see [partial_decrypt()] for the decryption seam.
#'
#' Read that at the right scope. In a [LocalSite] demonstration every
#' role still inhabits one \R process, and the master holds the site
#' objects in order to query them, so the shares are reachable from
#' the master's object graph even though no property of the master
#' contains one. A boundary between the parties requires separately
#' controlled processes behind [RemoteSite].
#'
#' @section Exact-integer contexts:
#'
#' Under BFV or BGV a site cannot contribute a value the scheme
#' cannot carry: [contribute()] refuses a non-integer, a non-finite
#' value, or one outside the plaintext modulus rather than rounding
#' it. What no party can check is the *total*: a sum that exceeds the
#' modulus wraps, and the wrapped value decrypts as an ordinary
#' integer with nothing to mark it. Choose `plaintext_modulus` for
#' the largest total the protocol can produce, not the largest
#' summand.
#'
#' Constructed by [make_threshold_master()].
#'
#' @param name short identifier.
#' @param crypto_context an `openfhe.R` `CryptoContext` with the
#'   `MULTIPARTY` feature enabled.
#' @param joint_pubkey the joint public key produced by chaining
#'   [keygen_round()] across the sites.
#' @param state an environment for mutable bookkeeping (the wired
#'   sites, in the order the key-generation chain visited them).
#' @return an S7 object of class `ThresholdMaster`, inheriting from [Master],
#'   with properties `name`, `crypto_context`, `joint_pubkey` and
#'   `state`. It carries no secret key and no secret shares: decryption
#'   is driven by asking each site for a partial decryption and fusing
#'   the results, so no party — the master included — can decrypt
#'   alone. Construct with [make_threshold_master()].
#' @export
ThresholdMaster <- new_class(
    "ThresholdMaster",
    parent  = Master,
    package = "homomorpheR",
    properties = list(
        crypto_context = openfhe.R::CryptoContext,
        joint_pubkey   = openfhe.R::PublicKey
    )
)

# ---- Public parameters ----------------------------------------------------

#' The public parameters a party encrypts under
#'
#' Abstract base for the setup bundle a party is handed once, when it
#' is configured, and holds from then on. It is public in full: it is
#' exactly the message a coordinator would put on a wire to an
#' untrusted peer, and it is all anyone needs in order to encrypt.
#'
#' The class carries **no secret property**, which is what makes the
#' claim structural rather than a promise in prose — there is nowhere
#' for a secret key or a key share to travel in this object. The
#' concrete kinds are [OpenFHEParams] and, on the frozen legacy path,
#' `PaillierParams`.
#'
#' Obtain the bundle a site holds with [site_params()]; encrypt with
#' [encrypt()].
#'
#' @return nothing — this class is abstract. Its concrete subclasses
#'   are constructed for you when a party is configured.
#' @seealso [OpenFHEParams], [site_params()], [actor-encryption]
#' @export
PublicParams <- new_class(
    "PublicParams",
    abstract = TRUE,
    package  = "homomorpheR"
)

#' Public parameters for the `openfhe.R` backends
#'
#' The crypto context and the public key to encrypt under — the joint
#' public key when the protocol uses threshold keys. Both are public.
#' The scheme is read back from the context, so one class serves CKKS,
#' BFV, and BGV.
#'
#' @param cc an `openfhe.R` `CryptoContext`.
#' @param pk an `openfhe.R` `PublicKey`.
#' @return an S7 object of class `OpenFHEParams`, inheriting from
#'   [PublicParams], with properties `cc` and `pk`.
#' @seealso [actor-encryption], [site_params()]
#' @export
OpenFHEParams <- new_class(
    "OpenFHEParams",
    parent  = PublicParams,
    package = "homomorpheR",
    properties = list(
        cc = openfhe.R::CryptoContext,
        pk = openfhe.R::PublicKey
    )
)

## A short public fingerprint of the key a bundle encrypts under.
##
## OpenFHE stamps every key and every ciphertext with a key tag, and
## preserves it through homomorphic operations, so comparing tags
## answers "was this produced under my key?" exactly, on public data,
## without decrypting anything. That one equality is what turns the
## whole class of wrong-key failures -- a worker re-wired to a second
## master, a ciphertext from another protocol, a site's own share
## applied to a foreign value -- from a silent wrong number into an
## error. See notes/discoveries for what those failures look like
## untrapped: under BFV a re-keyed decryption returns a plausible
## integer and nothing is raised anywhere.
##
## Paillier has no such stamp; the modulus serves, since two keys
## differ in it with overwhelming probability.
#' @noRd
params_tag <- new_generic("params_tag", "params")

method(params_tag, OpenFHEParams) <- function(params)
    openfhe.R::get_key_tag(params@pk)

# ---- Constructors ---------------------------------------------------------

#' Construct a CKKS-backed master
#'
#' The context must be a CKKS one. A [CKKSMaster] built over BFV or
#' BGV would work arithmetically but every sentence of its
#' documentation, and the class name a user reads in printed output,
#' would be wrong about which scheme is in use; exact-integer work
#' goes through [make_threshold_master()], which is scheme-agnostic by
#' design and says so.
#'
#' @inheritParams CKKSMaster
#' @return a [CKKSMaster].
#' @export
make_ckks_master <- function(name, crypto_context, keypair) {
    .require_scheme(crypto_context, "CKKSRNS_SCHEME", "make_ckks_master")
    CKKSMaster(name           = name,
               crypto_context = crypto_context,
               keypair        = keypair,
               state          = new.env(parent = emptyenv()))
}

#' Run threshold key generation across sites and construct the master
#'
#' Drives the chained key-generation ceremony *through the sites* and
#' returns a master wired to them. The lead site generates a fresh
#' keypair `(pk_1, sk_1)`; each subsequent site `i` derives
#' `(pk_{1..i}, sk_i)` from its predecessor's cumulative public key.
#' The final `pk_{1..n}` is the joint public key under which
#' everything is encrypted.
#'
#' Each step runs at the site, through [keygen_round()]: the site
#' keeps `sk_i` in its own state and returns only the cumulative
#' *public* key. No share is ever generated centrally, and none is
#' returned to this function, so the master cannot hold one even by
#' accident. Only public keys travel between parties, which is exactly
#' what can be sent over a wire to an untrusted peer.
#'
#' Decryption is n-of-n: [decrypt()] asks each site for a
#' partial decryption via [partial_decrypt()] and fuses the results
#' with `multiparty_decrypt_fusion`. There is no path by which the
#' master decrypts alone.
#'
#' The returned master is already wired, so [set_workers()] is neither
#' needed nor permitted afterwards — the site order fixed by the
#' key-generation chain is the order partial decryptions must be
#' fused in, and re-wiring would break it.
#'
#' A ceremony that fails part-way — an unimplemented [RemoteSite], an
#' unreachable endpoint, a context without `MULTIPARTY` — leaves no
#' trace on the sites it had already visited: their shares and
#' parameters are cleared before the error propagates, so the same
#' sites can be used again once the cause is fixed. For a
#' [RemoteSite] that undo reaches the local proxy only, so a remote
#' implementation should tolerate a repeated ceremony.
#'
#' @section What this does not defend against:
#'
#' The construction assumes participants follow the protocol
#' (honest-but-curious). A site that deviates can (a) return a
#' well-formed ciphertext that is not its honest contribution, (b)
#' return a malformed partial decryption, which corrupts the fused
#' plaintext *silently* — nothing in the scheme detects it — or (c)
#' contribute a degenerate share during key generation, weakening the
#' threshold. The chain is sequential, so each site also sees its
#' predecessors' cumulative public key; OpenFHE's multiparty key
#' generation carries no proofs of knowledge or commitments, so
#' rogue-key behavior is not prevented here. Defending against any of
#' this needs verifiable decryption and committed key generation,
#' neither of which this package provides.
#'
#' @param name short identifier.
#' @param crypto_context an `openfhe.R` `CryptoContext` (CKKS, BFV,
#'   or BGV) *with* the `MULTIPARTY` feature enabled. Pass
#'   `features = c(Feature$MULTIPARTY)` to `fhe_context()`. The
#'   scheme is read back from the context, so the same master drives
#'   the protocol over real-valued (CKKS) or exact-integer (BFV/BGV)
#'   arithmetic without further configuration.
#' @param sites a list of at least two **distinct, unconfigured**
#'   [Site]s, built with [make_worker()]. The first is the lead site.
#'   Each ends up holding its own secret share and the joint public
#'   key. Listing one site twice, or reusing a site that already holds
#'   a share or public parameters, is an error: the repeat would
#'   discard what the first round left behind, and under BFV or BGV
#'   nothing afterwards detects the loss.
#' @return a [ThresholdMaster], wired to `sites`.
#' @seealso [keygen_round()], [partial_decrypt()], [actor-encryption].
#' @export
make_threshold_master <- function(name, crypto_context, sites) {
    if (!is.list(sites))
        cli_abort("{.arg sites} must be a list of {.cls Site} objects.")
    n <- length(sites)
    if (n < 2)
        cli_abort("Threshold key generation requires at least two sites.")
    for (i in seq_len(n)) {
        s <- sites[[i]]
        if (!S7_inherits(s, Site))
            cli_abort("{.arg sites}[[{i}]] is not a {.cls Site}.")

        ## The same site twice is not two parties. Its second round
        ## overwrites the share its first round generated, so the
        ## joint key depends on a share nobody holds and every later
        ## decryption is wrong -- silently, under BFV and BGV, which
        ## have no approximation check to trip over.
        for (j in seq_len(i - 1L))
            if (identical(s@state, sites[[j]]@state))
                cli_abort(c("Sites {j} and {i} are the same party.",
                            i = "Threshold key generation needs {n} distinct
                                 parties; a repeated one overwrites the share
                                 it generated the first time, and nothing
                                 detects the loss afterwards."))

        ## Not a protocol failure, but it makes every later message
        ## about "site {.val X}" ambiguous.
        for (j in seq_len(i - 1L))
            if (identical(s@name, sites[[j]]@name))
                cli_abort("Sites {j} and {i} share the name {.val {s@name}}.")

        if (!is.null(s@state$sk) || !is.null(s@state$params))
            cli_abort(c("Site {.val {s@name}} is already taking part in a protocol.",
                        i = "A key-generation ceremony starts from unconfigured
                             sites: joining a second one would discard the share
                             and the parameters the first left behind.",
                        i = "Build a fresh site with {.fun make_worker}."))
    }

    ## The chain runs site to site. Each call returns a public key and
    ## nothing else; the share stays where it was generated.
    ##
    ## If any step fails -- a RemoteSite with no implementation, an
    ## unreachable endpoint, a context without MULTIPARTY -- the sites
    ## already visited hold a share belonging to a ceremony that will
    ## never complete, and the checks above would then refuse them a
    ## retry. Undo the visit rather than leave that behind.
    touched  <- list()
    complete <- FALSE
    on.exit(if (!complete) for (s in touched) .clear_site(s), add = TRUE)

    pk <- NULL
    for (s in sites) {
        touched[[length(touched) + 1L]] <- s
        pk <- keygen_round(s, crypto_context, pk)
    }
    joint_pk <- pk

    m <- ThresholdMaster(
        name           = name,
        crypto_context = crypto_context,
        joint_pubkey   = joint_pk,
        state          = new.env(parent = emptyenv()))
    m@state$workers <- sites

    ## Everyone encrypts under the joint key, so the public bundle goes
    ## back out to every site once the chain has completed. This is the
    ## setup message, and it goes through the generic so that a remote
    ## party can receive it at the far end -- see set_public_params().
    params <- public_params(m)
    for (s in sites) set_public_params(s, params)

    complete <- TRUE
    m
}

# ---- Generics -------------------------------------------------------------

#' Give a party the public parameters it will encrypt under
#'
#' The setup step of the protocol, and one of only two moments at
#' which anything passes between a coordinating party and a site — the
#' other being a round itself, which carries a query out and a
#' ciphertext back. A party receives its [PublicParams] once, here,
#' and from then on computes and encrypts with what it holds.
#'
#' Called for you by [set_workers()] and [make_threshold_master()].
#' You would call it directly only when writing a [RemoteSite] method.
#'
#' @section Why this is a generic:
#'
#' Setup is a *message*. For a co-located site, delivering it is an
#' assignment; for a remote one it is a network call that must
#' provision the far endpoint, and nothing in this process can do that
#' on the endpoint's behalf. Writing the parameters straight into a
#' remote proxy's `state` would leave the proxy looking configured
#' while the far end had never been told anything — a setup failure
#' that surfaces only much later, as a wrong answer. So the base
#' [RemoteSite] method **refuses**, and a subclass must implement the
#' provisioning it alone knows how to do. Missing remote setup fails
#' closed.
#'
#' What crosses is public in full: a crypto context and a public key.
#' There is no secret material in a [PublicParams] object and no
#' property for one to occupy.
#'
#' @section Reconfiguring:
#'
#' Receiving the same parameters again is harmless and allowed.
#' Receiving *different* ones is refused. A site that silently
#' switched keys would keep answering its first coordinator, in a key
#' that coordinator cannot read — under CKKS that surfaces as an
#' approximation-error abort, and under BFV or BGV as a plausible
#' wrong integer with nothing raised. Build a fresh site instead; they
#' are cheap.
#'
#' @param site a [Site], or a user-defined subclass of [RemoteSite].
#' @param ... method-specific arguments; the built-in method takes
#'   `params`, a [PublicParams] object.
#' @return the site, invisibly. Called for its side effect.
#' @seealso [site_params()] to read them back, [actor-encryption] for
#'   using them, [RemoteSite] for the full remote contract.
#' @export
set_public_params <- new_generic("set_public_params", "site")

#' The public parameters a party holds
#'
#' Reads back what [set_public_params()] delivered. Encryption needs
#' only this, so a party that has it is self-sufficient, and any other
#' party that will encrypt under the same key — a querier that is not
#' itself a site, say — can be handed a copy.
#'
#' Aborts if the site was never configured, rather than returning
#' `NULL` for a caller to encrypt with.
#'
#' @param site a [Site], or a user-defined subclass of [RemoteSite].
#' @param ... method-specific arguments; the built-in method takes
#'   none.
#' @return a [PublicParams] object.
#' @seealso [set_public_params()], [actor-encryption]
#' @export
site_params <- new_generic("site_params", "site")

#' Distribute the public key from the master to a downstream actor
#'
#' Part of the frozen Paillier-era legacy surface, used by
#' [round_robin_chain()]. The supported setup seam is
#' [set_public_params()], which carries the whole public bundle and
#' which a [RemoteSite] can implement.
#'
#' @param obj a [Site] (or legacy [NCParty]) to receive the key.
#' @param ... method-specific arguments. The methods take a single
#'   public key `pubkey` of the master's backend type.
#' @return the object `obj`, invisibly. Called for its side effect: the
#'   master's public key is stored in the receiving actor's `state`
#'   environment, and in the [NCParty] method is forwarded on to every
#'   [Site] that party manages, so each site can encrypt under it.
#' @export
set_public_key <- new_generic("set_public_key", "obj")

# ---- Methods --------------------------------------------------------------

method(set_public_key, Site)    <- function(obj, pubkey) {
    obj@state$pubkey <- pubkey; invisible(obj)
}

## Defined on Site, not LocalSite: a user's own Site subclass carrying
## protocol-specific state is still a co-located party, and stores its
## parameters the same way. RemoteSite gets an explicit refusal below,
## because storing them here would configure the proxy and not the
## endpoint.
method(set_public_params, Site) <- function(site, params) {
    if (!S7_inherits(params, PublicParams))
        cli_abort("{.arg params} must be a {.cls PublicParams} object.")
    held <- site@state$params
    if (!is.null(held) && !identical(params_tag(held), params_tag(params)))
        cli_abort(c("Site {.val {site@name}} already holds different public parameters.",
                    i = "It would go on answering its first coordinator in a key
                         that coordinator cannot read. Under CKKS that aborts at
                         decryption; under BFV or BGV it returns a plausible
                         wrong integer and raises nothing.",
                    i = "Build a fresh site with {.fun make_worker}."))
    site@state$params <- params
    invisible(site)
}

method(set_public_params, RemoteSite) <- function(site, params)
    cli_abort(c("{.cls RemoteSite} {.val {site@name}} has no {.fun set_public_params} method.",
                i = "Storing the parameters here would configure this proxy and
                     not the endpoint, which would then look wired while never
                     having been told anything.",
                i = "Implement {.fun set_public_params} for your subclass: send
                     {.arg params} to the far end and have it retain them. Only
                     public material travels -- a crypto context and a public
                     key."))

method(site_params, Site) <- function(site) {
    params <- site@state$params
    if (is.null(params))
        cli_abort(c("Site {.val {site@name}} has no public parameters.",
                    i = "A site is given them once, when it is wired with
                         {.fun set_workers} or taken through
                         {.fun make_threshold_master}. Do that first."))
    params
}

## Undo a site's participation in a key-generation ceremony that did
## not complete. Best effort, and deliberately local: for a RemoteSite
## this reaches the proxy only, which is why make_threshold_master()
## tells remote implementers to tolerate a repeated ceremony.
.clear_site <- function(site) {
    suppressWarnings(rm(list = c("sk", "cc", "params", "pubkey", "joint_tag"),
                        envir = site@state))
    invisible(site)
}

## Internal. The public setup a coordinator hands out once, at wiring
## time, and never again: the setup message it would send over a wire,
## and the *only* moment key material crosses between parties apart
## from a round's ciphertexts.
##
## Deliberately NOT exported. A site is autonomous once configured --
## it holds what it was given and encrypts with that. Calling this at
## encryption time would be a party reaching back for something it
## already has, which is the coupling the actor split exists to
## remove. The only callers are set_workers() and
## make_threshold_master(). A party that needs the bundle asks a site
## for it, through site_params(), which involves no coordinator.
#' @noRd
public_params <- new_generic("public_params", "master")

## Encryption under a bundle of public parameters. The generic is
## openfhe.R's, dispatching on (key, pt); here `key` is the bundle,
## which is what site_params() hands back, and `pt` the numeric value.
## The local() wrapper and the formal names are explained in generics.R.
local({
method(encrypt, list(OpenFHEParams, class_any)) <- function(key, pt) {
    params <- key
    value  <- pt
    encrypt(params@pk,
            .packed_codec(params@cc)$encode(value),
            cc = params@cc)
}
})

## A site encrypts with what it was given when it was wired, so it
## needs nothing but itself. Registered on Site rather than LocalSite:
## a RemoteSite subclass that accepted the parameters and computes in
## the current session -- which set_public_params() leaves open, and
## which it must, since some proxies do hold their own state -- reaches
## this the same way. One that never accepted them fails in
## site_params(), which says so in the terms of how a site is wired.
local({
method(encrypt, list(Site, class_any)) <- function(key, pt) {
    site  <- key
    value <- pt
    encrypt(site_params(site), value)
}
})

# ---- Backend-specific public_params / decrypt ----------------------------

method(public_params, CKKSMaster) <- function(master)
    OpenFHEParams(cc = master@crypto_context, pk = master@keypair@public)

## Dispatch on the master and `class_any`, not on the master and
## openfhe.R::Ciphertext. Narrowing the second argument to the class
## that works would turn a wrong kind of value into S7's "no method"
## message, when check_encrypted() below already explains what went
## wrong and why it matters -- a party that answers in cleartext hands
## over exactly the quantity the protocol hides.
##
## The generic is openfhe.R's decrypt(), dispatching on (ct, key) and
## accepting the key holder first as the C++ header does; in that
## order `ct` is the master and `key` the encrypted value. See
## generics.R for the formals and the local() wrapper.
local({
method(decrypt, list(CKKSMaster, class_any)) <- function(ct, key, len = 1L) {
    master     <- ct
    ciphertext <- key
    cc <- master@crypto_context
    check_encrypted(public_params(master), ciphertext, "decrypt")
    pt <- openfhe.R::decrypt(ciphertext, master@keypair@secret, cc = cc)
    openfhe.R::set_length(pt, as.integer(len))
    vals <- .packed_codec(cc)$decode(pt)
    if (len == 1L) vals[1] else vals[seq_len(len)]
}
})

method(public_params, ThresholdMaster) <- function(master)
    OpenFHEParams(cc = master@crypto_context, pk = master@joint_pubkey)

local({
method(decrypt, list(ThresholdMaster, class_any)) <- function(ct, key, len = 1L) {
    master     <- ct
    ciphertext <- key
    cc    <- master@crypto_context
    sites <- master@state$workers
    n     <- length(sites)
    if (n < 2)
        cli_abort("Threshold master is not wired to its sites.")
    check_encrypted(public_params(master), ciphertext, "decrypt")

    ## The master has no key material. It sends the ciphertext to each
    ## site and gets a partial decryption back; the share that produced
    ## the partial never leaves the site. In a real deployment each of
    ## these is a network round trip, which is why it goes through a
    ## generic a RemoteSite can implement.
    ##
    ## The lead/main distinction is a protocol role assigned by position
    ## in the key-generation chain, so the master tells each site which
    ## it is playing rather than the site deciding.
    partials <- vector("list", n)
    for (i in seq_len(n)) {
        partials[[i]] <- tryCatch(
            partial_decrypt(sites[[i]], ciphertext, lead = (i == 1L)),
            homomorpheR_site_unavailable = function(cnd)
                ## The condition carries the site's *name*, not the site.
                ## A LocalSite object drags its data and its key share
                ## into anything that logs or serializes the condition.
                cli_abort(c("Site {.val {sites[[i]]@name}} did not return a partial decryption.",
                            i = "Threshold decryption is n-of-n: one missing partial loses the whole result."),
                          class     = "homomorpheR_site_unavailable",
                          site_name = sites[[i]]@name,
                          parent    = cnd))
    }

    ## Fusion needs only the context, so the master can do it: it is a
    ## public operation on public data. n-of-n -- any subset of the
    ## partials would not suffice.
    ##
    ## A site that returns a well-formed but wrong partial corrupts the
    ## result here with no error raised anywhere; see the warning in
    ## make_threshold_master().
    pt <- do.call(openfhe.R::multiparty_decrypt_fusion, c(list(cc), partials))
    openfhe.R::set_length(pt, as.integer(len))
    vals <- .packed_codec(cc)$decode(pt)
    if (len == 1L) vals[1] else vals[seq_len(len)]
}
})

# ---- Helpers --------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

## Require a context built for a particular scheme. `caller` names the
## function to blame, without decoration -- cli styles it here.
.require_scheme <- function(cc, scheme, caller) {
    want <- as.integer(openfhe.R::SchemeId[[scheme]])
    got  <- as.integer(openfhe.R::get_scheme_id(cc))
    if (!identical(got, want)) {
        got_name <- names(openfhe.R::SchemeId)[
            match(got, as.integer(unlist(openfhe.R::SchemeId)))]
        cli_abort(c("{.fun {caller}} needs a {.val {scheme}} context.",
                    x = "The context supplied is {.val {got_name %||% got}}."))
    }
    invisible(cc)
}

## Is this value something the protocol can add to a running total,
## or decrypt?
##
## Without the class half of this check, a site that answers in
## cleartext has its plain number folded into the sum by ordinary
## scalar addition and the round returns the *correct* answer -- with
## that site's individual contribution having crossed the boundary the
## protocol exists to keep it behind. Nothing else in the pipeline
## notices.
##
## The tag half catches a value produced under some other key.
##
## A generic on the parameters rather than a plain function, because
## what counts as encrypted is a property of the backend: the frozen
## Paillier path has its own ciphertext type and no key tag.
#' @noRd
check_encrypted <- new_generic("check_encrypted", "params",
                               function(params, x, what, who = NULL) S7_dispatch())

## Shared message shaping, so both backends read alike. cli does the
## quoting -- building the fragment by hand needs non-ASCII quote
## characters, which R CMD check will not have in package code.
.from_site <- function(who)
    if (is.null(who)) "" else format_inline(" from site {.val {who}}")

.bad_encrypted <- function(x, what, who) {
    where <- .from_site(who)
    cli_abort(c("Cannot {what} a {.cls {class(x)[[1L]]}}{where}.",
                i = "An encrypted value is expected. A party that replies in
                     cleartext hands over exactly the individual quantity the
                     protocol keeps hidden, and the arithmetic would go through
                     without complaint."),
              class = "homomorpheR_bad_contribution")
}

method(check_encrypted, OpenFHEParams) <- function(params, x, what, who = NULL) {
    if (!S7_inherits(x, openfhe.R::Ciphertext)) .bad_encrypted(x, what, who)
    where <- .from_site(who)
    ## OpenFHE stamps keys and ciphertexts with a tag and preserves it
    ## through homomorphic operations, so this is an exact test on
    ## public data -- no decryption, no secret material.
    if (!identical(openfhe.R::get_key_tag(x), params_tag(params)))
        cli_abort(c("Encrypted value{where} was produced under a different key.",
                    i = "Its key tag does not match this protocol's public key.
                         Decrypting it would return noise -- and under BFV or
                         BGV that noise is a plausible integer, raising
                         nothing."),
                  class = "homomorpheR_key_mismatch")
    invisible(x)
}

## Reject a value an exact-integer scheme cannot carry, rather than
## rounding it away. `as.integer(0.9)` is 0, and BFV then reports a
## sum of zero for something that was never zero.
.as_exact_integer <- function(value, cc) {
    if (!is.numeric(value))
        cli_abort("An exact-integer context needs a numeric value, not {.cls {class(value)[[1L]]}}.")
    if (anyNA(value) || any(!is.finite(value)))
        cli_abort(c("Exact-integer schemes cannot represent NA, NaN, or an infinity.",
                    i = "A site that cannot evaluate a query returns {.val NA}
                         from its {.arg contribution_fn}, and {.fun contribute}
                         handles it before encryption."))
    bad <- value != round(value)
    if (any(bad))
        cli_abort(c("Value {.val {value[which(bad)[1L]]}} is not an integer.",
                    i = "BFV and BGV carry exact integers. Rounding it here
                         would report a total that is not the one asked for;
                         use a CKKS context for real-valued work."))
    if (any(abs(value) > .Machine$integer.max))
        cli_abort("Value {.val {value[which.max(abs(value))]}} is outside R's integer range.")
    t <- suppressWarnings(as.numeric(openfhe.R::get_plaintext_modulus(cc)))
    if (length(t) == 1L && is.finite(t) && t > 0 && any(abs(value) >= t / 2))
        cli_abort(c("Value {.val {value[which.max(abs(value))]}} does not fit the
                     plaintext modulus {.val {t}}.",
                    i = "It would wrap around and decrypt to a different number."))
    as.integer(value)
}

## Plaintext codec matching a crypto context's scheme.
##
## The protocol body (encrypt local summaries, add homomorphically,
## decrypt the total) is identical across schemes; only the
## plaintext encode/decode pair differs. CKKS carries reals and its
## decode returns approximate doubles; BFV and BGV carry exact
## integers. Detecting the scheme from the context keeps it the
## single source of truth -- a master never has to be told which
## scheme its context was built for.
##
## The three supported schemes are named explicitly. Treating
## "anything that is not CKKS" as an integer scheme meant a context
## this package has no codec for -- a future scheme, or one built by
## mistake -- would quietly encode as packed integers instead of
## saying so.
.packed_codec <- function(cc) {
    id <- as.integer(openfhe.R::get_scheme_id(cc))
    if (identical(id, as.integer(openfhe.R::SchemeId$CKKSRNS_SCHEME))) {
        list(
            encode = function(value) openfhe.R::make_ckks_packed_plaintext(cc, value),
            decode = function(pt)    openfhe.R::get_real_packed_value(pt)
        )
    } else if (identical(id, as.integer(openfhe.R::SchemeId$BFVRNS_SCHEME)) ||
               identical(id, as.integer(openfhe.R::SchemeId$BGVRNS_SCHEME))) {
        list(
            encode = function(value)
                openfhe.R::make_packed_plaintext(cc, .as_exact_integer(value, cc)),
            decode = function(pt) openfhe.R::get_packed_value(pt)
        )
    } else {
        cli_abort(c("No plaintext codec for this crypto context.",
                    i = "{.pkg homomorpheR} carries CKKS for real-valued work
                         and BFV or BGV for exact integers."))
    }
}

#' Construct a worker
#'
#' Builds the [Site] one party contributes to a multi-party protocol.
#' A `Site` becomes a *worker* once it has been wired and given its
#' public parameters; from that point it is autonomous, computing and
#' encrypting on its own.
#'
#' @inheritParams LocalSite
#' @return a [LocalSite].
#' @seealso [RemoteSite] for a site whose contribution is produced
#'   outside this \R session.
#' @export
make_worker <- function(name, data, contribution_fn) {
    LocalSite(name = name, data = data, contribution_fn = contribution_fn,
              state = new.env(parent = emptyenv()))
}

# ---- The site-facing protocol seam ----------------------------------------

#' Signal that a site could not be reached
#'
#' The condition a [RemoteSite] implementation raises when a transport,
#' authentication, or timeout failure stops it from answering. This is
#' **not** the same event as returning `NA`, which means the requested
#' `theta` is non-evaluable at a site that answered perfectly well; see
#' the contract in [RemoteSite]. Raising it aborts the round rather
#' than silently changing the set of sites being summed over.
#'
#' @param message what went wrong, for the caller.
#' @param site optionally, the [Site] that was unreachable; its name is
#'   added to the message by [master_aggregate()].
#' @param parent optionally, the underlying condition (an `httr2` error,
#'   say) to chain for debugging.
#' @return nothing — called for its side effect of signaling a
#'   condition of class `homomorpheR_site_unavailable`.
#'
#' @section What the re-raised condition carries:
#'
#' When [master_aggregate()] or [decrypt()] re-raise this, the
#' condition they signal carries a `site_name` field and **not** the
#' site object. A [LocalSite] would drag its data, and under threshold
#' keys its key share, into anything that logs or serializes the
#' condition. Catch on the class and read `cnd$site_name`.
#' @export
site_unavailable <- function(message, site = NULL, parent = NULL) {
    cli_abort(message, class = "homomorpheR_site_unavailable",
              site = site, parent = parent)
}

#' A site's encrypted contribution at a parameter value
#'
#' The single call the protocol runner makes on a site. Implementations
#' return the site's contribution **already encrypted**, using the
#' public parameters the site was given when it was configured, so an
#' individual site's cleartext contribution never reaches the
#' aggregator — that is the property the whole protocol rests on, and
#' [master_aggregate()] refuses a reply that is neither an encrypted
#' value under this protocol's key nor `NA`.
#'
#' The computation is entirely local. A site needs nothing at call time
#' beyond `theta`, its own data, and what it already holds.
#'
#' The only permitted plaintext reply is `NA`, signaling that `theta`
#' is non-evaluable at this site; CKKS has no representation for it, so
#' it cannot be encrypted. A site that cannot be *reached* must signal
#' [site_unavailable()] instead of returning `NA`.
#'
#' @param site a [LocalSite], or a user-defined subclass of
#'   [RemoteSite].
#' @param ... method-specific arguments; both built-in methods take
#'   `theta`, the parameter value being queried.
#' @return an encrypted contribution, of whatever type the site's own
#'   public parameters imply, or `NA` if `theta` is non-evaluable here.
#' @seealso [RemoteSite] for the contract a remote implementation must
#'   honor.
#' @export
contribute <- new_generic("contribute", "site")

method(contribute, LocalSite) <- function(site, theta) {
    value <- site@contribution_fn(site@data, theta)
    if (length(value) == 1 && is.na(value)) return(NA)
    encrypt(site, value)
}

## Unlike keygen_round, the built-in method above is on LocalSite rather
## than Site, so a RemoteSite subclass has nothing to fall through to and
## would fail on its own. It would fail with a bare S7 dispatch error,
## though, which says a method is missing without saying what the method
## owes anyone -- and contribute is the first one a subclass author
## reaches. The refusal below says it, matching set_public_params,
## keygen_round and partial_decrypt.
method(contribute, RemoteSite) <- function(site, theta)
    cli_abort(c("{.cls RemoteSite} {.val {site@name}} has no {.fun contribute} method.",
                i = "Evaluating the contribution here would need this site's data
                     in this process, and encrypting it here would mean the
                     cleartext value existed locally first -- which is the
                     disclosure the protocol exists to prevent.",
                i = "Implement {.fun contribute} for your subclass: send
                     {.arg theta} to the far end, have it evaluate and encrypt
                     with the parameters it retained, and return the encrypted
                     value. Reply {.code NA} only when {.arg theta} is
                     non-evaluable there, and signal {.fun site_unavailable} if
                     the endpoint cannot be reached."))

#' One site's step in the threshold key-generation chain
#'
#' The site derives its own secret share from its predecessor's
#' cumulative public key, **keeps the share**, and returns only the new
#' cumulative public key. The share is generated at the site and is
#' never a return value, so no other party can hold it.
#'
#' Called by [make_threshold_master()], once per site, in order. A
#' [RemoteSite] implementation must do the same thing at the far end:
#' receive a public key, generate and retain a share locally, send a
#' public key back. Nothing secret crosses the wire in either
#' direction.
#'
#' @param site a [LocalSite], or a user-defined subclass of [RemoteSite].
#' @param ... method-specific arguments; the built-in method takes
#'   `cc`, the crypto context, and `prev_pk`, the cumulative public key
#'   from the previous site in the chain (`NULL` for the lead site,
#'   which starts the chain with a fresh keypair).
#' @return the cumulative public key including this site's
#'   contribution. Never a secret key.
#' @seealso [make_threshold_master()], [partial_decrypt()].
#' @export
keygen_round <- new_generic("keygen_round", "site")

## Defined on Site, not LocalSite: any co-located site -- including a
## user's own Site subclass carrying protocol-specific state -- keeps
## its share the same way. A RemoteSite must not fall through to this,
## because generating the share here would generate it in the *master's*
## process, so RemoteSite gets an explicit refusal below.
method(keygen_round, Site) <- function(site, cc, prev_pk = NULL) {
    kp <- if (is.null(prev_pk))
              openfhe.R::key_gen(cc)                      # lead site
          else
              openfhe.R::multiparty_key_gen(cc, prev_pk)
    ## The share stays here. Only the public half is returned.
    site@state$cc <- cc
    site@state$sk <- kp@secret
    kp@public
}

method(keygen_round, RemoteSite) <- function(site, cc, prev_pk = NULL)
    cli_abort(c("{.cls RemoteSite} {.val {site@name}} has no {.fun keygen_round} method.",
                i = "The share must be generated at the far end and stay there.
                     Running the built-in method would generate it in this
                     process, which is the thing threshold keys exist to prevent.",
                i = "Implement {.fun keygen_round} for your subclass: send
                     {.arg prev_pk}, have the far end generate and retain its
                     share, and return the cumulative public key."))

#' One site's partial decryption of a ciphertext
#'
#' Under threshold keys no party can decrypt alone. A ciphertext is
#' sent to each site; each site applies **its own** secret share and
#' returns a partial decryption, and the partials are fused (see
#' [decrypt()]). The share never leaves the site, so no other
#' party ends up holding anything that would let it decrypt.
#'
#' Whether a site plays the `lead` role is fixed by its position in the
#' key-generation chain, so it arrives with the request; the site does
#' not choose and does not need to know who is asking.
#'
#' A site that cannot be reached must signal [site_unavailable()].
#' Because decryption is n-of-n, this loses the entire round rather
#' than one summand — see the availability note in [RemoteSite].
#'
#' @param site a [LocalSite], or a user-defined subclass of [RemoteSite].
#' @param ... method-specific arguments; the built-in method takes
#'   `ciphertext` and `lead`, a flag marking the first site in the
#'   chain.
#' @return a partial decryption, to be fused by [decrypt()].
#' @seealso [make_threshold_master()], [keygen_round()].
#' @export
partial_decrypt <- new_generic("partial_decrypt", "site")

method(partial_decrypt, Site) <- function(site, ciphertext, lead = FALSE) {
    if (is.null(site@state$sk))
        cli_abort(c("Site {.val {site@name}} holds no secret share.",
                    i = "Only sites that took part in {.fun make_threshold_master}
                         can produce a partial decryption."))

    ## The site checks for itself, with the joint key it was given at
    ## setup, that this ciphertext belongs to the protocol it joined.
    ## Applying its share to anything else is work it did not agree
    ## to, and the requester is not a party it has reason to trust.
    ## Nothing here is asked of anyone: the tag and the joint public
    ## key are both already in hand.
    held <- site@state$params
    if (!is.null(held))
        check_encrypted(held, ciphertext, "partially decrypt")

    cc <- site@state$cc
    if (lead)
        openfhe.R::multiparty_decrypt_lead(cc, site@state$sk, ciphertext)
    else
        openfhe.R::multiparty_decrypt_main(cc, site@state$sk, ciphertext)
}

method(partial_decrypt, RemoteSite) <- function(site, ciphertext, lead = FALSE)
    cli_abort(c("{.cls RemoteSite} {.val {site@name}} has no {.fun partial_decrypt} method.",
                i = "Send the ciphertext to the far end, have it apply its own
                     share, and return the partial. The share must not travel."))

#' Wire a master to a flat list of workers
#'
#' Stashes the workers in the master's state and publishes
#' its public parameters to each one. That bundle is public: it is the
#' setup broadcast a coordinator would send over the wire, and it is
#' all a site needs in order to encrypt. After this call,
#' [master_aggregate()] can drive an iteration of the protocol.
#'
#' Use this for the realistic master/worker (star) topology that
#' distcomp- and DataSHIELD-style federated analyses follow. For the
#' legacy Paillier round-robin idiom, use [round_robin_chain()] instead.
#'
#' A [ThresholdMaster] does **not** use this function: its joint public
#' key does not exist until key generation has run through every site,
#' so [make_threshold_master()] takes the sites and returns a master
#' already wired to them, in the order the chain fixed.
#'
#' @param master a [CKKSMaster], or the frozen legacy [PaillierMaster].
#' @param workers a list of worker [Site]s.
#' @return the master, invisibly.
#' @export
set_workers <- function(master, workers) {
    if (S7_inherits(master, ThresholdMaster))
        cli_abort(c("A {.cls ThresholdMaster} is already wired to its sites.",
                    i = "Pass the sites to {.fun make_threshold_master}; the
                         joint key is built from them, and the site order it
                         fixes is the order partial decryptions fuse in."))
    if (length(workers) < 1)
        cli_abort("Need at least one worker.")
    params <- public_params(master)

    ## Publish before recording, so that a worker which refuses the
    ## setup message -- a RemoteSite with no provisioning method --
    ## leaves the master unwired rather than half-wired.
    ##
    ## Public material only, and it goes through the generic: a
    ## RemoteSite has to be told at the far end, and writing into its
    ## proxy here would make it look configured when it is not.
    for (w in workers) set_public_params(w, params)
    master@state$workers <- workers
    invisible(master)
}

#' Run one round of the master/worker protocol
#'
#' Backend-agnostic: sites are reached through [contribute()] and the
#' total is recovered through the [decrypt()] generic, so the
#' same body works over [CKKSMaster] and [ThresholdMaster].
#'
#' The master broadcasts `theta` to each worker — and only `theta`;
#' each worker supplies its own `data`. Each worker returns
#' `contribution_fn(data, theta)` and the result is
#' encrypted under the master's public key. The master sums the
#' encrypted contributions homomorphically and decrypts the total.
#'
#' This is the topology that mirrors how distcomp, DataSHIELD, and
#' similar federated-analysis frameworks actually deploy: a flat
#' fan-out / fan-in. With a single-decrypter master, the master could
#' in principle decrypt individual contributions; the cryptographic
#' guarantee strengthens when paired with threshold key generation
#' (no single party holds the secret key).
#'
#' Each worker returns an *already encrypted* contribution (see
#' [contribute()]), so no individual site's cleartext value reaches the
#' master. Only the aggregate is decrypted.
#'
#' Two failure modes, deliberately distinct. If a worker returns `NA`,
#' `theta` is non-evaluable there and this function returns `NA_real_`,
#' which optimizers read as "back off and try elsewhere". If a worker
#' signals [site_unavailable()], it could not be reached at all; that
#' condition propagates and aborts the round, because continuing would
#' sum over a different set of sites and silently change the objective
#' between optimizer iterations.
#'
#' `NA` is the one value that travels in the clear, since CKKS cannot
#' represent it. A master that chooses `theta` adaptively therefore
#' learns which parameter values break which site — a residual side
#' channel that no amount of encryption here removes.
#'
#' @param master a [Master], wired to its workers — with
#'   [set_workers()] for a [CKKSMaster], or by
#'   [make_threshold_master()], which returns one already wired.
#' @param theta the current parameter value (passed through to each
#'   worker).
#' @return the aggregated value, or `NA_real_` if some site found
#'   `theta` non-evaluable.
#' @export
master_aggregate <- function(master, theta) {
    workers <- master@state$workers
    if (is.null(workers) || length(workers) == 0)
        cli_abort("Master has no workers; call {.fun set_workers} first.")

    params        <- public_params(master)
    contributions <- vector("list", length(workers))
    for (i in seq_along(workers)) {
        w <- workers[[i]]
        ## Each site encrypts its own contribution; the master never
        ## sees an individual cleartext value. `NA` is the documented
        ## exception -- it cannot be encrypted, so a non-evaluable
        ## `theta` is visible to the master by construction.
        ci <- tryCatch(
            contribute(w, theta),
            homomorpheR_site_unavailable = function(cnd)
                cli_abort("Site {.val {w@name}}: {conditionMessage(cnd)}",
                          class     = "homomorpheR_site_unavailable",
                          site_name = w@name,
                          parent    = cnd))
        if (is.atomic(ci) && length(ci) == 1L && is.na(ci)) return(NA_real_)

        ## Check what came back before adding it to the total. The
        ## contract in ?RemoteSite says a reply is a ciphertext or
        ## `NA`; an implementation that returns the plain number
        ## instead would otherwise be summed in silently and the round
        ## would report the right answer, having been handed the one
        ## quantity the protocol exists to hide.
        check_encrypted(params, ci, "aggregate", who = w@name)
        contributions[[i]] <- ci
    }
    decrypt(master, Reduce(`+`, contributions))
}

# ---- Print methods --------------------------------------------------------

method(print, Site) <- function(x, ...) {
    cat("<Site> ", x@name, "\n", sep = "")
    invisible(x)
}
method(print, Master) <- function(x, ...) {
    cat("<", sub("^homomorpheR::", "", class(x)[[1L]]), "> ", x@name,
        "\n", sep = "")
    invisible(x)
}

## Printing the bundle is how a reader sees that it is public in full:
## a scheme, a key fingerprint, and a line saying there is nothing
## else. The vignettes show this rather than describing it.
method(print, OpenFHEParams) <- function(x, ...) {
    id   <- as.integer(openfhe.R::get_scheme_id(x@cc))
    name <- names(openfhe.R::SchemeId)[
        match(id, as.integer(unlist(openfhe.R::SchemeId)))]
    cat("<OpenFHEParams> ", sub("RNS_SCHEME$", "", name %||% "unknown"), "\n",
        "  public key  ", params_tag(x), "\n",
        "  secret material: none\n", sep = "")
    invisible(x)
}
