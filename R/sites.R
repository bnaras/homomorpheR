#' @importFrom S7 new_class new_generic new_object method method<- class_any class_character class_integer S7_object S7_inherits
#' @importFrom openfhe.R make_ckks_packed_plaintext get_real_packed_value make_packed_plaintext get_packed_value get_scheme_id
#' @importFrom openfhe.R multiparty_key_gen multiparty_decrypt_lead multiparty_decrypt_main multiparty_decrypt_fusion
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
## bookkeeping (next site in a chain, intermediate results, the master
## reference each site needs in order to signal a failed local
## computation, etc.). S7 properties themselves stay immutable.

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
#' @param name short identifier shown in printed output.
#' @param state an environment for mutable bookkeeping — the public
#'   key the site encrypts under, the capability [set_workers()]
#'   installs, and, on the frozen legacy path, the next link in the
#'   round-robin chain. Default: a fresh empty env.
#' @return nothing — this class is abstract, so calling it raises an
#'   error instead of returning an object. It is the common parent of
#'   [LocalSite] and [RemoteSite], and the dispatch target for
#'   [contribute()]. Construct a co-located site with [make_worker()].
#' @seealso [LocalSite], [RemoteSite], [contribute()]
#' @export
Site <- new_class(
    "Site",
    abstract = TRUE,
    package  = "homomorpheR",
    properties = list(
        name  = class_character,
        state = class_any
    )
)

#' A site whose data lives in this \R session
#'
#' The ordinary case: the records are here, and `contribution_fn` is
#' evaluated in-process. [contribute()] computes the contribution and
#' **encrypts it** with the public parameters the site was given when
#' it was wired, so what leaves is already a ciphertext.
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
        data            = class_any,
        contribution_fn = class_any
    )
)

#' A site whose contribution is produced outside this \R session
#'
#' Abstract. [homomorpheR] deliberately ships **no** implementation:
#' transports differ too much, and a crypto package has no business
#' carrying an HTTP client. Subclass it, add whatever properties your
#' transport needs, and register a [contribute()] method:
#'
#' ```
#' HttpSite <- S7::new_class("HttpSite", parent = RemoteSite,
#'                           properties = list(url = S7::class_character))
#' S7::method(contribute, HttpSite) <- function(site, theta) {
#'     ## ... call site@url with theta; the far end encrypts ...
#' }
#' ```
#'
#' @section The contract an implementation must honor:
#'
#' \describe{
#'   \item{Return a ciphertext, never a plain number.}{The remote end
#'     was given the public parameters when it was wired, so it
#'     encrypts *before* the value crosses the wire. A `RemoteSite`
#'     that returns cleartext hands the aggregator an individual
#'     per-site contribution, which is precisely what the protocol
#'     exists to prevent. `NA` is the one permitted plaintext reply,
#'     because CKKS has no representation for it; the aggregator
#'     consequently learns which `theta` a site could not evaluate,
#'     and that residual side channel is documented in
#'     [master_aggregate()].}
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
#' @inheritParams Site
#' @return nothing — this class is abstract. Subclass it as shown above.
#' @seealso [contribute()], [site_unavailable()], [LocalSite]
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
#' recovers the total through the [master_decrypt()] generic, which
#' dispatches on the concrete master class, so the same protocol runs
#' over any backend.
#'
#' A master never encrypts site data, and has no encryption entry point
#' at all. Each party encrypts its own values with [encrypt_under()],
#' using the public parameters it was handed when it was wired. The
#' asymmetry is deliberate and worth reading off the API: decryption is
#' privileged — it needs secret material, or the standing to convene
#' every site — while encryption needs only public material and is
#' available to anyone.
#'
#' @param name short identifier shown in printed output.
#' @param state an environment for mutable bookkeeping.
#' @return nothing — this class is abstract, so calling it raises an error
#'   instead of returning an object. It exists so that [master_decrypt()]
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
        state = class_any
    )
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
        crypto_context = class_any,
        keypair        = class_any
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
#' **The master holds no secret material.** Its properties are the
#' crypto context and the joint public key, both public; the shares
#' live at the sites that generated them and never travel. That is
#' what makes the n-of-n claim true of the objects and not merely of
#' the prose — see [partial_decrypt()] for the decryption seam.
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
        crypto_context = class_any,
        joint_pubkey   = class_any
    )
)

# ---- Constructors ---------------------------------------------------------

#' Construct a CKKS-backed master
#'
#' @inheritParams CKKSMaster
#' @return a [CKKSMaster].
#' @export
make_ckks_master <- function(name, crypto_context, keypair) {
    m <- CKKSMaster(name = name, crypto_context = crypto_context,
                    keypair = keypair,
                    state   = new.env(parent = emptyenv()))
    m@state$pubkey  <- keypair@public
    m@state$privkey <- keypair@secret
    m
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
#' Decryption is n-of-n: [master_decrypt()] asks each site for a
#' partial decryption via [partial_decrypt()] and fuses the results
#' with `multiparty_decrypt_fusion`. There is no path by which the
#' master decrypts alone.
#'
#' The returned master is already wired, so [set_workers()] is neither
#' needed nor permitted afterwards — the site order fixed by the
#' key-generation chain is the order partial decryptions must be
#' fused in, and re-wiring would break it.
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
#' @param sites a list of at least two [Site]s, built with
#'   [make_worker()]. The first is the lead site. Each ends up
#'   holding its own secret share and the joint public key.
#' @return a [ThresholdMaster], wired to `sites`.
#' @seealso [keygen_round()], [partial_decrypt()], [master_decrypt()].
#' @export
make_threshold_master <- function(name, crypto_context, sites) {
    if (!is.list(sites))
        cli_abort("{.arg sites} must be a list of {.cls Site} objects.")
    n <- length(sites)
    if (n < 2)
        cli_abort("Threshold key generation requires at least two sites.")

    ## The chain runs site to site. Each call returns a public key and
    ## nothing else; the share stays where it was generated.
    pk <- NULL
    for (s in sites) pk <- keygen_round(s, crypto_context, pk)
    joint_pk <- pk

    m <- ThresholdMaster(
        name           = name,
        crypto_context = crypto_context,
        joint_pubkey   = joint_pk,
        state          = new.env(parent = emptyenv()))
    m@state$pubkey  <- joint_pk
    m@state$workers <- sites

    ## Everyone encrypts under the joint key, so the public bundle goes
    ## back out to every site once the chain has completed.
    params <- public_params(m)
    for (s in sites) {
        set_public_key(s, joint_pk)
        s@state$params <- params
    }
    m
}

# ---- Generics -------------------------------------------------------------

#' Distribute the public key from the master to a downstream actor
#' @param obj a [Site] (or legacy [NCParty]) to receive the key.
#' @param ... method-specific arguments. The methods take a single
#'   public key `pubkey` of the master's backend type.
#' @return the object `obj`, invisibly. Called for its side effect: the
#'   master's public key is stored in the receiving actor's `state`
#'   environment, and in the [NCParty] method is forwarded on to every
#'   [Site] that party manages, so each site can encrypt under it.
#' @export
set_public_key <- new_generic("set_public_key", "obj")

#' Decrypt the master's protocol result back to a scalar real
#'
#' Dispatches on the master's class.
#'
#' @param master a [Master].
#' @param ... method-specific arguments. Both backends take a single
#'   `ciphertext` of the appropriate type.
#' @return a single numeric value.
#' @export
master_decrypt <- new_generic("master_decrypt", "master")

# ---- Methods --------------------------------------------------------------

method(set_public_key, Site)    <- function(obj, pubkey) {
    obj@state$pubkey <- pubkey; invisible(obj)
}
## Internal. The public setup handed out once, at wiring time, and
## never again: it is the setup message a coordinator would send over a
## wire, and the *only* moment key material crosses between parties
## apart from a round's ciphertexts.
##
## Deliberately NOT exported. A site is autonomous once constructed --
## it holds what it was given and encrypts with that. Calling this at
## encryption time would be a party reaching back for something it
## already has, which is the coupling the actor split exists to remove.
## The only callers are set_workers() and make_threshold_master().
##
## Contents are scheme-dependent (crypto context and public key for the
## OpenFHE backends; public key and denominator for frozen Paillier),
## so the bundle names its `scheme` and encrypt_under() reads it back.
## It never contains a secret key.
#' @noRd
public_params <- new_generic("public_params", "master")

#' Encrypt a value under the public parameters a party holds
#'
#' The one encryption entry point. It takes only public material, so a
#' party that was handed that material at setup encrypts entirely on
#' its own, with nothing to consult and no one to ask. A [Site] keeps
#' its copy in `state$params` from the moment it is wired, which is
#' what makes [contribute()] a purely local computation.
#'
#' For the `openfhe` schemes the plaintext encoding follows whatever
#' the context was built for (packed CKKS reals, or packed integers for
#' BFV and BGV), read back from the context itself.
#'
#' @param params the public parameters this party holds — for a [Site],
#'   `site@state$params`, installed when it was wired.
#' @param value a numeric vector.
#' @return an encrypted value of the backend's type.
#' @seealso [contribute()], which is how a [Site] uses this on its own
#'   data.
#' @export
encrypt_under <- function(params, value) {
    ## Two cryptosystems, two parameter sets. The `paillier` arm serves
    ## the frozen legacy backend only and will not grow a third case.
    switch(params$scheme,
           openfhe  = openfhe.R::encrypt(params$pk,
                                         .packed_codec(params$cc)$encode(value),
                                         cc = params$cc),
           paillier = encrypt_real(params$pk, value, params$den),
           cli_abort("Unknown scheme {.val {params$scheme}} in public parameters."))
}

# ---- Backend-specific public_params / master_decrypt ---------------------

method(public_params, CKKSMaster) <- function(master)
    list(scheme = "openfhe",
         cc     = master@crypto_context,
         pk     = master@keypair@public)

method(master_decrypt, CKKSMaster) <- function(master, ciphertext, len = 1L) {
    cc <- master@crypto_context
    pt <- openfhe.R::decrypt(ciphertext, master@keypair@secret, cc = cc)
    openfhe.R::set_length(pt, as.integer(len))
    vals <- .packed_codec(cc)$decode(pt)
    if (len == 1L) vals[1] else vals[seq_len(len)]
}

method(public_params, ThresholdMaster) <- function(master)
    list(scheme = "openfhe",
         cc     = master@crypto_context,
         pk     = master@joint_pubkey)

method(master_decrypt, ThresholdMaster) <- function(master, ciphertext, len = 1L) {
    cc    <- master@crypto_context
    sites <- master@state$workers
    n     <- length(sites)
    if (n < 2)
        cli_abort("Threshold master is not wired to its sites.")

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
                cli_abort(c("Site {.val {sites[[i]]@name}} did not return a partial decryption.",
                            i = "Threshold decryption is n-of-n: one missing partial loses the whole result."),
                          class  = "homomorpheR_site_unavailable",
                          site   = sites[[i]],
                          parent = cnd))
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

# ---- Helpers --------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

## Plaintext codec matching a crypto context's scheme.
##
## The protocol body (encrypt local summaries, add homomorphically,
## decrypt the total) is identical across schemes; only the
## plaintext encode/decode pair differs. CKKS carries reals and its
## decode returns approximate doubles; BFV and BGV carry exact
## integers. Detecting the scheme from the context keeps it the
## single source of truth -- a master never has to be told which
## scheme its context was built for.
.packed_codec <- function(cc) {
    if (openfhe.R::get_scheme_id(cc) == openfhe.R::SchemeId$CKKSRNS_SCHEME) {
        list(
            encode = function(value) openfhe.R::make_ckks_packed_plaintext(cc, value),
            decode = function(pt)    openfhe.R::get_real_packed_value(pt)
        )
    } else {
        list(
            encode = function(value) openfhe.R::make_packed_plaintext(cc, as.integer(value)),
            decode = function(pt)    openfhe.R::get_packed_value(pt)
        )
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
#' @return nothing — called for its side effect of signalling a
#'   condition of class `homomorpheR_site_unavailable`.
#' @export
site_unavailable <- function(message, site = NULL, parent = NULL) {
    cli_abort(message, class = "homomorpheR_site_unavailable",
              site = site, parent = parent)
}

#' A site's encrypted contribution at a parameter value
#'
#' The single call the protocol runner makes on a site. Implementations
#' return the site's contribution **already encrypted**, using the
#' public parameters the site was given when it was wired, so an
#' individual site's cleartext contribution never reaches the
#' aggregator — that is the property the whole protocol rests on.
#'
#' The computation is entirely local. A site needs nothing at call time
#' beyond `theta`, its own data, and what it already holds.
#'
#' The only permitted plaintext reply is `NA`, signalling that `theta`
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
    if (is.null(site@state$params))
        cli_abort(c("Site {.val {site@name}} has no public parameters.",
                    i = "A site is given them once, when it is wired with
                         {.fun set_workers} or taken through
                         {.fun make_threshold_master}. Do that first."))
    encrypt_under(site@state$params, value)
}

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
#' [master_decrypt()]). The share never leaves the site, so no other
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
#' @return a partial decryption, to be fused by [master_decrypt()].
#' @seealso [make_threshold_master()], [keygen_round()].
#' @export
partial_decrypt <- new_generic("partial_decrypt", "site")

method(partial_decrypt, Site) <- function(site, ciphertext, lead = FALSE) {
    if (is.null(site@state$sk))
        cli_abort(c("Site {.val {site@name}} holds no secret share.",
                    i = "Only sites that took part in {.fun make_threshold_master}
                         can produce a partial decryption."))
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
    master@state$workers <- workers
    params <- public_params(master)
    for (w in workers) {
        ## Public material only. The site encrypts its own value with
        ## this; nothing cleartext is ever passed to the master.
        set_public_key(w, master@state$pubkey)
        w@state$params <- params
    }
    invisible(master)
}

#' Run one round of the master/worker protocol
#'
#' Backend-agnostic: sites are reached through [contribute()] and the
#' total is recovered through the [master_decrypt()] generic, so the
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
#' @param master a [Master], wired to workers via [set_workers()].
#' @param theta the current parameter value (passed through to each
#'   worker).
#' @return the aggregated value, or `NA_real_` if some site found
#'   `theta` non-evaluable.
#' @export
master_aggregate <- function(master, theta) {
    workers <- master@state$workers
    if (is.null(workers) || length(workers) == 0)
        cli_abort("Master has no workers; call {.fun set_workers} first.")

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
                          class  = "homomorpheR_site_unavailable",
                          site   = w,
                          parent = cnd))
        if (is.atomic(ci) && length(ci) == 1L && is.na(ci)) return(NA_real_)
        contributions[[i]] <- ci
    }
    master_decrypt(master, Reduce(`+`, contributions))
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
