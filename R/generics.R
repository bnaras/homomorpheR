## R-SPECIFIC: homomorpheR extends openfhe.R's encrypt()/decrypt().
##
## There is one `encrypt()` generic and one `decrypt()` generic in this
## stack, and openfhe.R owns both. homomorpheR imports them, registers
## its protocol-actor methods on them, and re-exports them, so a user
## who attaches homomorpheR gets the same two objects openfhe.R exports
## and one method table per generic holding both layers' methods. The
## class of the first argument selects the layer.
##
## homomorpheR used to define its own generics of these two names
## (2026-04-25, the S7 rewrite -- the same commit that added the
## openfhe.R import). Two S7 generics of one name are two disjoint
## method tables, and whichever package is attached last is the one a
## bare call reaches; `library()` on an already-attached package does
## not reorder the search path, so in a session that attaches both --
## `tools::buildVignettes` runs every vignette in one -- the first
## attach wins for good. Registering on the upstream generic is the
## ordinary R pattern (nobody defines their own print(); they register
## print.foo on base's), and S7 provides for it explicitly: see
## ?S7::methods_register and ?S7::new_external_generic. .onLoad calls
## S7::methods_register().
##
## Every `method<-` on these generics, in every file of this package,
## sits inside local(). The reason is measured, not stylistic: `method<-`
## is a replacement function, so `method(encrypt, sig) <- f` at top
## level expands to `encrypt <- \`method<-\`(encrypt, sig, f)` and
## creates a namespace-level binding named `encrypt` that shadows the
## import. Environments are copied when a namespace is lazy-loaded, so
## that binding comes back as a *snapshot* of the generic with its own
## copy of the method table -- not identical to openfhe.R's, masking it
## with a warning, and blind to any method registered later. Inside
## local() the replacement assignment lands in a throwaway frame, the
## namespace keeps no binding, and `export(encrypt)` resolves to the
## imported object itself. See notes/discoveries/D031.
##
## The dispatch formals are openfhe.R's: `key`, `pt` for encrypt and
## `ct`, `key` for decrypt. S7 requires every method to name its
## dispatch arguments exactly as the generic does, and the owner of the
## generic chose them to follow the OpenFHE C++ signatures --
## Encrypt(publicKey, plaintext), Decrypt(ciphertext, privateKey) --
## which openfhe-python binds under the same names. So in a method on a
## Site, `key` is the site; in a method on a Master, `ct` is the master.
## Each body binds a local alias on its first line and says so.

#' @importFrom openfhe.R encrypt decrypt
NULL

#' @export
openfhe.R::encrypt

#' @export
openfhe.R::decrypt

#' Encrypt and decrypt with protocol actors
#'
#' homomorpheR adds methods to `openfhe.R`'s [encrypt()] and
#' [decrypt()] generics, so the same two verbs serve both layers of the
#' stack and the class of the first argument selects which layer
#' answers. This page describes the actor-level methods; the key-level
#' ones are documented in `openfhe.R`.
#'
#' @section Encrypting:
#'
#' Encryption needs only public material, so a party handed that
#' material at setup encrypts entirely on its own, with nothing to
#' consult and no one to ask — which is what makes [contribute()] a
#' purely local computation. Methods are registered on whatever holds
#' the public material:
#'
#' - a [Site] encrypts with the parameters it was given when it was
#'   wired, so `encrypt(site, value)` needs nothing besides the site
#'   itself;
#' - [OpenFHEParams] encrypts under a bundle held directly, which is
#'   what a party reads back from a site with [site_params()];
#' - the frozen [PaillierParams] and [PaillierPublicKey] encrypt under
#'   the Paillier scheme.
#'
#' For the `openfhe` backends the encoding follows whatever the
#' context was built for, read back from the context itself: packed
#' reals under CKKS, packed integers under BFV and BGV. The exact
#' schemes reject a value they cannot represent rather than round it;
#' see [OpenFHEParams].
#'
#' There is deliberately no method on [Master], and `public_params()`
#' is deliberately not exported. An encryption entry point taking a
#' master would advertise a privilege that does not exist, and would
#' invite site-side code to reach back to a coordinator for something
#' it was already given. A site is autonomous once configured.
#'
#' @section Decrypting:
#'
#' Decryption is the asymmetric half of the pair, and that asymmetry is
#' the point: it takes either secret material or the standing to
#' convene every site, while encryption takes neither. Methods are
#' registered on the decrypting party:
#'
#' - [CKKSMaster] decrypts with the secret key it holds;
#' - [ThresholdMaster] holds no key material at all and recovers a
#'   value by asking each site for a partial decryption through
#'   [partial_decrypt()] and fusing the results, so no party — the
#'   master included — can decrypt alone;
#' - the frozen [PaillierMaster] and [PaillierPrivateKey] decrypt
#'   under the Paillier scheme.
#'
#' The master methods take `len`, the number of packed slots to
#' return, defaulting to `1`.
#'
#' @section Return semantics:
#'
#' The methods return deliberately different types, because the
#' encodings differ:
#'
#' - [CKKSMaster] and [ThresholdMaster] return a `numeric` of length
#'   `len`, decoded for whatever scheme the context was built for.
#' - [PaillierMaster] returns the scalar real its protocol
#'   accumulated.
#' - a [PaillierCiphertext] under a [PaillierPrivateKey] gives a
#'   [gmp::bigz] in `[0, n)`. This preserves raw mod-`n` arithmetic;
#'   callers wanting signed integers should re-center themselves
#'   (`if (m > n/2) m - n`).
#' - a [PaillierEncryptedReal] gives a `numeric` in `(-n/2, n/2)`. The
#'   method re-centers the raw mod-`n` residues so that negative real
#'   numbers and running totals that cross zero round-trip correctly.
#'   See [PaillierEncryptedReal] for the full convention.
#'
#' @section Argument names:
#'
#' The generics belong to `openfhe.R`, and their argument names follow
#' the OpenFHE C++ signatures: `encrypt(key, pt, ...)` for
#' `Encrypt(publicKey, plaintext)`, and `decrypt(ct, key, ...)` for
#' `Decrypt(ciphertext, privateKey)`. S7 requires every method to use
#' the generic's names for the arguments it dispatches on, so those
#' are the names here too, and in a call on a protocol actor they read
#' by position: in `encrypt(site, value)`, `key` is the site and `pt`
#' the value; in `decrypt(master, ct, len)`, `ct` is the master and
#' `key` the encrypted value. Every call in this package and its
#' vignettes is positional, so the names are never written out.
#'
#' @name actor-encryption
#' @aliases actor-decryption
#' @seealso [openfhe.R::encrypt()] and [openfhe.R::decrypt()] for the
#'   key-level methods; [contribute()], which is how a [Site] encrypts
#'   its own data during a round; [partial_decrypt()] for the site side
#'   of a threshold decryption.
NULL
