## The shared encrypt()/decrypt() surface.
##
## homomorpheR registers its methods on openfhe.R's encrypt() and
## decrypt() generics and re-exports them, so one generic per verb
## serves the key level and the actor level, and the class of the
## first argument selects the layer. These assertions pin that: that
## the exported generics ARE openfhe.R's and not copies, that a site
## is a sufficient argument for encryption, that a master is one for
## decryption, that both layers stay reachable through the bare names,
## and that the two absences in the design are still absent.

library(homomorpheR)

cc <- openfhe.R::fhe_context("CKKS",
                             multiplicative_depth = 1L,
                             scaling_mod_size     = 50L,
                             batch_size           = 8L)
keys <- openfhe.R::key_gen(cc)

## ---- One generic, not two -------------------------------------------------
## If a namespace-level copy ever creeps back in -- a top-level
## `method<-` on these generics outside local() -- the exported object
## becomes a lazy-load snapshot with its own method table, and these
## two lines are what catch it. See R/generics.R.
expect_identical(homomorpheR::encrypt, openfhe.R::encrypt)
expect_identical(homomorpheR::decrypt, openfhe.R::decrypt)

## openfhe.R's own key-level methods stay reachable by the bare name
## with homomorpheR attached, in the header's argument order.
pt0 <- openfhe.R::make_ckks_packed_plaintext(cc, c(1, 2))
ct0 <- encrypt(keys@public, pt0, cc = cc)
expect_true(S7::S7_inherits(ct0, openfhe.R::Ciphertext))
expect_true(S7::S7_inherits(decrypt(ct0, keys@secret, cc = cc), openfhe.R::Plaintext))

nll <- function(d, lambda) -sum(stats::dpois(d, lambda, log = TRUE))

## ---- A site is a sufficient argument for encryption ---------------------
## The point of the rename: a party holds its parameters, so it does
## not fetch them and hand them back to the function that asked.
m <- make_ckks_master(name = "M", crypto_context = cc, keypair = keys)
w1 <- make_worker("S1", c(2, 3), nll)
w2 <- make_worker("S2", c(4, 5), nll)
set_workers(m, list(w1, w2))

expect_true(S7::S7_inherits(encrypt(w1, 1.5), openfhe.R::Ciphertext))

## Encrypting through the site and through the bundle it holds are the
## same operation, so the two must agree after decryption.
via_site   <- decrypt(m, encrypt(w1, 2.25))
via_bundle <- decrypt(m, encrypt(site_params(w1), 2.25))
expect_true(abs(via_site - 2.25) < 1e-6)
expect_true(abs(via_site - via_bundle) < 1e-6)

## ---- A site that was never wired says so in a site's own terms ----------
lone <- make_worker("Lone", 1, nll)
expect_error(encrypt(lone, 1), pattern = "no public parameters")

## ---- Decryption dispatches on the master --------------------------------
expect_true(abs(decrypt(m, encrypt(w1, 7)) - 7) < 1e-6)

## `len` reaches the method through the generic's dots.
packed <- decrypt(m, encrypt(w1, c(1, 2, 3)), len = 3L)
expect_equal(length(packed), 3L)
expect_true(max(abs(packed - c(1, 2, 3))) < 1e-6)

## ---- A wrong kind of value is explained, not left to dispatch -----------
## The openfhe methods dispatch on `class_any` for the encrypted value
## precisely so this message comes from check_encrypted() rather than
## from S7 reporting that no method was found.
expect_error(decrypt(m, 5), class = "homomorpheR_bad_contribution")

## ---- The two layers do not shadow each other ----------------------------
## Same two generics, unrelated classes: the frozen Paillier methods
## still resolve with the openfhe ones registered alongside them.
pk <- paillier_keypair(512)
pub <- pk@pubkey
priv <- get_private_key(pk)
expect_equal(decrypt(priv, encrypt(pub, gmp::as.bigz(42L))), gmp::as.bigz(42L))
expect_true(abs(decrypt(priv, encrypt_real(pub, -3.5, gmp::as.bigz(2)^64)) + 3.5) < 1e-9)

## A Paillier master answers the same decrypt() generic.
pm <- make_master("PM", pk)
pw <- make_worker("PS", c(2, 3), nll)
set_workers(pm, list(pw))
expect_true(abs(decrypt(pm, contribute(pw, 3.5)) - nll(c(2, 3), 3.5)) < 1e-6)

## ---- The deliberate absences stay absent --------------------------------
## No encryption entry point takes a master: encryption needs only
## public material, and a method here would invite site-side code to
## hold a coordinator it has no use for.
expect_error(encrypt(m, 1))
expect_error(encrypt(pm, 1))

## `encrypt_under()` and `master_decrypt()` are gone, not deprecated.
expect_false(exists("encrypt_under",  where = asNamespace("homomorpheR")))
expect_false(exists("master_decrypt", where = asNamespace("homomorpheR")))
