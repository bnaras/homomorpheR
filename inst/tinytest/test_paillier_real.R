## PaillierEncryptedReal round-trip and signed-arithmetic semantics.
##
## The real-valued wrapper re-centers raw mod-n values into the
## interval (-n/2, n/2) on decryption so that negative reals and
## running totals that cross zero round-trip correctly. These tests
## pin that semantic so a future refactor cannot silently revert it.

library(homomorpheR)

keys <- paillier_keypair(512)
priv <- get_private_key(keys)
den  <- gmp::as.bigq(2)^256

## ---- Round-trip on positive values --------------------------------------
expect_equal(decrypt(priv, encrypt_real(keys@pubkey, 0,        den)), 0)
expect_equal(decrypt(priv, encrypt_real(keys@pubkey, 1,        den)), 1)
expect_equal(decrypt(priv, encrypt_real(keys@pubkey, 3.14159,  den)), 3.14159)

## ---- Round-trip on negative values --------------------------------------
expect_equal(decrypt(priv, encrypt_real(keys@pubkey, -1,         den)), -1)
expect_equal(decrypt(priv, encrypt_real(keys@pubkey, -3.14159,   den)), -3.14159)
expect_equal(decrypt(priv, encrypt_real(keys@pubkey, -0.001,     den)), -0.001)

## Larger negatives — the encrypted plaintext is n - |x|, which lives in
## the upper half of [0, n); the decrypt method must re-center it.
big_neg <- -123456.789
expect_equal(
    decrypt(priv, encrypt_real(keys@pubkey, big_neg, den)),
    big_neg)

## ---- Operator dispatch on PaillierEncryptedReal --------------------------
a <- encrypt_real(keys@pubkey, 3.14159, den)
b <- encrypt_real(keys@pubkey, 1.0,     den)
expect_equal(decrypt(priv, a + b), 4.14159)
expect_equal(decrypt(priv, a - b), 2.14159)

## ---- Running sum that crosses zero --------------------------------------
## The bug we are guarding against: previously a sum that transiently
## or terminally went negative wrapped to a ~10^150 value because the
## raw mod-n decrypt was used.
contributions <- c(10, -3, -8, 2)   # running sums: 10, 7, -1, 1
running <- encrypt_real(keys@pubkey, 0, den)
for (x in contributions) running <- running + encrypt_real(keys@pubkey, x, den)
expect_equal(decrypt(priv, running), sum(contributions))

## And one that ends negative.
expect_equal(
    decrypt(priv,
            encrypt_real(keys@pubkey, 5,  den) -
            encrypt_real(keys@pubkey, 12, den)),
    -7)

## ---- Mixed PaillierEncryptedReal + cleartext via Ops handler ------------
expect_equal(decrypt(priv, a + 1.0),    4.14159)
expect_equal(decrypt(priv, 1.0 + a),    4.14159)
expect_equal(decrypt(priv, a - 0.14159), 3.0)
