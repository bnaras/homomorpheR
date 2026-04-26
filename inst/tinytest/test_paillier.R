## Integer Paillier round-trip and homomorphic operators.

library(homomorpheR)

keys <- paillier_keypair(512)
priv <- get_private_key(keys)

## Round-trip
expect_equal(decrypt(priv, encrypt(keys@pubkey, gmp::as.bigz(0))),
             gmp::as.bigz(0))
expect_equal(decrypt(priv, encrypt(keys@pubkey, gmp::as.bigz(1))),
             gmp::as.bigz(1))
expect_equal(decrypt(priv, encrypt(keys@pubkey, gmp::as.bigz(123456))),
             gmp::as.bigz(123456))

## Random round-trip
m <- random.bigz(nBits = 256)
expect_equal(decrypt(priv, encrypt(keys@pubkey, m)), m)

## Operator dispatch on PaillierCiphertext.
ct_a <- encrypt(keys@pubkey, gmp::as.bigz(42))
ct_b <- encrypt(keys@pubkey, gmp::as.bigz(8))
expect_equal(decrypt(priv, ct_a + ct_b), gmp::as.bigz(50))
expect_equal(decrypt(priv, ct_a - ct_b), gmp::as.bigz(34))
expect_equal(decrypt(priv, ct_a * gmp::as.bigz(3)), gmp::as.bigz(126))
expect_equal(decrypt(priv, ct_a + gmp::as.bigz(5)), gmp::as.bigz(47))

## Unary minus + reconstruction (decrypt then add gives n - value, then mod n
## gives back zero — verify by adding back to original).
expect_equal(decrypt(priv, ct_a + (-ct_a) + ct_b), gmp::as.bigz(8))

## Mod-n semantics: integer decrypt does NOT center; large negative-style
## values (encrypted as negative integer) come back as their mod-n
## representation in [0, n).
n  <- keys@pubkey@n
ct_neg <- encrypt(keys@pubkey, gmp::as.bigz(-7))
res    <- decrypt(priv, ct_neg)
expect_true(res >= 0 && res < n)
## (n - 7) mod n
expect_equal(res, gmp::sub.bigz(n, gmp::as.bigz(7)))
