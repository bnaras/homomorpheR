## Version 1.3.0

- Migrated from R6 to S7 throughout. The `R6` dependency is removed.
- New `PaillierCiphertext` class wraps encrypted values together with
  the public key they were encrypted under. R's arithmetic operators
  (`+`, `-`, `*` against a cleartext scalar) now dispatch directly on
  encrypted values via an S3 `Ops` group handler, so computations on
  encrypted data read like ordinary R arithmetic.
- API renames:
  - `PaillierKeyPair$new(bits)` → `paillier_keypair(modulus_bits)`
  - `pubkey$encrypt(m)` → `encrypt(pubkey, m)` (S7 generic)
  - `privkey$decrypt(ct)` → `decrypt(privkey, ct)` (S7 generic)
  - `pubkey$add(a, b)` / `pubkey$sub(a, b)` → `a + b` / `a - b`
  - `pubkey$mult(ct, k)` → `ct * k` (cleartext scalar)
  - `keys$getPrivateKey()` → `get_private_key(keys)`
  - `privkey$getLambda()` → `get_lambda(privkey)`
- Field access: `keys$pubkey` → `keys@pubkey`, etc.
- Minimum R version bumped to 4.3.0 (required for S7 `@` and
  `chooseOpsMethod`).

## Version 0.3

- Added MPC vignettes

## Versions up to 0.2.x

- Initial versions
