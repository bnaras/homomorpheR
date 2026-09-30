# Encrypt and decrypt with protocol actors

homomorpheR adds methods to `openfhe.R`'s
[`encrypt()`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)
and
[`decrypt()`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)
generics, so the same two verbs serve both layers of the stack and the
class of the first argument selects which layer answers. This page
describes the actor-level methods; the key-level ones are documented in
`openfhe.R`.

## Encrypting

Encryption needs only public material, so a party handed that material
at setup encrypts entirely on its own, with nothing to consult and no
one to ask — which is what makes
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
a purely local computation. Methods are registered on whatever holds the
public material:

- a [Site](https://bnaras.github.io/homomorpheR/reference/Site.md)
  encrypts with the parameters it was given when it was wired, so
  `encrypt(site, value)` needs nothing besides the site itself;

- [OpenFHEParams](https://bnaras.github.io/homomorpheR/reference/OpenFHEParams.md)
  encrypts under a bundle held directly, which is what a party reads
  back from a site with
  [`site_params()`](https://bnaras.github.io/homomorpheR/reference/site_params.md);

- the frozen
  [PaillierParams](https://bnaras.github.io/homomorpheR/reference/PaillierParams.md)
  and
  [PaillierPublicKey](https://bnaras.github.io/homomorpheR/reference/PaillierPublicKey.md)
  encrypt under the Paillier scheme.

For the `openfhe` backends the encoding follows whatever the context was
built for, read back from the context itself: packed reals under CKKS,
packed integers under BFV and BGV. The exact schemes reject a value they
cannot represent rather than round it; see
[OpenFHEParams](https://bnaras.github.io/homomorpheR/reference/OpenFHEParams.md).

There is deliberately no method on
[Master](https://bnaras.github.io/homomorpheR/reference/Master.md), and
`public_params()` is deliberately not exported. An encryption entry
point taking a master would advertise a privilege that does not exist,
and would invite site-side code to reach back to a coordinator for
something it was already given. A site is autonomous once configured.

## Decrypting

Decryption is the asymmetric half of the pair, and that asymmetry is the
point: it takes either secret material or the standing to convene every
site, while encryption takes neither. Methods are registered on the
decrypting party:

- [CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md)
  decrypts with the secret key it holds;

- [ThresholdMaster](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md)
  holds no key material at all and recovers a value by asking each site
  for a partial decryption through
  [`partial_decrypt()`](https://bnaras.github.io/homomorpheR/reference/partial_decrypt.md)
  and fusing the results, so no party — the master included — can
  decrypt alone;

- the frozen
  [PaillierMaster](https://bnaras.github.io/homomorpheR/reference/PaillierMaster.md)
  and
  [PaillierPrivateKey](https://bnaras.github.io/homomorpheR/reference/PaillierPrivateKey.md)
  decrypt under the Paillier scheme.

The master methods take `len`, the number of packed slots to return,
defaulting to `1`.

## Return semantics

The methods return deliberately different types, because the encodings
differ:

- [CKKSMaster](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md)
  and
  [ThresholdMaster](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md)
  return a `numeric` of length `len`, decoded for whatever scheme the
  context was built for.

- [PaillierMaster](https://bnaras.github.io/homomorpheR/reference/PaillierMaster.md)
  returns the scalar real its protocol accumulated.

- a
  [PaillierCiphertext](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md)
  under a
  [PaillierPrivateKey](https://bnaras.github.io/homomorpheR/reference/PaillierPrivateKey.md)
  gives a [gmp::bigz](https://rdrr.io/pkg/gmp/man/biginteger.html) in
  `[0, n)`. This preserves raw mod-`n` arithmetic; callers wanting
  signed integers should re-center themselves (`if (m > n/2) m - n`).

- a
  [PaillierEncryptedReal](https://bnaras.github.io/homomorpheR/reference/PaillierEncryptedReal.md)
  gives a `numeric` in `(-n/2, n/2)`. The method re-centers the raw
  mod-`n` residues so that negative real numbers and running totals that
  cross zero round-trip correctly. See
  [PaillierEncryptedReal](https://bnaras.github.io/homomorpheR/reference/PaillierEncryptedReal.md)
  for the full convention.

## Argument names

The generics belong to `openfhe.R`, and their argument names follow the
OpenFHE C++ signatures: `encrypt(key, pt, ...)` for
`Encrypt(publicKey, plaintext)`, and `decrypt(ct, key, ...)` for
`Decrypt(ciphertext, privateKey)`. S7 requires every method to use the
generic's names for the arguments it dispatches on, so those are the
names here too, and in a call on a protocol actor they read by position:
in `encrypt(site, value)`, `key` is the site and `pt` the value; in
`decrypt(master, ct, len)`, `ct` is the master and `key` the encrypted
value. Every call in this package and its vignettes is positional, so
the names are never written out.

## See also

[`openfhe.R::encrypt()`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)
and
[`openfhe.R::decrypt()`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)
for the key-level methods;
[`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md),
which is how a
[Site](https://bnaras.github.io/homomorpheR/reference/Site.md) encrypts
its own data during a round;
[`partial_decrypt()`](https://bnaras.github.io/homomorpheR/reference/partial_decrypt.md)
for the site side of a threshold decryption.
