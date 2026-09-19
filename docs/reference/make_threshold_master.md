# Run threshold key generation across sites and construct the master

Drives the chained key-generation ceremony *through the sites* and
returns a master wired to them. The lead site generates a fresh keypair
`(pk_1, sk_1)`; each subsequent site `i` derives `(pk_{1..i}, sk_i)`
from its predecessor's cumulative public key. The final `pk_{1..n}` is
the joint public key under which everything is encrypted.

## Usage

``` r
make_threshold_master(name, crypto_context, sites)
```

## Arguments

- name:

  short identifier.

- crypto_context:

  an `openfhe.R` `CryptoContext` (CKKS, BFV, or BGV) *with* the
  `MULTIPARTY` feature enabled. Pass `features = c(Feature$MULTIPARTY)`
  to `fhe_context()`. The scheme is read back from the context, so the
  same master drives the protocol over real-valued (CKKS) or
  exact-integer (BFV/BGV) arithmetic without further configuration.

- sites:

  a list of at least two
  [Site](https://bnaras.github.io/homomorpheR/reference/Site.md)s, built
  with
  [`make_worker()`](https://bnaras.github.io/homomorpheR/reference/make_worker.md).
  The first is the lead site. Each ends up holding its own secret share
  and the joint public key.

## Value

a
[ThresholdMaster](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md),
wired to `sites`.

## Details

Each step runs at the site, through
[`keygen_round()`](https://bnaras.github.io/homomorpheR/reference/keygen_round.md):
the site keeps `sk_i` in its own state and returns only the cumulative
*public* key. No share is ever generated centrally, and none is returned
to this function, so the master cannot hold one even by accident. Only
public keys travel between parties, which is exactly what can be sent
over a wire to an untrusted peer.

Decryption is n-of-n:
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
asks each site for a partial decryption via
[`partial_decrypt()`](https://bnaras.github.io/homomorpheR/reference/partial_decrypt.md)
and fuses the results with `multiparty_decrypt_fusion`. There is no path
by which the master decrypts alone.

The returned master is already wired, so
[`set_workers()`](https://bnaras.github.io/homomorpheR/reference/set_workers.md)
is neither needed nor permitted afterwards — the site order fixed by the
key-generation chain is the order partial decryptions must be fused in,
and re-wiring would break it.

## What this does not defend against

The construction assumes participants follow the protocol
(honest-but-curious). A site that deviates can (a) return a well-formed
ciphertext that is not its honest contribution, (b) return a malformed
partial decryption, which corrupts the fused plaintext *silently* —
nothing in the scheme detects it — or (c) contribute a degenerate share
during key generation, weakening the threshold. The chain is sequential,
so each site also sees its predecessors' cumulative public key;
OpenFHE's multiparty key generation carries no proofs of knowledge or
commitments, so rogue-key behavior is not prevented here. Defending
against any of this needs verifiable decryption and committed key
generation, neither of which this package provides.

## See also

[`keygen_round()`](https://bnaras.github.io/homomorpheR/reference/keygen_round.md),
[`partial_decrypt()`](https://bnaras.github.io/homomorpheR/reference/partial_decrypt.md),
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md).
