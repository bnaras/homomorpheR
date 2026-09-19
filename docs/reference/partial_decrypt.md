# One site's partial decryption of a ciphertext

Under threshold keys no party can decrypt alone. A ciphertext is sent to
each site; each site applies **its own** secret share and returns a
partial decryption, and the partials are fused (see
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)).
The share never leaves the site, so no other party ends up holding
anything that would let it decrypt.

## Usage

``` r
partial_decrypt(site, ...)
```

## Arguments

- site:

  a
  [LocalSite](https://bnaras.github.io/homomorpheR/reference/LocalSite.md),
  or a user-defined subclass of
  [RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md).

- ...:

  method-specific arguments; the built-in method takes `ciphertext` and
  `lead`, a flag marking the first site in the chain.

## Value

a partial decryption, to be fused by
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md).

## Details

Whether a site plays the `lead` role is fixed by its position in the
key-generation chain, so it arrives with the request; the site does not
choose and does not need to know who is asking.

A site that cannot be reached must signal
[`site_unavailable()`](https://bnaras.github.io/homomorpheR/reference/site_unavailable.md).
Because decryption is n-of-n, this loses the entire round rather than
one summand — see the availability note in
[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md).

## See also

[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md),
[`keygen_round()`](https://bnaras.github.io/homomorpheR/reference/keygen_round.md).
