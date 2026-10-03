# One site's step in the threshold key-generation chain

The site derives its own secret share from its predecessor's cumulative
public key, **keeps the share**, and returns only the new cumulative
public key. The share is generated at the site and is never a return
value, so no other party can hold it.

## Usage

``` r
keygen_round(site, ...)
```

## Arguments

- site:

  a
  [LocalSite](https://bnaras.github.io/homomorpheR/reference/LocalSite.md),
  or a user-defined subclass of
  [RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md).

- ...:

  method-specific arguments; the built-in method takes `cc`, the crypto
  context, and `prev_pk`, the cumulative public key from the previous
  site in the chain (`NULL` for the lead site, which starts the chain
  with a fresh keypair).

## Value

the cumulative public key including this site's contribution. Never a
secret key.

## Details

Called by
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md),
once per site, in order. A
[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md)
implementation must do the same thing at the far end: receive a public
key, generate and retain a share locally, send a public key back.
Nothing secret crosses the wire in either direction.

## See also

[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md),
[`partial_decrypt()`](https://bnaras.github.io/homomorpheR/reference/partial_decrypt.md).
