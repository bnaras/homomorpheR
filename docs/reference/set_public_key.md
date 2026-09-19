# Distribute the public key from the master to a downstream actor

Part of the frozen Paillier-era legacy surface, used by
[`round_robin_chain()`](https://bnaras.github.io/homomorpheR/reference/round_robin_chain.md).
The supported setup seam is
[`set_public_params()`](https://bnaras.github.io/homomorpheR/reference/set_public_params.md),
which carries the whole public bundle and which a
[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md)
can implement.

## Usage

``` r
set_public_key(obj, ...)
```

## Arguments

- obj:

  a [Site](https://bnaras.github.io/homomorpheR/reference/Site.md) (or
  legacy
  [NCParty](https://bnaras.github.io/homomorpheR/reference/NCParty.md))
  to receive the key.

- ...:

  method-specific arguments. The methods take a single public key
  `pubkey` of the master's backend type.

## Value

the object `obj`, invisibly. Called for its side effect: the master's
public key is stored in the receiving actor's `state` environment, and
in the
[NCParty](https://bnaras.github.io/homomorpheR/reference/NCParty.md)
method is forwarded on to every
[Site](https://bnaras.github.io/homomorpheR/reference/Site.md) that
party manages, so each site can encrypt under it.
