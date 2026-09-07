# Distribute the public key from the master to a downstream actor

Distribute the public key from the master to a downstream actor

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
