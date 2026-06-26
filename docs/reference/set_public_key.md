# Distribute the public key from the master to a downstream actor

Distribute the public key from the master to a downstream actor

## Usage

``` r
set_public_key(obj, ...)
```

## Arguments

- obj:

  an
  [NCParty](https://bnaras.github.io/homomorpheR/reference/NCParty.md)
  or [Site](https://bnaras.github.io/homomorpheR/reference/Site.md) to
  receive the key.

- ...:

  method-specific arguments. The Site/NCParty methods take a single
  [PaillierPublicKey](https://bnaras.github.io/homomorpheR/reference/PaillierPublicKey.md)
  `pubkey`.
