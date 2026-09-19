# Give a party the public parameters it will encrypt under

The setup step of the protocol, and one of only two moments at which
anything passes between a coordinating party and a site — the other
being a round itself, which carries a query out and a ciphertext back. A
party receives its
[PublicParams](https://bnaras.github.io/homomorpheR/reference/PublicParams.md)
once, here, and from then on computes and encrypts with what it holds.

## Usage

``` r
set_public_params(site, ...)
```

## Arguments

- site:

  a [Site](https://bnaras.github.io/homomorpheR/reference/Site.md), or a
  user-defined subclass of
  [RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md).

- ...:

  method-specific arguments; the built-in method takes `params`, a
  [PublicParams](https://bnaras.github.io/homomorpheR/reference/PublicParams.md)
  object.

## Value

the site, invisibly. Called for its side effect.

## Details

Called for you by
[`set_workers()`](https://bnaras.github.io/homomorpheR/reference/set_workers.md)
and
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md).
You would call it directly only when writing a
[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md)
method.

## Why this is a generic

Setup is a *message*. For a co-located site, delivering it is an
assignment; for a remote one it is a network call that must provision
the far endpoint, and nothing in this process can do that on the
endpoint's behalf. Writing the parameters straight into a remote proxy's
`state` would leave the proxy looking configured while the far end had
never been told anything — a setup failure that surfaces only much
later, as a wrong answer. So the base
[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md)
method **refuses**, and a subclass must implement the provisioning it
alone knows how to do. Missing remote setup fails closed.

What crosses is public in full: a crypto context and a public key. There
is no secret material in a
[PublicParams](https://bnaras.github.io/homomorpheR/reference/PublicParams.md)
object and no property for one to occupy.

## Reconfiguring

Receiving the same parameters again is harmless and allowed. Receiving
*different* ones is refused. A site that silently switched keys would
keep answering its first coordinator, in a key that coordinator cannot
read — under CKKS that surfaces as an approximation-error abort, and
under BFV or BGV as a plausible wrong integer with nothing raised. Build
a fresh site instead; they are cheap.

## See also

[`site_params()`](https://bnaras.github.io/homomorpheR/reference/site_params.md)
to read them back,
[`encrypt_under()`](https://bnaras.github.io/homomorpheR/reference/encrypt_under.md)
to use them,
[RemoteSite](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md)
for the full remote contract.
