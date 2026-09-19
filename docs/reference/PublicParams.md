# The public parameters a party encrypts under

Abstract base for the setup bundle a party is handed once, when it is
configured, and holds from then on. It is public in full: it is exactly
the message a coordinator would put on a wire to an untrusted peer, and
it is all anyone needs in order to encrypt.

## Usage

``` r
PublicParams()
```

## Value

nothing — this class is abstract. Its concrete subclasses are
constructed for you when a party is configured.

## Details

The class carries **no secret property**, which is what makes the claim
structural rather than a promise in prose — there is nowhere for a
secret key or a key share to travel in this object. The concrete kinds
are
[OpenFHEParams](https://bnaras.github.io/homomorpheR/reference/OpenFHEParams.md)
and, on the frozen legacy path, `PaillierParams`.

Obtain the bundle a site holds with
[`site_params()`](https://bnaras.github.io/homomorpheR/reference/site_params.md);
encrypt with
[`encrypt_under()`](https://bnaras.github.io/homomorpheR/reference/encrypt_under.md).

## See also

[OpenFHEParams](https://bnaras.github.io/homomorpheR/reference/OpenFHEParams.md),
[`site_params()`](https://bnaras.github.io/homomorpheR/reference/site_params.md),
[`encrypt_under()`](https://bnaras.github.io/homomorpheR/reference/encrypt_under.md)
