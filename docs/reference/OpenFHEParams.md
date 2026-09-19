# Public parameters for the `openfhe.R` backends

The crypto context and the public key to encrypt under — the joint
public key when the protocol uses threshold keys. Both are public. The
scheme is read back from the context, so one class serves CKKS, BFV, and
BGV.

## Usage

``` r
OpenFHEParams(cc = openfhe.R::CryptoContext(), pk = openfhe.R::PublicKey())
```

## Arguments

- cc:

  an `openfhe.R` `CryptoContext`.

- pk:

  an `openfhe.R` `PublicKey`.

## Value

an S7 object of class `OpenFHEParams`, inheriting from
[PublicParams](https://bnaras.github.io/homomorpheR/reference/PublicParams.md),
with properties `cc` and `pk`.

## See also

[`encrypt_under()`](https://bnaras.github.io/homomorpheR/reference/encrypt_under.md),
[`site_params()`](https://bnaras.github.io/homomorpheR/reference/site_params.md)
