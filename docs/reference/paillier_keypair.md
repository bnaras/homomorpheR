# Generate a new Paillier key pair

Generates two random primes of `modulus_bits / 2` bits each, forms the
modulus, and returns a
[PaillierKeyPair](https://bnaras.github.io/homomorpheR/reference/PaillierKeyPair.md)
containing the matching public and private keys.

## Usage

``` r
paillier_keypair(modulus_bits)
```

## Arguments

- modulus_bits:

  modulus length in bits (e.g. 1024 or 2048).

## Value

a
[PaillierKeyPair](https://bnaras.github.io/homomorpheR/reference/PaillierKeyPair.md).

## Examples

``` r
keys <- paillier_keypair(1024)
ct   <- encrypt(keys@pubkey, gmp::as.bigz(42))
ct
#> <PaillierCiphertext> (under 1024-bit key)

## Only the private key recovers the cleartext:
decrypt(get_private_key(keys), ct)
#> Big Integer ('bigz') :
#> [1] 42
```
