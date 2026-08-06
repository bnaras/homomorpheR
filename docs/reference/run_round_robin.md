# Run one round of the round-robin protocol

Backend-agnostic via the
[`master_encrypt()`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md)
/
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
generics, but part of the frozen Paillier-era legacy surface: the
random-offset chain idiom compensated for Paillier-era trust
assumptions. The supported pattern is
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md).

## Usage

``` r
run_round_robin(master, theta)
```

## Arguments

- master:

  a [Master](https://bnaras.github.io/homomorpheR/reference/Master.md),
  wired to a chain via
  [`round_robin_chain()`](https://bnaras.github.io/homomorpheR/reference/round_robin_chain.md).

- theta:

  the current parameter value (passed through to each worker's
  `local_fn`).

## Value

the aggregated value, or `NA_real_` on failure.

## Details

The master generates a random real offset, encrypts it under its public
key, and sends it around the chain. Each worker site adds its encrypted
local summary to the running total and forwards. On return, the master
decrypts the running total, subtracts the offset in the clear, and
returns the resulting scalar.

If any worker's `local_fn` returns `NA`, the chain stops and this
function returns `NA_real_`.
