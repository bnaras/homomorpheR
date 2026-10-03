# Precision

## Introduction

Some computations on encrypted data are exact while others are
approximate. We describe the differences below.

``` r

library(openfhe.R)
```

## Exact, for integer schemes

BFV does exact integer arithmetic. A count computed under encryption is
*the same integer* as the count computed in the clear — not a value near
it.

``` r

cc  <- fhe_context("BFV", plaintext_modulus = 65537L,
                   multiplicative_depth = 1L, batch_size = 8L)
key <- key_gen(cc)

counts <- c(46L, 15L, 52L)
cts    <- lapply(counts, function(n)
  encrypt(key@public, make_packed_plaintext(cc, n), cc = cc))

total <- decrypt(Reduce(`+`, cts), key@secret, cc = cc)
set_length(total, 1L)
get_packed_value(total)[1] == sum(counts)
```

    ## [1] TRUE

[`vignette("privacy-preserving-aggregation")`](https://bnaras.github.io/homomorpheR/articles/privacy-preserving-aggregation.md)
and
[`vignette("query-count-threshold")`](https://bnaras.github.io/homomorpheR/articles/query-count-threshold.md)
assert equality exactly like this. A tolerance there would be hiding a
bug rather than accommodating one.

## Approximate, for CKKS

CKKS encrypts real numbers and trades exactness for that ability.
Results carry an approximation error that grows with the depth of the
computation and shrinks as the scaling factor widens.

``` r

ckks_error <- function(depth, scaling_mod_size, values, weights = NULL) {
  cc  <- fhe_context("CKKS", multiplicative_depth = depth,
                     scaling_mod_size = scaling_mod_size,
                     first_mod_size = 60L, batch_size = 8L)
  key <- key_gen(cc, eval_mult = TRUE)

  cts <- lapply(values, function(v)
    encrypt(key@public, make_ckks_packed_plaintext(cc, v), cc = cc))

  if (is.null(weights)) {
    ct       <- Reduce(`+`, cts)
    expected <- Reduce(`+`, values)
  } else {
    ct       <- Reduce(`+`, Map(function(c, w) c * w, cts, weights))
    expected <- Reduce(`+`, Map(function(v, w) v * w, values, weights))
  }

  res <- decrypt(ct, key@secret, cc = cc)
  set_length(res, length(expected))
  got <- get_real_packed_value(res)[seq_along(expected)]

  absolute <- max(abs(got - expected))
  c(absolute = absolute, relative = absolute / max(abs(expected)))
}

## The three settings at a given magnitude are run on the SAME draw, so
## the comparison between them is controlled: only the parameter under
## study changes. Redrawing per row would confound the setting with the
## sample.
settings <- list(
  list(name = "sum",                         depth = 1L, sms = 50L, w = NULL),
  list(name = "sum, wider scale",            depth = 1L, sms = 59L, w = NULL),
  list(name = "weighted sum (one multiply)", depth = 2L, sms = 50L,
       w = list(0.35, -0.20, 0.50)))

set.seed(1)
rows <- do.call(rbind, lapply(c(1, 1e2, 1e4), function(m) {
  values <- replicate(3, rnorm(8, m, m / 10), simplify = FALSE)
  do.call(rbind, lapply(settings, function(s) {
    e <- ckks_error(s$depth, s$sms, values, s$w)
    data.frame(computation = s$name, magnitude = m,
               depth = s$depth, scaling_mod_size = s$sms,
               absolute = e[["absolute"]], relative = e[["relative"]])
  }))
}))
```

| Computation | Magnitude | Depth | `scaling_mod_size` | Absolute error | Relative error |
|:---|---:|---:|---:|---:|---:|
| sum | \\1\\ | 1 | 50 | \\1.08 \times 10^{-13}\\ | \\3.31 \times 10^{-14}\\ |
| sum, wider scale | \\1\\ | 1 | 59 | \\8.88 \times 10^{-16}\\ | \\2.73 \times 10^{-16}\\ |
| weighted sum (one multiply) | \\1\\ | 2 | 50 | \\1.29 \times 10^{-13}\\ | \\1.77 \times 10^{-13}\\ |
| sum | \\10^{2}\\ | 1 | 50 | \\1.14 \times 10^{-13}\\ | \\3.46 \times 10^{-16}\\ |
| sum, wider scale | \\10^{2}\\ | 1 | 59 | \\5.68 \times 10^{-14}\\ | \\1.73 \times 10^{-16}\\ |
| weighted sum (one multiply) | \\10^{2}\\ | 2 | 50 | \\3.27 \times 10^{-13}\\ | \\4.62 \times 10^{-15}\\ |
| sum | \\10^{4}\\ | 1 | 50 | \\7.28 \times 10^{-12}\\ | \\2.21 \times 10^{-16}\\ |
| sum, wider scale | \\10^{4}\\ | 1 | 59 | \\3.64 \times 10^{-12}\\ | \\1.11 \times 10^{-16}\\ |
| weighted sum (one multiply) | \\10^{4}\\ | 2 | 50 | \\1.46 \times 10^{-11}\\ | \\2.02 \times 10^{-15}\\ |

CKKS error against the same computation in the clear. {.table .table
.table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

Three things to read off that table.

**State tolerances relatively, not absolutely.** For the plain sum the
absolute error rises from 1.1e-13 at magnitude 1 to 7.3e-12 at magnitude
\\10^4\\, a factor of about 67, while the relative error *falls* — from
3.3e-14 to 2.2e-16. At magnitude 1 a fixed noise floor is large compared
to the answer; by magnitude \\10^4\\ it is negligible against it. A
tolerance calibrated on standardized covariates is therefore far too
tight for a log-likelihood in the hundreds, which is why the Cox
vignettes raise `scaling_mod_size` above the default rather than
loosening a comparison.

**Widening the scaling factor helps only where that floor binds.** At
magnitude 1 it improves the absolute error by a factor of 120. At
magnitude \\10^4\\ the same change gives a factor of 2 — that is,
nothing, and the two settings land within a small multiple of each other
in either direction. Once the floor is no longer what limits the answer,
a wider scale stops buying accuracy while still consuming modulus
budget. It is a targeted fix for small-magnitude work, not a general
accuracy dial.

**Each multiplication costs precision as well as budget.** The depth-2
row is worse than the depth-1 sum at every magnitude: by a factor of 1.2
in absolute terms at magnitude 1, and 9.1 in relative terms at magnitude
\\10^4\\. The budget is consumed whether or not the extra precision is
missed.

These are comparisons of the encrypted result against *the same
computation performed in the clear on the same data*.
