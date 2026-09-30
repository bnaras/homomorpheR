# Precision

## Introduction

Some computations on encrypted data are exact while others are
approximate. We describe the differences below.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`openfhe.R`](https://openfheorg.github.io/openfhe.R/)`)`

## Exact, for integer schemes

BFV does exact integer arithmetic. A count computed under encryption is
*the same integer* as the count computed in the clear — not a value near
it.

\
`cc``  ``<-`` `[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"BFV"``, plaintext_modulus ``=`` ``65537L``,`\
`                   multiplicative_depth ``=`` ``1L``, batch_size ``=`` ``8L``)`\
`key`` ``<-`` `[`key_gen`](https://openfheorg.github.io/openfhe.R/reference/key_gen.html)`(``cc``)`\
\
`counts`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``46L``, ``15L``, ``52L``)`\
`cts``    ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``counts``, ``function``(``n``)`\
`  `[`encrypt`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)`(``key``@``public``, `[`make_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_packed_plaintext.html)`(``cc``, ``n``)``, cc ``=`` ``cc``)``)`\
\
`total`` ``<-`` `[`decrypt`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)`(`[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``cts``)``, ``key``@``secret``, cc ``=`` ``cc``)`\
[`set_length`](https://openfheorg.github.io/openfhe.R/reference/set_length.html)`(``total``, ``1L``)`\
[`get_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_packed_value.html)`(``total``)``[``1``]`` ``==`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``counts``)`

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

\
`ckks_error`` ``<-`` ``function``(``depth``, ``scaling_mod_size``, ``values``, ``weights`` ``=`` ``NULL``)`` ``{`\
`  ``cc``  ``<-`` `[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``, multiplicative_depth ``=`` ``depth``,`\
`                     scaling_mod_size ``=`` ``scaling_mod_size``,`\
`                     first_mod_size ``=`` ``60L``, batch_size ``=`` ``8L``)`\
`  ``key`` ``<-`` `[`key_gen`](https://openfheorg.github.io/openfhe.R/reference/key_gen.html)`(``cc``, eval_mult ``=`` ``TRUE``)`\
\
`  ``cts`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``values``, ``function``(``v``)`\
`    `[`encrypt`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)`(``key``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``v``)``, cc ``=`` ``cc``)``)`\
\
`  ``if`` ``(`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``weights``)``)`` ``{`\
`    ``ct``       ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``cts``)`\
`    ``expected`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``values``)`\
`  ``}`` ``else`` ``{`\
`    ``ct``       ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`Map`](https://rdrr.io/r/base/funprog.html)`(``function``(``c``, ``w``)`` ``c`` ``*`` ``w``, ``cts``, ``weights``)``)`\
`    ``expected`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`Map`](https://rdrr.io/r/base/funprog.html)`(``function``(``v``, ``w``)`` ``v`` ``*`` ``w``, ``values``, ``weights``)``)`\
`  ``}`\
\
`  ``res`` ``<-`` `[`decrypt`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)`(``ct``, ``key``@``secret``, cc ``=`` ``cc``)`\
`  `[`set_length`](https://openfheorg.github.io/openfhe.R/reference/set_length.html)`(``res``, `[`length`](https://rdrr.io/r/base/length.html)`(``expected``)``)`\
`  ``got`` ``<-`` `[`get_real_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_real_packed_value.html)`(``res``)``[`[`seq_along`](https://rdrr.io/r/base/seq.html)`(``expected``)``]`\
\
`  ``absolute`` ``<-`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``got`` ``-`` ``expected``)``)`\
`  `[`c`](https://rdrr.io/r/base/c.html)`(``absolute ``=`` ``absolute``, relative ``=`` ``absolute`` ``/`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``expected``)``)``)`\
`}`\
\
`## The three settings at a given magnitude are run on the SAME draw, so`\
`## the comparison between them is controlled: only the parameter under`\
`## study changes. Redrawing per row would confound the setting with the`\
`## sample.`\
`settings`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  `[`list`](https://rdrr.io/r/base/list.html)`(``name ``=`` ``"sum"``,                         depth ``=`` ``1L``, sms ``=`` ``50L``, w ``=`` ``NULL``)``,`\
`  `[`list`](https://rdrr.io/r/base/list.html)`(``name ``=`` ``"sum, wider scale"``,            depth ``=`` ``1L``, sms ``=`` ``59L``, w ``=`` ``NULL``)``,`\
`  `[`list`](https://rdrr.io/r/base/list.html)`(``name ``=`` ``"weighted sum (one multiply)"``, depth ``=`` ``2L``, sms ``=`` ``50L``,`\
`       w ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``0.35``, ``-``0.20``, ``0.50``)``)``)`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`\
`rows`` ``<-`` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1e2``, ``1e4``)``, ``function``(``m``)`` ``{`\
`  ``values`` ``<-`` `[`replicate`](https://rdrr.io/r/base/lapply.html)`(``3``, `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``8``, ``m``, ``m`` ``/`` ``10``)``, simplify ``=`` ``FALSE``)`\
`  `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``settings``, ``function``(``s``)`` ``{`\
`    ``e`` ``<-`` ``ckks_error``(``s``$``depth``, ``s``$``sms``, ``values``, ``s``$``w``)`\
`    `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``computation ``=`` ``s``$``name``, magnitude ``=`` ``m``,`\
`               depth ``=`` ``s``$``depth``, scaling_mod_size ``=`` ``s``$``sms``,`\
`               absolute ``=`` ``e``[[``"absolute"``]``]``, relative ``=`` ``e``[[``"relative"``]``]``)`\
`  ``}``)``)`\
`}``)``)`

| Computation | Magnitude | Depth | `scaling_mod_size` | Absolute error | Relative error |
|:---|---:|---:|---:|---:|---:|
| sum | \\1\\ | 1 | 50 | \\1.07 \times 10^{-13}\\ | \\3.27 \times 10^{-14}\\ |
| sum, wider scale | \\1\\ | 1 | 59 | \\8.88 \times 10^{-16}\\ | \\2.73 \times 10^{-16}\\ |
| weighted sum (one multiply) | \\1\\ | 2 | 50 | \\1.57 \times 10^{-13}\\ | \\2.16 \times 10^{-13}\\ |
| sum | \\10^{2}\\ | 1 | 50 | \\1.71 \times 10^{-13}\\ | \\5.20 \times 10^{-16}\\ |
| sum, wider scale | \\10^{2}\\ | 1 | 59 | \\5.68 \times 10^{-14}\\ | \\1.73 \times 10^{-16}\\ |
| weighted sum (one multiply) | \\10^{2}\\ | 2 | 50 | \\1.92 \times 10^{-13}\\ | \\2.71 \times 10^{-15}\\ |
| sum | \\10^{4}\\ | 1 | 50 | \\3.64 \times 10^{-12}\\ | \\1.11 \times 10^{-16}\\ |
| sum, wider scale | \\10^{4}\\ | 1 | 59 | \\3.64 \times 10^{-12}\\ | \\1.11 \times 10^{-16}\\ |
| weighted sum (one multiply) | \\10^{4}\\ | 2 | 50 | \\1.55 \times 10^{-11}\\ | \\2.15 \times 10^{-15}\\ |

CKKS error against the same computation in the clear. {.table .table
.table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

Three things to read off that table.

**State tolerances relatively, not absolutely.** For the plain sum the
absolute error rises from 1.1e-13 at magnitude 1 to 3.6e-12 at magnitude
\\10^4\\, a factor of about 34, while the relative error *falls* — from
3.3e-14 to 1.1e-16. At magnitude 1 a fixed noise floor is large compared
to the answer; by magnitude \\10^4\\ it is negligible against it. A
tolerance calibrated on standardized covariates is therefore far too
tight for a log-likelihood in the hundreds, which is why the Cox
vignettes raise `scaling_mod_size` above the default rather than
loosening a comparison.

**Widening the scaling factor helps only where that floor binds.** At
magnitude 1 it improves the absolute error by a factor of 120. At
magnitude \\10^4\\ the same change gives a factor of 1 — that is,
nothing, and the two settings land within a small multiple of each other
in either direction. Once the floor is no longer what limits the answer,
a wider scale stops buying accuracy while still consuming modulus
budget. It is a targeted fix for small-magnitude work, not a general
accuracy dial.

**Each multiplication costs precision as well as budget.** The depth-2
row is worse than the depth-1 sum at every magnitude: by a factor of 1.5
in absolute terms at magnitude 1, and 19 in relative terms at magnitude
\\10^4\\. The budget is consumed whether or not the extra precision is
missed.

These are comparisons of the encrypted result against *the same
computation performed in the clear on the same data*.
