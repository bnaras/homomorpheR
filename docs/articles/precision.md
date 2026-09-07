# Precision

“The encrypted computation gives the right answer” means different
things in different vignettes, and being clear about which is the
difference between a meaningful check and a misleading one.

[`library`](https://rdrr.io/r/base/library.html)`(`[`openfhe.R`](https://openfheorg.github.io/openfhe.R/)`)`` `[`set_num_threads`](https://openfheorg.github.io/openfhe.R/reference/set_num_threads.html)`(``2L``)`

## Exact, for integer schemes

BFV does exact integer arithmetic. A count computed under encryption is
*the same integer* as the count computed in the clear — not a value near
it.

`cc`` ``<-`` `[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"BFV"``, plaintext_modulus ``=`` ``65537L``,`` `` multiplicative_depth ``=`` ``1L``, batch_size ``=`` ``8L``)`` ``key`` ``<-`` `[`key_gen`](https://openfheorg.github.io/openfhe.R/reference/key_gen.html)`(``cc``)`` `` ``counts`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``46L``, ``15L``, ``52L``)`` ``cts`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``counts``, ``function``(``n``)`` `` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``key``@``public``, `[`make_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_packed_plaintext.html)`(``cc``, ``n``)``, cc ``=`` ``cc``)``)`` `` ``total`` ``<-`` `[`decrypt`](https://bnaras.github.io/homomorpheR/reference/decrypt.md)`(`[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``cts``)``, ``key``@``secret``, cc ``=`` ``cc``)`` `[`set_length`](https://openfheorg.github.io/openfhe.R/reference/set_length.html)`(``total``, ``1L``)`` `[`get_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_packed_value.html)`(``total``)``[``1``]`` ``==`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``counts``)`

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

`ckks_error`` ``<-`` ``function``(``depth``, ``scaling_mod_size``, ``values``, ``weights`` ``=`` ``NULL``)`` ``{`` `` ``cc`` ``<-`` `[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``, multiplicative_depth ``=`` ``depth``,`` `` scaling_mod_size ``=`` ``scaling_mod_size``,`` `` first_mod_size ``=`` ``60L``, batch_size ``=`` ``8L``)`` `` ``key`` ``<-`` `[`key_gen`](https://openfheorg.github.io/openfhe.R/reference/key_gen.html)`(``cc``, eval_mult ``=`` ``TRUE``)`` `` `` ``cts`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``values``, ``function``(``v``)`` `` `[`encrypt`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)`(``key``@``public``, `[`make_ckks_packed_plaintext`](https://openfheorg.github.io/openfhe.R/reference/make_ckks_packed_plaintext.html)`(``cc``, ``v``)``, cc ``=`` ``cc``)``)`` `` `` ``if`` ``(`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``weights``)``)`` ``{`` `` ``ct`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``cts``)`` `` ``expected`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``values``)`` `` ``}`` ``else`` ``{`` `` ``ct`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`Map`](https://rdrr.io/r/base/funprog.html)`(``function``(``c``, ``w``)`` ``c`` ``*`` ``w``, ``cts``, ``weights``)``)`` `` ``expected`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`Map`](https://rdrr.io/r/base/funprog.html)`(``function``(``v``, ``w``)`` ``v`` ``*`` ``w``, ``values``, ``weights``)``)`` `` ``}`` `` `` ``res`` ``<-`` `[`decrypt`](https://bnaras.github.io/homomorpheR/reference/decrypt.md)`(``ct``, ``key``@``secret``, cc ``=`` ``cc``)`` `` `[`set_length`](https://openfheorg.github.io/openfhe.R/reference/set_length.html)`(``res``, `[`length`](https://rdrr.io/r/base/length.html)`(``expected``)``)`` `` ``got`` ``<-`` `[`get_real_packed_value`](https://openfheorg.github.io/openfhe.R/reference/get_real_packed_value.html)`(``res``)``[`[`seq_along`](https://rdrr.io/r/base/seq.html)`(``expected``)``]`` `` `` ``absolute`` ``<-`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``got`` ``-`` ``expected``)``)`` `` `[`c`](https://rdrr.io/r/base/c.html)`(``absolute ``=`` ``absolute``, relative ``=`` ``absolute`` ``/`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``expected``)``)``)`` ``}`` `` ``## The three settings at a given magnitude are run on the SAME draw, so`` ``## the comparison between them is controlled: only the parameter under`` ``## study changes. Redrawing per row would confound the setting with the`` ``## sample.`` ``settings`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``name ``=`` ``"sum"``, depth ``=`` ``1L``, sms ``=`` ``50L``, w ``=`` ``NULL``)``,`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``name ``=`` ``"sum, wider scale"``, depth ``=`` ``1L``, sms ``=`` ``59L``, w ``=`` ``NULL``)``,`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``name ``=`` ``"weighted sum (one multiply)"``, depth ``=`` ``2L``, sms ``=`` ``50L``,`` `` w ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``0.35``, ``-``0.20``, ``0.50``)``)``)`` `` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`` ``rows`` ``<-`` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1e2``, ``1e4``)``, ``function``(``m``)`` ``{`` `` ``values`` ``<-`` `[`replicate`](https://rdrr.io/r/base/lapply.html)`(``3``, `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``8``, ``m``, ``m`` ``/`` ``10``)``, simplify ``=`` ``FALSE``)`` `` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``settings``, ``function``(``s``)`` ``{`` `` ``e`` ``<-`` ``ckks_error``(``s``$``depth``, ``s``$``sms``, ``values``, ``s``$``w``)`` `` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``computation ``=`` ``s``$``name``, magnitude ``=`` ``m``,`` `` depth ``=`` ``s``$``depth``, scaling_mod_size ``=`` ``s``$``sms``,`` `` absolute ``=`` ``e``[[``"absolute"``]``]``, relative ``=`` ``e``[[``"relative"``]``]``)`` `` ``}``)``)`` ``}``)``)`

| computation | magnitude | depth | scaling_mod_size | absolute | relative |
|:---|---:|---:|---:|:---|:---|
| sum | 1 | 1 | 50 | 6.62e-14 | 2.03e-14 |
| sum, wider scale | 1 | 1 | 59 | 8.88e-16 | 2.73e-16 |
| weighted sum (one multiply) | 1 | 2 | 50 | 1.01e-13 | 1.39e-13 |
| sum | 100 | 1 | 50 | 1.71e-13 | 5.20e-16 |
| sum, wider scale | 100 | 1 | 59 | 1.14e-13 | 3.46e-16 |
| weighted sum (one multiply) | 100 | 2 | 50 | 1.85e-13 | 2.61e-15 |
| sum | 10000 | 1 | 50 | 3.64e-12 | 1.11e-16 |
| sum, wider scale | 10000 | 1 | 59 | 7.28e-12 | 2.21e-16 |
| weighted sum (one multiply) | 10000 | 2 | 50 | 1.46e-11 | 2.02e-15 |

CKKS error against the same computation in the clear. {.table}

Three things to read off that table.

**State tolerances relatively, not absolutely.** For the plain sum the
absolute error rises from 6.6e-14 at magnitude 1 to 3.6e-12 at magnitude
$`10^4`$, a factor of about 55, while the relative error *falls* — from
2.0e-14 to 1.1e-16. At magnitude 1 a fixed noise floor is large compared
to the answer; by magnitude $`10^4`$ it is negligible against it. A
tolerance calibrated on standardized covariates is therefore far too
tight for a log-likelihood in the hundreds, which is why the Cox
vignettes raise `scaling_mod_size` above the default rather than
loosening a comparison.

**Widening the scaling factor helps only where that floor binds.** At
magnitude 1 it improves the absolute error by a factor of 74. At
magnitude $`10^4`$ the same change gives a factor of 0.5 — that is,
nothing, and the two settings land within a small multiple of each other
in either direction. Once the floor is no longer what limits the answer,
a wider scale stops buying accuracy while still consuming modulus
budget. It is a targeted fix for small-magnitude work, not a general
accuracy dial.

**Each multiplication costs precision as well as budget.** The depth-2
row is worse than the depth-1 sum at every magnitude: by a factor of 1.5
in absolute terms at magnitude 1, and 18 in relative terms at magnitude
$`10^4`$. The budget is consumed whether or not the extra precision is
missed.

These are comparisons of the encrypted result against *the same
computation performed in the clear on the same data*. That is the actual
cryptographic claim, and it is the tightest bound any vignette here can
make.

## Statistical, for fitted estimates

When an optimizer runs through the encrypted channel, what comes back is
compared against a centralized fit. The relevant tolerance is then not
CKKS precision but a *statistical* one: two optimizers with different
stopping rules do not halt at the same point even on identical data, and
a difference between them is meaningless if it is small relative to the
standard error.

[`vignette("cox")`](https://bnaras.github.io/homomorpheR/articles/cox.md)
states its agreement in standard-error units for exactly that reason. A
difference of a thousandth of one standard error is invisible in any
reported result, and quoting it as a bare number would invite the reader
to compare it against a CKKS error bound — a category mistake.

## What cannot be compared

**Differentially private runs.**
[`vignette("cox-threshold-dp")`](https://bnaras.github.io/homomorpheR/articles/cox-threshold-dp.md)
and
[`vignette("cvxr-consensus-admm-dp")`](https://bnaras.github.io/homomorpheR/articles/cvxr-consensus-admm-dp.md)
draw fresh noise on every query, so two runs of the same code on the
same data disagree by construction. Only distributional behavior is
comparable: how the error grows with $`\sigma`$, and which optimizers
degrade first.

**Iteration counts.** A loop that stops when a residual first falls
below a threshold is reporting where a continuous quantity crossed a
line. CKKS noise alone can move it by one, and redrawing the data moves
it further. It is a property of the sample, not of the method.

## Where the data comes from

**Measured data ships with the package.** The DLBCL cohort is real, so
every vignette that uses it reads the same values each time and its
numbers are reproducible in the strict sense.

**Simulated data is drawn fresh.** The other vignettes generate their
cohorts on each run. The claim they make is that the encrypted protocol
reproduces the cleartext answer *on whatever data it was given*, which
is a stronger claim than reproducing one fixed dataset.

**Expensive fits are precomputed.** `data(cvxr_consensus)` carries the
~150-iteration ADMM results that
[`vignette("cvxr-cox-lasso-dlbcl")`](https://bnaras.github.io/homomorpheR/articles/cvxr-cox-lasso-dlbcl.md)
would otherwise recompute on every build. The script that produced it
ships with the package, so the artifact is regenerable rather than
merely asserted.

## Categorical order is protocol semantics

Sites are visited in a fixed order, and the first is the lead decryptor
in the threshold ceremony. In the Cox vignettes the sites come from
splitting the cohort on a factor, so that factor’s *level* order
determines which site leads:

[`data`](https://rdrr.io/r/utils/data.html)`(``DLBCL``, package ``=`` ``"homomorpheR"``)`` `[`levels`](https://rdrr.io/r/base/levels.html)`(``DLBCL``$``Subgroup``)`

    ## [1] "GCB"      "ABC"      "Type III"

That is not alphabetical:

[`sort`](https://rdrr.io/r/base/sort.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(`[`as.character`](https://rdrr.io/r/base/character.html)`(``DLBCL``$``Subgroup``)``)``)`

    ## [1] "ABC"      "GCB"      "Type III"

[`split()`](https://rdrr.io/r/base/split.html) orders its output by
factor levels, so the protocol visits GCB first. Had `Subgroup` been
stored as a character vector,
[`split()`](https://rdrr.io/r/base/split.html) would have sorted it and
put ABC first instead — silently swapping which site holds the lead
decryption role. The column is a factor with a deliberate level order
for that reason, and it is worth knowing before anyone “tidies” it.
