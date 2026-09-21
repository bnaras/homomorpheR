# Consensus ADMM + Output Differential Privacy (Demonstration)

## Introduction

This vignette is a **demonstration**, not a recommendation.
[`vignette("cvxr-consensus-admm")`](https://bnaras.github.io/homomorpheR/articles/cvxr-consensus-admm.md)
shows the lossless consensus-ADMM protocol over the threshold-FHE
channel: bit-identical to the centralized CVXR fit, no single decrypter,
residuals drive convergence. The aggregator sees the full trajectory
$`\{z^k\}`$ in the clear after each iteration’s threshold fusion.

A natural question is whether one can compose that lossless channel with
**output differential privacy** to bound the trajectory leakage
formally. The answer in this simulation is: yes, the cryptographic
protocol composes cleanly and the implementation is one extra
[`rnorm()`](https://rdrr.io/r/stats/Normal.html) call per site per
iteration, but the accuracy/privacy trade-off does not produce a usable
point at small $`\varepsilon`$ given the optimizer structure here. This
vignette runs that trade-off end-to-end.

`feedback_he_not_dp` in the package’s notes records the framing
explicitly: lossless threshold-FHE is the package’s primary path; DP
demonstrations show *what happens if* a user composes output DP on top,
not the recommended deployment for precision-critical workloads.

## A brief output-DP primer

Given a query $`f: \mathcal{D} \to \mathbb{R}^p`$, the **Gaussian
mechanism** releases
$`\tilde f(D) = f(D) + \mathcal{N}(0, \sigma^2 I)`$. Sensitivity-driven
$`\sigma`$ gives single-query $`(\varepsilon, \delta)`$ DP. Multi-query
composition uses **zCDP** \[Bun & Steinke 2016\]: each release is
$`(\Delta/\sigma)^2/2`$-zCDP; $`T`$ releases compose linearly to
$`T \cdot \rho`$; convert back to $`(\varepsilon, \delta)`$ via
$`\varepsilon = \rho + 2\sqrt{\rho \log(1/\delta)}`$.

## The protocol modification

The setup is identical to
[`vignette("cvxr-consensus-admm")`](https://bnaras.github.io/homomorpheR/articles/cvxr-consensus-admm.md).
The only change is **noise injection at each site**: at every ADMM
iteration site $`i`$ adds an independent draw
$`\eta_i \sim \mathcal{N}(0, \sigma^2 N \cdot I)`$ to
$`x_i + u_i`$*before* encrypting. The encrypted noise terms sum under
the joint key, the $`1/N`$ scaling contracts the variance back to
$`\sigma^2`$ per coordinate, and the recovered $`z`$ has noise
$`\mathcal{N}(0, \sigma^2 I)`$.

Distributing the noise across sites (rather than centralizing it at the
aggregator) means:

1.  No single point ever holds the noiseless value. Even if the
    aggregator is compromised it sees only encrypted *noised*
    contributions until the final fusion.
2.  The trust model matches `cox-threshold-dp`: each site is its own
    randomness boundary.

Both are properties of the code below rather than of this paragraph:
`site_contribution_dp()` draws the noise *and* encrypts inside the site,
so `encrypted_consensus_dp()` receives encrypted values and nothing
else. Had the aggregator done the noising — reading each site’s
$`x_i + u_i`$ and adding a draw itself — every sentence above would be
false while the numbers came out identical.

## The stopping rule changes

Residual-based convergence checks are not meaningful under noise: the
residuals never shrink below the per-iteration noise floor. So $`T`$ is
**fixed in advance** rather than discovered.

That is forced twice over. Even without noise, a residual-based stop is
an adaptive, data-dependent decision — the iteration at which you halt
is a function of the records — and $`T`$ is exactly the quantity that
multiplies the privacy budget below. A protocol that reads $`T`$ off the
cohort and then bills $`T`$ releases against $`\varepsilon`$ has left
the accounting open at the step that defines it. The next section picks
$`\rho`$ and $`T`$ without touching the cohort at all.

The $`\sigma = 0`$ row of the table below verifies that the protocol
with the noise mechanism turned off lands on the lossless ADMM fit at
that pre-committed $`T`$.

## Setup

\
[`suppressPackageStartupMessages`](https://rdrr.io/r/base/message.html)`(``{`\
`    `[`library`](https://rdrr.io/r/base/library.html)`(`[`homomorpheR`](https://bnaras.github.io/homomorpheR/)`)`\
`    `[`library`](https://rdrr.io/r/base/library.html)`(`[`CVXR`](https://cvxr.rbind.io)`)`\
`    `[`library`](https://rdrr.io/r/base/library.html)`(`[`S7`](https://rconsortium.github.io/S7/)`)`\
`}``)`\
\
`N``   ``<-`` ``3L`\
`p``   ``<-`` ``4L`\
`lam`` ``<-`` ``1`

\
`build_local_problem`` ``<-`` ``function``(``X_i``, ``y_i``, ``rho_val``)`` ``{`\
`    ``x``  ``<-`` `[`Variable`](https://www.cvxgrp.org/CVXR/reference/Variable.html)`(``p``)`\
`    ``zp`` ``<-`` `[`Parameter`](https://www.cvxgrp.org/CVXR/reference/Parameter.html)`(``p``)`\
`    ``up`` ``<-`` `[`Parameter`](https://www.cvxgrp.org/CVXR/reference/Parameter.html)`(``p``)`\
`    ``y_signs`` ``<-`` ``2`` ``*`` ``y_i`` ``-`` ``1`\
`    ``margins`` ``<-`` ``-``y_signs`` ``*`` ``(``X_i`` `[`%*%`](https://rdrr.io/r/base/matmult.html)` ``x``)`\
`    ``local_loss`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`logistic`](https://www.cvxgrp.org/CVXR/reference/logistic.html)`(``margins``)``)`` ``+`\
`                  ``(``lam`` ``/`` ``(``2`` ``*`` ``N``)``)`` ``*`` `[`sum_squares`](https://www.cvxgrp.org/CVXR/reference/sum_squares.html)`(``x``)`\
`    ``augmented``  ``<-`` ``(``rho_val`` ``/`` ``2``)`` ``*`` `[`sum_squares`](https://www.cvxgrp.org/CVXR/reference/sum_squares.html)`(``x`` ``-`` ``zp`` ``+`` ``up``)`\
`    ``prob`` ``<-`` `[`Problem`](https://www.cvxgrp.org/CVXR/reference/Problem.html)`(`[`Minimize`](https://www.cvxgrp.org/CVXR/reference/Minimize.html)`(``local_loss`` ``+`` ``augmented``)``)`\
`    `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``zp``)`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p``)``; `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``up``)`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p``)`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``prob ``=`` ``prob``, x ``=`` ``x``, zp ``=`` ``zp``, up ``=`` ``up``)`\
`}`\
\
`` ## Inherits homomorpheR's abstract `Site` (which supplies `name` and the ``\
`` ## `state` environment), so it can take part in threshold key generation ``\
`## and keep its own share.`\
`ConsensusSite`` ``<-`` `[`new_class`](https://rconsortium.github.io/S7/reference/new_class.html)`(``"ConsensusSite"``,`\
`    parent     ``=`` ``homomorpheR``::`[`Site`](https://bnaras.github.io/homomorpheR/reference/Site.md)`,`\
`    properties ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``n ``=`` ``class_integer``)``)`\
\
`make_consensus_site`` ``<-`` ``function``(``name``, ``X_i``, ``y_i``, ``rho_val``)`` ``{`\
`    ``st``        ``<-`` `[`new.env`](https://rdrr.io/r/base/environment.html)`(``parent ``=`` `[`emptyenv`](https://rdrr.io/r/base/environment.html)`(``)``)`\
`    ``st``$``X``      ``<-`` ``X_i`\
`    ``st``$``y``      ``<-`` ``y_i`\
`    ``built``     ``<-`` ``build_local_problem``(``X_i``, ``y_i``, ``rho_val``)`\
`    ``st``$``prob``   ``<-`` ``built``$``prob`\
`    ``st``$``x_var``  ``<-`` ``built``$``x`\
`    ``st``$``zp``     ``<-`` ``built``$``zp`\
`    ``st``$``up``     ``<-`` ``built``$``up`\
`    ``st``$``x_curr`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``X_i``)``)`\
`    ``st``$``u_curr`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``X_i``)``)`\
`    ``ConsensusSite``(``name ``=`` ``name``, n ``=`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``X_i``)``, state ``=`` ``st``)`\
`}`\
\
`local_update`` ``<-`` ``function``(``site``, ``z_curr``)`` ``{`\
`    ``st`` ``<-`` ``site``@``state`\
`    `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``st``$``zp``)`` ``<-`` ``z_curr`\
`    `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``st``$``up``)`` ``<-`` ``st``$``u_curr`\
`    `[`suppressMessages`](https://rdrr.io/r/base/message.html)`(`[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(`[`psolve`](https://www.cvxgrp.org/CVXR/reference/psolve.html)`(``st``$``prob``, solver ``=`` ``"CLARABEL"``)``)``)`\
`    ``if`` ``(``!`[`status`](https://www.cvxgrp.org/CVXR/reference/status.html)`(``st``$``prob``)`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"optimal"``, ``"optimal_inaccurate"``)``)`\
`        `[`stop`](https://rdrr.io/r/base/stop.html)`(``"Local CVXR solve at "``, ``site``@``name``, ``" did not reach optimal status."``)`\
`    ``st``$``x_curr`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``st``$``x_var``)``)`\
`    `[`invisible`](https://rdrr.io/r/base/invisible.html)`(``st``$``x_curr``)`\
`}`

## Simulated cohort

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``20260412``)`\
`n_per_site`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``500L``, ``1000L``, ``1500L``)`\
`beta_true``  ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``intercept ``=`` ``-``0.5``, age ``=`` ``0.4``, bmi ``=`` ``-``0.3``, sex ``=`` ``0.6``)`\
\
`make_site_data`` ``<-`` ``function``(``n``)`` ``{`\
`    ``X``  ``<-`` `[`cbind`](https://rdrr.io/r/base/cbind.html)`(``1``, `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)``, `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)``, `[`rbinom`](https://rdrr.io/r/stats/Binomial.html)`(``n``, ``1``, ``0.5``)``)`\
`    ``pr`` ``<-`` `[`plogis`](https://rdrr.io/r/stats/Logistic.html)`(`[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``X`` `[`%*%`](https://rdrr.io/r/base/matmult.html)` ``beta_true``)``)`\
`    ``y``  ``<-`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(`[`runif`](https://rdrr.io/r/stats/Uniform.html)`(``n``)`` ``<`` ``pr``)`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``X ``=`` ``X``, y ``=`` ``y``)`\
`}`\
`site_data`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``n_per_site``, ``make_site_data``)`

## Choosing $`\rho`$ and $`T`$ without touching the cohort

The lossless ADMM vignette tunes $`\rho`$ by sweeping candidates through
the encrypted channel, *on the real data*. Under output DP that sweep is
not available to us. Every read of the cohort is a release that has to
be paid for, and a sweep is a large one: it solves at every site, at
every candidate, at every iteration, and what comes out — the chosen
$`\rho`$, and above all $`T`$ — is a function of the records. Billing
$`T \cdot N`$ Gaussian releases while $`T`$ itself was read off the data
is not an accounting, it is an accounting with its own parameter left
outside.

There are two honest ways to close that. One is to **pay for the
selection**: report-noisy-max or the exponential mechanism over the
grid, with negative iteration count as utility. That is the principled
route, but it needs a sensitivity bound for “iterations to convergence”
under a one-record change, and we do not have one — it would trade this
gap for a weaker one.

The other is to make the selection **data-independent**, which is what
DP deployments in practice do, and what we do here. The sweep runs on a
**surrogate cohort** built only from facts the protocol already treats
as public — how many sites there are, roughly how large they are, and
the covariate schema — together with nominal effect sizes written into
the analysis plan and claimed by nobody to be correct. No record from
any site enters it.

Two consequences worth being explicit about. First, this sweep needs
**no encrypted channel**: there is no secret in it to protect. That is
the exact opposite of the situation in
[`vignette("cvxr-consensus-admm")`](https://bnaras.github.io/homomorpheR/articles/cvxr-consensus-admm.md),
where the sweep goes through the threshold channel *because* it touches
real data. Cleartext here is a conclusion, not a convenience. Second,
the cost is real but it is paid in **utility, not privacy**: a surrogate
that misjudges the curvature returns a $`\rho`$ the cohort would not
have picked and a $`T`$ shorter than it needed. There is no way to check
that against the cohort without spending budget, so the $`T`$ below is a
pre-commitment — and since the budget grows linearly in $`T`$,
pre-committing generously is not free either.

\
`tol``      ``<-`` ``1e-3`\
`max_iter`` ``<-`` ``60L`\
\
`## Public design facts: three sites of these approximate sizes, four`\
`## covariates of these types. Nominal effect sizes, not the cohort's.`\
`beta_nominal``   ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0.5``, ``0.5``, ``0.5``)`\
`surrogate_seed`` ``<-`` ``20260413L`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``surrogate_seed``)`\
`surrogate_data`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``n_per_site``, ``function``(``n``)`` ``{`\
`    ``X``  ``<-`` `[`cbind`](https://rdrr.io/r/base/cbind.html)`(``1``, `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)``, `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)``, `[`rbinom`](https://rdrr.io/r/stats/Binomial.html)`(``n``, ``1``, ``0.5``)``)`\
`    ``pr`` ``<-`` `[`plogis`](https://rdrr.io/r/stats/Logistic.html)`(`[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``X`` `[`%*%`](https://rdrr.io/r/base/matmult.html)` ``beta_nominal``)``)`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``X ``=`` ``X``, y ``=`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(`[`runif`](https://rdrr.io/r/stats/Uniform.html)`(``n``)`` ``<`` ``pr``)``)`\
`}``)`\
\
`sweep_one_rho`` ``<-`` ``function``(``cohort``, ``rho_val``)`` ``{`\
`    ``built`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``cohort``,`\
`                    ``function``(``s``)`` ``build_local_problem``(``s``$``X``, ``s``$``y``, ``rho_val``)``)`\
`    ``x_curr`` ``<-`` ``u_curr`` ``<-`` `[`replicate`](https://rdrr.io/r/base/lapply.html)`(``N``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p``)``, simplify ``=`` ``FALSE``)`\
`    ``z``      ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p``)`\
`    ``k_conv`` ``<-`` ``NA_integer_`\
`    ``for`` ``(``k`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``max_iter``)``)`` ``{`\
`        ``for`` ``(``i`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``N``)``)`` ``{`\
`            `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``built``[[``i``]``]``$``zp``)`` ``<-`` ``z`\
`            `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``built``[[``i``]``]``$``up``)`` ``<-`` ``u_curr``[[``i``]``]`\
`            `[`suppressMessages`](https://rdrr.io/r/base/message.html)`(`[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(`\
`                `[`psolve`](https://www.cvxgrp.org/CVXR/reference/psolve.html)`(``built``[[``i``]``]``$``prob``, solver ``=`` ``"CLARABEL"``)``)``)`\
`            ``x_curr``[[``i``]``]`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``built``[[``i``]``]``$``x``)``)`\
`        ``}`\
`        ``z_prev`` ``<-`` ``z`\
`        ``z`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`Map`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``x_curr``, ``u_curr``)``)`` ``/`` ``N`\
`        ``for`` ``(``i`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``N``)``)`` ``u_curr``[[``i``]``]`` ``<-`` ``u_curr``[[``i``]``]`` ``+`` ``x_curr``[[``i``]``]`` ``-`` ``z`\
`        ``pri`` ``<-`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`sum`](https://rdrr.io/r/base/sum.html)`(`[`vapply`](https://rdrr.io/r/base/lapply.html)`(`[`seq_len`](https://rdrr.io/r/base/seq.html)`(``N``)``,`\
`            ``function``(``i``)`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``(``x_curr``[[``i``]``]`` ``-`` ``z``)``^``2``)``, ``0``)``)`` ``/`` ``N``)`\
`        ``dua`` ``<-`` ``rho_val`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`sum`](https://rdrr.io/r/base/sum.html)`(``(``z`` ``-`` ``z_prev``)``^``2``)``)`\
`        ``if`` ``(``pri`` ``<`` ``tol`` ``&&`` ``dua`` ``<`` ``tol``)`` ``{`` ``k_conv`` ``<-`` ``k``; ``break`` ``}`\
`    ``}`\
`    `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``rho ``=`` ``rho_val``,`\
`               iters ``=`` ``if`` ``(`[`is.na`](https://rdrr.io/r/base/NA.html)`(``k_conv``)``)`` ``max_iter`` ``else`` ``k_conv``,`\
`               converged ``=`` ``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``k_conv``)``)`\
`}`\
\
`rho_grid``  ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``10``, ``20``, ``50``, ``100``, ``500``)`\
`rho_sweep`` ``<-`` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``,`\
`                     `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``rho_grid``,`\
`                            ``function``(``r``)`` ``sweep_one_rho``(``surrogate_data``, ``r``)``)``)`\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``rho_sweep``,`\
`             caption ``=`` ``"Consensus-ADMM convergence on the surrogate cohort"``)`

| rho | iters | converged |
|----:|------:|:----------|
|  10 |    60 | FALSE     |
|  20 |    60 | FALSE     |
|  50 |    33 | TRUE      |
| 100 |    28 | TRUE      |
| 500 |    60 | FALSE     |

Consensus-ADMM convergence on the surrogate cohort {.table}

\
`converged_rows`` ``<-`` ``rho_sweep``[``rho_sweep``$``converged``, ``]`\
`if`` ``(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``converged_rows``)`` ``==`` ``0L``)`\
`    `[`stop`](https://rdrr.io/r/base/stop.html)`(``"No rho in the grid converged within max_iter on the surrogate."``)`\
\
`rho_chosen`` ``<-`` ``converged_rows``$``rho``[`[`which.min`](https://rdrr.io/r/base/which.min.html)`(``converged_rows``$``iters``)``]`\
`T_fixed``    ``<-`` ``converged_rows``$``iters``[``converged_rows``$``rho`` ``==`` ``rho_chosen``]`\
[`cat`](https://rdrr.io/r/base/cat.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"Pre-committed rho = %g, T = %d (surrogate cohort).\n"``,`\
`            ``rho_chosen``, ``T_fixed``)``)`

    ## Pre-committed rho = 100, T = 28 (surrogate cohort).

These two numbers are now constants of the protocol. The DP-ADMM loop
below runs for exactly $`T = 28`$ iterations regardless of residuals,
and nothing downstream is allowed to revisit them in light of what the
cohort does.

## Threshold-FHE setup

\
`cc`` ``<-`` ``openfhe.R``::`[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`\
`                           multiplicative_depth ``=`` ``1L``,`\
`                           scaling_mod_size     ``=`` ``59L``,`\
`                           first_mod_size       ``=`` ``60L``,`\
`                           batch_size           ``=`` ``8L``,`\
`                           features             ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``openfhe.R``::`[`Feature`](https://openfheorg.github.io/openfhe.R/reference/Feature.html)`$``MULTIPARTY``)``)`

The DP version of the consensus step. The only change from the lossless
ADMM vignette’s `encrypted_consensus()` is the
`rnorm(p, ..., sd = sigma * sqrt(Nv))` term inside the per-site loop:

\
`## Site-side, and this is the whole point of the DP variant: the site`\
`## draws its own noise, adds it, and encrypts with the public bundle it`\
`## was handed at setup -- all before anything leaves. The noiseless`\
`## x_i + u_i exists nowhere but here, which is what "each site is its`\
`## own randomness boundary" has to mean.`\
`site_contribution_dp`` ``<-`` ``function``(``site``, ``sigma``, ``Nv``)`` ``{`\
`    ``st`` ``<-`` ``site``@``state`\
`    ``noised`` ``<-`` ``st``$``x_curr`` ``+`` ``st``$``u_curr`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``p``, mean ``=`` ``0``, sd ``=`` ``sigma`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``Nv``)``)`\
`    `[`encrypt_under`](https://bnaras.github.io/homomorpheR/reference/encrypt_under.md)`(`[`site_params`](https://bnaras.github.io/homomorpheR/reference/site_params.md)`(``site``)``, ``noised``)`\
`}`\
\
`## Aggregator-side: sum the encrypted values, scale, threshold-decrypt. The`\
`## 1/N scaling contracts the summed noise variance back to sigma^2.`\
`encrypted_consensus_dp`` ``<-`` ``function``(``threshold_master``, ``sites``, ``sigma``)`` ``{`\
`    ``Nv``  ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(``sites``)`\
`    ``cts`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``site_contribution_dp``, sigma ``=`` ``sigma``, Nv ``=`` ``Nv``)`\
`    ``ct_avg`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``cts``)`` ``*`` ``(``1`` ``/`` ``Nv``)`\
`    `[`master_decrypt`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)`(``threshold_master``, ``ct_avg``, len ``=`` ``p``)`\
`}`

## The DP-ADMM loop

\
`run_dp_admm`` ``<-`` ``function``(``sigma``, ``T_iter`` ``=`` ``T_fixed``, ``seed`` ``=`` ``NULL``)`` ``{`\
`    ``if`` ``(``!`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``seed``)``)`` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``seed``)`\
`    ``## The sites exist first: the joint public key is built from them,`\
`    ``## each keeping the share it generates.`\
`    ``sites`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        ``make_consensus_site``(``"Site 1"``, ``site_data``[[``1``]``]``$``X``, ``site_data``[[``1``]``]``$``y``, ``rho_chosen``)``,`\
`        ``make_consensus_site``(``"Site 2"``, ``site_data``[[``2``]``]``$``X``, ``site_data``[[``2``]``]``$``y``, ``rho_chosen``)``,`\
`        ``make_consensus_site``(``"Site 3"``, ``site_data``[[``3``]``]``$``X``, ``site_data``[[``3``]``]``$``y``, ``rho_chosen``)``)`\
`    ``master`` ``<-`` `[`make_threshold_master`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)`(``"Aggregator"``,`\
`                                    crypto_context ``=`` ``cc``,`\
`                                    sites          ``=`` ``sites``)`\
\
`    ``z_curr`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p``)`\
`    ``z_hist`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(``NA_real_``, nrow ``=`` ``T_iter``, ncol ``=`` ``p``,`\
`                     dimnames ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``NULL``, `[`names`](https://rdrr.io/r/base/names.html)`(``beta_true``)``)``)`\
`    ``for`` ``(``k`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``T_iter``)``)`` ``{`\
`        ``for`` ``(``s`` ``in`` ``sites``)`` ``local_update``(``s``, ``z_curr``)`\
`        ``z_curr`` ``<-`` ``encrypted_consensus_dp``(``master``, ``sites``, ``sigma``)`\
`        ``for`` ``(``s`` ``in`` ``sites``)`` ``{`\
`            ``s``@``state``$``u_curr`` ``<-`` ``s``@``state``$``u_curr`` ``+`` ``(``s``@``state``$``x_curr`` ``-`` ``z_curr``)`\
`        ``}`\
`        ``z_hist``[``k``, ``]`` ``<-`` ``z_curr`\
`    ``}`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``z ``=`` ``z_curr``, z_hist ``=`` ``z_hist``)`\
`}`

## Centralized CVXR fit

This fit, and every `max_dev` column derived from it below, is a
**simulation diagnostic and not part of the protocol**. It pools the raw
data, which the deployed protocol never does, and it is not charged to
the budget because it is not released — it exists so that this document
can show you how far the noise moved the answer. A real deployment has
no access to it, which is precisely why $`\rho`$ and $`T`$ had to be
pre-committed above.

\
`X_pooled``  ``<-`` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``site_data``, ``` `[[` ```, ``"X"``)``)`\
`y_pooled``  ``<-`` `[`unlist`](https://rdrr.io/r/base/unlist.html)`(`[`lapply`](https://rdrr.io/r/base/lapply.html)`(``site_data``, ``` `[[` ```, ``"y"``)``)`\
`beta_var``  ``<-`` `[`Variable`](https://www.cvxgrp.org/CVXR/reference/Variable.html)`(``p``)`\
`y_signs_p`` ``<-`` ``2`` ``*`` ``y_pooled`` ``-`` ``1`\
`margins_p`` ``<-`` ``-``y_signs_p`` ``*`` ``(``X_pooled`` `[`%*%`](https://rdrr.io/r/base/matmult.html)` ``beta_var``)`\
[`suppressMessages`](https://rdrr.io/r/base/message.html)`(`[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(`\
`    `[`psolve`](https://www.cvxgrp.org/CVXR/reference/psolve.html)`(`[`Problem`](https://www.cvxgrp.org/CVXR/reference/Problem.html)`(`[`Minimize`](https://www.cvxgrp.org/CVXR/reference/Minimize.html)`(`[`sum`](https://rdrr.io/r/base/sum.html)`(`[`logistic`](https://www.cvxgrp.org/CVXR/reference/logistic.html)`(``margins_p``)``)`` ``+`\
`                            ``(``lam`` ``/`` ``2``)`` ``*`` `[`sum_squares`](https://www.cvxgrp.org/CVXR/reference/sum_squares.html)`(``beta_var``)``)``)``,`\
`           solver ``=`` ``"CLARABEL"``)``)``)`

    ## [1] 1932.792

\
`beta_central`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``beta_var``)``)`\
[`names`](https://rdrr.io/r/base/names.html)`(``beta_central``)`` ``<-`` `[`names`](https://rdrr.io/r/base/names.html)`(``beta_true``)`

## The $`\sigma`$ sweep

Six $`\sigma`$ values from zero to one. The $`\sigma = 0`$ row is the
sanity check that the DP mechanism is a no-op when off.

\
`sigma_grid``    ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1e-4``, ``1e-3``, ``1e-2``, ``1e-1``, ``1``)`\
`sweep_results`` ``<-`` `[`vector`](https://rdrr.io/r/base/vector.html)`(``"list"``, `[`length`](https://rdrr.io/r/base/length.html)`(``sigma_grid``)``)`\
`for`` ``(``j`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``sigma_grid``)``)`` ``{`\
`    ``sweep_results``[[``j``]``]`` ``<-`` ``run_dp_admm``(``sigma ``=`` ``sigma_grid``[``j``]``, seed ``=`` ``100L`` ``+`` ``j``)`\
`}`\
[`names`](https://rdrr.io/r/base/names.html)`(``sweep_results``)`` ``<-`` `[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"sigma=%.0e"``, ``sigma_grid``)`

## $`\sigma = 0`$ sanity check

\
`clean_dev`` ``<-`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``sweep_results``[[``1``]``]``$``z`` ``-`` ``beta_central``)``)`\
`agree_tol`` ``<-`` ``10`` ``*`` ``tol`\
`if`` ``(``clean_dev`` ``>`` ``agree_tol``)`\
`    `[`stop`](https://rdrr.io/r/base/stop.html)`(``"DP-ADMM at sigma = 0 disagrees with the centralized fit."``)`\
[`cat`](https://rdrr.io/r/base/cat.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"Sigma = 0 max coefficient deviation: %.2e (tol %.0e)\n"``,`\
`            ``clean_dev``, ``agree_tol``)``)`

    ## Sigma = 0 max coefficient deviation: 2.96e-05 (tol 1e-02)

## Summary table

\
`summary_df`` ``<-`` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`seq_along`](https://rdrr.io/r/base/seq.html)`(``sigma_grid``)``, ``function``(``j``)`` ``{`\
`    ``z`` ``<-`` ``sweep_results``[[``j``]``]``$``z`\
`    `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``sigma     ``=`` ``sigma_grid``[``j``]``,`\
`               intercept ``=`` ``z``[``1``]``,`\
`               age       ``=`` ``z``[``2``]``,`\
`               bmi       ``=`` ``z``[``3``]``,`\
`               sex       ``=`` ``z``[``4``]``,`\
`               max_dev   ``=`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z`` ``-`` ``beta_central``)``)``)`\
`}``)``)`\
`central_row`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``sigma ``=`` ``NA``, intercept ``=`` ``beta_central``[``1``]``,`\
`                         age ``=`` ``beta_central``[``2``]``, bmi ``=`` ``beta_central``[``3``]``,`\
`                         sex ``=`` ``beta_central``[``4``]``, max_dev ``=`` ``0``)`\
`summary_table`` ``<-`` `[`rbind`](https://rdrr.io/r/base/cbind.html)`(``summary_df``, ``central_row``)`\
[`rownames`](https://rdrr.io/r/base/colnames.html)`(``summary_table``)`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"sigma=%g"``, ``sigma_grid``)``, ``"centralized"``)`\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``summary_table``, digits ``=`` ``6``,`\
`             caption ``=`` ``"DP-ADMM coefficients vs centralized CVXR"``)`

|              | sigma | intercept |      age |       bmi |      sex |  max_dev |
|:-------------|------:|----------:|---------:|----------:|---------:|---------:|
| sigma=0      | 0e+00 | -0.591081 | 0.402672 | -0.325981 | 0.641534 | 0.000030 |
| sigma=0.0001 | 1e-04 | -0.591148 | 0.402593 | -0.326072 | 0.641677 | 0.000113 |
| sigma=0.001  | 1e-03 | -0.589677 | 0.401934 | -0.325976 | 0.642103 | 0.001427 |
| sigma=0.01   | 1e-02 | -0.619390 | 0.398715 | -0.323019 | 0.648941 | 0.028286 |
| sigma=0.1    | 1e-01 | -0.617981 | 0.428505 | -0.372438 | 0.754536 | 0.112973 |
| sigma=1      | 1e+00 | -3.460462 | 2.788708 |  2.514526 | 2.062808 | 2.869358 |
| centralized  |    NA | -0.591104 | 0.402674 | -0.325983 | 0.641564 | 0.000000 |

DP-ADMM coefficients vs centralized CVXR {.table style="width:100%;"}

The deviation grows roughly linearly with $`\sigma`$ in the
small-$`\sigma`$ regime and then leaves the linear band — at
$`\sigma = 1`$ the recovered coefficients drift far from the centralized
fit. The protocol is faithfully running the same algorithm at every
$`\sigma`$; the deterioration is what the noise mechanism does to the
optimization, not what the cryptographic channel does.

## Privacy budget

Per-iteration zCDP: $`\rho_{\text{iter}} = (\Delta/\sigma)^2/2`$ per
*site* per coordinate. Across $`T = 28`$ iterations and $`N = 3`$ sites,
total $`\rho = T \cdot N \cdot (\Delta/\sigma)^2/2`$. Convert to
$`(\varepsilon, \delta)`$ via the standard formula.

\
`zcdp_to_eps`` ``<-`` ``function``(``rho``, ``delta`` ``=`` ``1e-5``)`` ``rho`` ``+`` ``2`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``rho`` ``*`` `[`log`](https://rdrr.io/r/base/Log.html)`(``1`` ``/`` ``delta``)``)`\
\
`budget`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``sigma ``=`` ``sigma_grid``[``sigma_grid`` ``>`` ``0``]``)`\
`budget``$``rho_total``                  ``<-`` ``T_fixed`` ``*`` ``N`` ``*`` ``(``1`` ``/`` ``budget``$``sigma``)``^``2`` ``/`` ``2`\
`budget``$``epsilon_at_delta_1e_minus_5`` ``<-`` ``zcdp_to_eps``(``budget``$``rho_total``)`\
`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``budget``, digits ``=`` ``4``,`\
`             caption ``=`` ``"zCDP composition; sensitivity Delta = 1, delta = 1e-5"``)`

| sigma | rho_total | epsilon_at_delta_1e_minus_5 |
|------:|----------:|----------------------------:|
| 1e-04 |   4.2e+09 |                4.200440e+09 |
| 1e-03 |   4.2e+07 |                4.204398e+07 |
| 1e-02 |   4.2e+05 |                4.243979e+05 |
| 1e-01 |   4.2e+03 |                4.639792e+03 |
| 1e+00 |   4.2e+01 |                8.597920e+01 |

zCDP composition; sensitivity Delta = 1, delta = 1e-5 {.table}

This total is the whole of the procedure’s data-dependent exposure,
which is the point of having chosen $`\rho`$ and $`T`$ off the
surrogate: the selection step contributes nothing to it, because it read
nothing. The one quantity still taken on faith is the sensitivity
$`\Delta = 1`$, and that is flagged as a placeholder in the limitations
below — an honest budget with one declared placeholder, rather than a
budget whose own $`T`$ came from an unbilled read.

Read `epsilon_at_delta_1e_minus_5`. The smallest $`\varepsilon`$
attained — at $`\sigma = 1`$ where the fit is already broken — is still
in the dozens. Whether large $`\varepsilon`$ is acceptable is a use-case
decision; we report the numbers and stop.

## What this demonstrates

1.  **The cryptographic protocol composes cleanly with output DP.** At
    $`\sigma = 0`$ the DP-ADMM protocol reproduces the lossless ADMM
    fit. The DP layer is one
    [`rnorm()`](https://rdrr.io/r/stats/Normal.html) call per site per
    iteration and zero new homomorpheR machinery.
2.  **The accuracy/privacy trade-off is reported end-to-end.** Six
    $`\sigma`$ values, one fixed-$`T`$ DP-ADMM run each, summary table
    showing the recovered coefficients and the deviation from the
    centralized fit.
3.  **At noise scales the optimizer tolerates, $`\varepsilon`$ is
    large.** zCDP composition over $`T \cdot N`$ Gaussian releases
    inflates the budget; tightening it requires either fewer iterations,
    tighter sensitivity, or subsampling amplification — all out of scope
    for this demonstration.
4.  **Hyperparameter selection is part of the budget, or it is outside
    the guarantee.** The step that picks $`\rho`$ and $`T`$ looks like
    setup rather than analysis, which is exactly why it escapes
    scrutiny; here it also *sets the multiplier on the budget*. Making
    it data-independent keeps the accounting closed and costs utility
    instead. Composing a cryptographic channel with a DP guarantee does
    not make that step go away — encryption hides intermediates, not
    choices.

## Limitations

- **Tight sensitivity bounds** for the consensus update. $`\Delta = 1`$
  is a placeholder.
- **Paying for the selection instead of avoiding it.** Report-noisy-max
  or the exponential mechanism over the $`\rho`$ grid would let the
  cohort inform the choice for a declared cost. That needs a sensitivity
  bound on iterations-to-convergence under a one-record change, which we
  do not have.
- **Surrogate quality.** A surrogate that misjudges the cohort’s
  curvature costs accuracy — a worse $`\rho`$, or a $`T`$ too short to
  converge — and there is no way to detect that from inside the protocol
  without spending budget.
- **Adaptive noise** schedules across iterations.
- **Subsampling amplification** of the DP accountant.
- **Malicious-site protection.** A hostile site can corrupt either its
  $`(x_i + u_i)`$ or its noise draw; out of scope.

Readers who need the lossless fit should use
[`vignette("cvxr-consensus-admm")`](https://bnaras.github.io/homomorpheR/articles/cvxr-consensus-admm.md)
and accept the trajectory exposure that comes with it.

## References

- Bun & Steinke (2016). *Concentrated Differential Privacy.* TCC.
- Cyffers, Bellet & Upadhyay (2023). *Muffliato: Peer-to-Peer Privacy
  Amplification for Decentralized Optimisation and Averaging.* The
  DP-ADMM analytic companion to this empirical demonstration.
