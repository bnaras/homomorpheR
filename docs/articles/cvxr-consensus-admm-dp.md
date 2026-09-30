# Consensus ADMM + Output Differential Privacy (Demonstration)

## Introduction

This vignette is a demonstration. It fits an L2-regularized logistic
regression across \\N\\ sites by consensus ADMM under threshold FHE, the
protocol
[`vignette("cvxr-cox-lasso-dlbcl")`](https://bnaras.github.io/homomorpheR/articles/cvxr-cox-lasso-dlbcl.md)
uses for the Cox lasso. Each site solves its local problem with CVXR,
and the aggregator decrypts only the average of the sites’ encrypted
vectors.

## The problem and its consensus split

With \\N\\ sites, local data \\(X_i, y_i)\\ at site \\i\\, and a shared
coefficient \\x \in \mathbb{R}^p\\, the global problem is

\\ \min\_{x \in \mathbb{R}^p} \sum\_{i=1}^{N} \ell_i(x; X_i, y_i) +
\frac{\lambda}{2}\\ \lVert x \rVert_2^2 \\

where \\\ell_i\\ is the logistic loss on site \\i\\. The standard
consensus split (Boyd, Parikh, Chu, Peleato, Eckstein, 2011) introduces
local copies \\x_i \in \mathbb{R}^p\\ and a single global consensus \\z
\in \mathbb{R}^p\\:

\\ \min\_{\\x_i\\, z} \sum_i \ell_i(x_i; X_i, y_i) + \frac{\lambda}{2}\\
\lVert z \rVert_2^2 \quad \text{s.t.}\quad x_i = z, \\ \forall i. \\

The augmented-Lagrangian iteration is:

\\ \begin{aligned} x_i^{k+1} &= \arg\min\_{x_i}\\ \ell_i(x_i) +
\frac{\lambda}{2N}\lVert x_i\rVert_2^2 + \frac{\rho}{2}\lVert x_i -
z^k + u_i^k\rVert_2^2, \\ z^{k+1} &= \frac{1}{N}\sum_i (x_i^{k+1} +
u_i^k), \\ u_i^{k+1} &= u_i^k + (x_i^{k+1} - z^{k+1}). \end{aligned} \\

The \\x\\-update is local at each site; the \\z\\-update is the
consensus average that has to traverse the encrypted channel; the
\\u\\-update is local again. Only the \\z\\-update needs cryptography.

## Adding noise

Here we ask what happens if each site also adds noise, so that the
released iterates satisfy *output differential privacy*. Adding the
noise takes one extra [`rnorm()`](https://rdrr.io/r/stats/Normal.html)
call per site per iteration. With this optimizer, the fits are poor at
any noise level that gives a small \\\varepsilon\\. The vignette runs
the fits and reports the numbers.

## A brief output-DP primer

Given a query \\f: \mathcal{D} \to \mathbb{R}^p\\, the *Gaussian
mechanism* releases \\\tilde f(D) = f(D) + \mathcal{N}(0, \sigma^2 I)\\.
Sensitivity-driven \\\sigma\\ gives single-query \\(\varepsilon,
\delta)\\ DP. Multi-query composition uses *zCDP* (Bun and Steinke
2016): each release is \\(\Delta/\sigma)^2/2\\-zCDP; \\T\\ releases
compose linearly to \\T \cdot \rho\\; convert back to \\(\varepsilon,
\delta)\\ via \\\varepsilon = \rho + 2\sqrt{\rho \log(1/\delta)}\\.

## The protocol modification

Without noise, site \\i\\ encrypts \\x_i + u_i\\ and the aggregator
decrypts only their average \\z\\. The change here is that at every ADMM
iteration site \\i\\ adds an independent draw \\\eta_i \sim
\mathcal{N}(0, \sigma^2 N \cdot I)\\ to \\x_i + u_i\\ *before*
encrypting. The encrypted noise terms sum under the joint key, the
\\1/N\\ scaling contracts the variance back to \\\sigma^2\\ per
coordinate, and the recovered \\z\\ has noise \\\mathcal{N}(0, \sigma^2
I)\\.

As in `cox-threshold-dp`, each site draws its own noise, so only site
\\i\\ sees its noiseless \\x_i + u_i\\. The aggregator sees encrypted
noisy contributions and, after decryption, their noisy average.

In the code below, `site_contribution_dp()` adds the noise and encrypts
inside the site, so `encrypted_consensus_dp()` receives only encrypted
values. If the aggregator added the noise instead, the numbers would be
the same, but the aggregator would see each site’s noiseless \\x_i +
u_i\\.

## The stopping rule changes

Under noise the residuals do not shrink below the per-iteration noise,
so the number of iterations \\T\\ is *fixed in advance*. Stopping on the
residuals would also make \\T\\ depend on the data, and \\T\\ multiplies
the privacy budget below. The next section picks \\\rho\\ and \\T\\
without using the cohort.

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

## Choosing \\\rho\\ and \\T\\ without touching the cohort

Tuning \\\rho\\ on the real data would itself be a release: the chosen
\\\rho\\ and \\T\\ depend on the records, and the budget below counts
only the \\T\\ Gaussian releases. So the sweep runs on a *surrogate
cohort* built only from facts the protocol already treats as public: the
number of sites, their approximate sizes, and the covariate schema. The
effect sizes are nominal values fixed in the analysis plan. No record
from any site enters it, so the sweep needs no encryption.

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
`show_rho_sweep``(``rho_sweep``)`\
\
`converged_rows`` ``<-`` ``rho_sweep``[``rho_sweep``$``converged``, ``]`\
`if`` ``(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``converged_rows``)`` ``==`` ``0L``)`\
`    `[`stop`](https://rdrr.io/r/base/stop.html)`(``"No rho in the grid converged within max_iter on the surrogate."``)`\
\
`rho_chosen`` ``<-`` ``converged_rows``$``rho``[`[`which.min`](https://rdrr.io/r/base/which.min.html)`(``converged_rows``$``iters``)``]`\
`T_fixed``    ``<-`` ``converged_rows``$``iters``[``converged_rows``$``rho`` ``==`` ``rho_chosen``]`

| \\\rho\\ | Iterations | Converged |
|---------:|-----------:|:----------|
|       10 |         60 | FALSE     |
|       20 |         60 | FALSE     |
|       50 |         33 | TRUE      |
|      100 |         28 | TRUE      |
|      500 |         60 | FALSE     |

Consensus-ADMM convergence on the surrogate cohort {.table .table
.table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

The value of \\\rho\\ with the fewest iterations to convergence is
\\\rho = 100\\, with \\T = 28\\. Both are now fixed. The DP-ADMM loop
below runs exactly \\T = 28\\ iterations, whatever the residuals do.

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
`## Site-side: the site draws its own noise, adds it, and encrypts with`\
`## the public parameters it received at setup, all before anything`\
`## leaves the site. The noiseless x_i + u_i never leaves.`\
`site_contribution_dp`` ``<-`` ``function``(``site``, ``sigma``, ``Nv``)`` ``{`\
`    ``st`` ``<-`` ``site``@``state`\
`    ``noised`` ``<-`` ``st``$``x_curr`` ``+`` ``st``$``u_curr`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``p``, mean ``=`` ``0``, sd ``=`` ``sigma`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``Nv``)``)`\
`    `[`encrypt`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)`(``site``, ``noised``)`\
`}`\
\
`## Aggregator-side: sum the encrypted values, scale, threshold-decrypt. The`\
`## 1/N scaling contracts the summed noise variance back to sigma^2.`\
`encrypted_consensus_dp`` ``<-`` ``function``(``threshold_master``, ``sites``, ``sigma``)`` ``{`\
`    ``Nv``  ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(``sites``)`\
`    ``cts`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``site_contribution_dp``, sigma ``=`` ``sigma``, Nv ``=`` ``Nv``)`\
`    ``ct_avg`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``cts``)`` ``*`` ``(``1`` ``/`` ``Nv``)`\
`    `[`decrypt`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)`(``threshold_master``, ``ct_avg``, len ``=`` ``p``)`\
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

This fit pools the raw data, so it is not part of the protocol. We use
it only to measure how far the noise moves the answer, in the last
column of the table below. It is not released, so it is not charged to
the budget.

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

## The \\\sigma\\ sweep

Six \\\sigma\\ values from zero to one. The \\\sigma = 0\\ row checks
that the protocol without noise matches the centralized fit.

\
`sigma_grid``    ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1e-4``, ``1e-3``, ``1e-2``, ``1e-1``, ``1``)`\
`sweep_results`` ``<-`` `[`vector`](https://rdrr.io/r/base/vector.html)`(``"list"``, `[`length`](https://rdrr.io/r/base/length.html)`(``sigma_grid``)``)`\
`for`` ``(``j`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``sigma_grid``)``)`` ``{`\
`    ``sweep_results``[[``j``]``]`` ``<-`` ``run_dp_admm``(``sigma ``=`` ``sigma_grid``[``j``]``, seed ``=`` ``100L`` ``+`` ``j``)`\
`}`\
[`names`](https://rdrr.io/r/base/names.html)`(``sweep_results``)`` ``<-`` `[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"sigma=%.0e"``, ``sigma_grid``)`

\
`clean_dev`` ``<-`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``sweep_results``[[``1``]``]``$``z`` ``-`` ``beta_central``)``)`\
`agree_tol`` ``<-`` ``10`` ``*`` ``tol`\
`if`` ``(``clean_dev`` ``>`` ``agree_tol``)`\
`    `[`stop`](https://rdrr.io/r/base/stop.html)`(``"DP-ADMM at sigma = 0 disagrees with the centralized fit."``)`

At \\\sigma = 0\\ the largest coefficient deviation from the centralized
fit is 2.91 × 10⁻⁵, within \\10 \times\\ the ADMM tolerance of 0.001.

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
`show_summary``(``summary_table``)`

| \\\sigma\\ | intercept | age | bmi | sex | \\\max_j \lvert \hat z_j - \hat\beta_j^{\text{centralized}} \rvert\\ |
|:---|---:|---:|---:|---:|---:|
| \\0\\ | -0.591081 | 0.402672 | -0.325981 | 0.641535 | \\2.91 \times 10^{-5}\\ |
| \\10^{-4}\\ | -0.591148 | 0.402593 | -0.326072 | 0.641677 | \\1.132 \times 10^{-4}\\ |
| \\10^{-3}\\ | -0.589677 | 0.401934 | -0.325976 | 0.642103 | \\1.427 \times 10^{-3}\\ |
| \\10^{-2}\\ | -0.619390 | 0.398715 | -0.323019 | 0.648941 | \\0.02829\\ |
| \\10^{-1}\\ | -0.617981 | 0.428505 | -0.372438 | 0.754537 | \\0.113\\ |
| \\1\\ | -3.460462 | 2.788708 | 2.514526 | 2.062808 | \\2.869\\ |
| centralized | -0.591104 | 0.402674 | -0.325983 | 0.641564 | \\0\\ |

DP-ADMM coefficients vs the centralized CVXR fit {.table .table
.table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

Fidelity decays monotonically as expected.

## Privacy budget

Each iteration releases one noisy average \\z\\, with noise
\\\mathcal{N}(0, \sigma^2 I)\\. Let \\\Delta\\ be the largest change in
\\z\\ (L2 norm over all \\p\\ coordinates) from adding or removing one
record. Each record sits at one site, and the aggregator decrypts only
the average, so one iteration is a single Gaussian release with
\\\rho\_{\text{iter}} = (\Delta/\sigma)^2/2\\. Over \\T = 28\\
iterations the total is \\\rho = T \cdot (\Delta/\sigma)^2/2\\, whatever
the number of sites. This is the same accounting as `cox-threshold-dp`.
With sensitivity \\\Delta = 1\\ (placeholder) and target \\\delta =
10^{-5}\\, zCDP composition gives:

\
`zcdp_to_eps`` ``<-`` ``function``(``rho``, ``delta`` ``=`` ``1e-5``)`` ``rho`` ``+`` ``2`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``rho`` ``*`` `[`log`](https://rdrr.io/r/base/Log.html)`(``1`` ``/`` ``delta``)``)`\
\
`budget`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``sigma ``=`` ``sigma_grid``[``sigma_grid`` ``>`` ``0``]``)`\
`budget``$``rho_total``                  ``<-`` ``T_fixed`` ``*`` ``(``1`` ``/`` ``budget``$``sigma``)``^``2`` ``/`` ``2`\
`budget``$``epsilon_at_delta_1e_minus_5`` ``<-`` ``zcdp_to_eps``(``budget``$``rho_total``)`\
`show_budget``(``budget``, ``T_fixed``)`

| \\\sigma\\ | \\\rho\_{\text{total}} = 28\\\rho\\ | \\\varepsilon\\ at \\\delta = 10^{-5}\\ |
|:---|---:|---:|
| \\10^{-4}\\ | \\1.4 \times 10^{9}\\ | \\1.4 \times 10^{9}\\ |
| \\10^{-3}\\ | \\1.4 \times 10^{7}\\ | \\1.403 \times 10^{7}\\ |
| \\10^{-2}\\ | \\1.4 \times 10^{5}\\ | \\1.425 \times 10^{5}\\ |
| \\10^{-1}\\ | \\1400\\ | \\1654\\ |
| \\1\\ | \\14\\ | \\39.39\\ |

zCDP composition; sensitivity \\\Delta = 1\\, target \\\delta =
10^{-5}\\ {.table .table .table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

The smallest \\\varepsilon\\, at \\\sigma = 1\\ where the fit is already
poor, is 39. Whether that is acceptable depends on the application.

One could also explore a tighter sensitivity bound \\\Delta\\, or pay
for the choice of \\\rho\\ and \\T\\ with a DP selection mechanism, etc.
We don’t do that here.

## References

Bun, Mark, and Thomas Steinke. 2016. “Concentrated Differential Privacy:
Simplifications, Extensions, and Lower Bounds.” *Theory of Cryptography
Conference (TCC)*, 635–58.
<https://doi.org/10.1007/978-3-662-53641-4_24>.
