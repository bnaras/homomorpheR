# Consensus ADMM + Output Differential Privacy (Demonstration)

## Introduction

This vignette is a **demonstration**, not a recommendation.
[`vignette("cvxr-consensus-admm")`](https://bnaras.github.io/homomorpheR/articles/cvxr-consensus-admm.md)
shows the lossless consensus-ADMM protocol over the threshold-FHE
channel: bit-identical to the centralized CVXR fit, no single decrypter,
residuals drive convergence. The aggregator sees the full trajectory
$`\{z^k\}`$ as plaintext after each iteration’s threshold fusion.

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
    aggregator is compromised it sees only ciphertexts of *noised*
    contributions until the final fusion.
2.  The trust model matches `cox-threshold-dp`: each site is its own
    randomness boundary.

## The stopping rule changes

Residual-based convergence checks are not meaningful under noise: the
residuals never shrink below the per-iteration noise floor. We replace
residual-based stopping with **fixed $`T`$** equal to the cleartext
convergence iteration count for the chosen $`\rho`$. The $`\sigma = 0`$
row of the table below verifies that the protocol with the noise
mechanism turned off lands on the lossless ADMM fit at the same fixed
$`T`$.

## Setup

[`suppressPackageStartupMessages`](https://rdrr.io/r/base/message.html)`(``{`` `` `[`library`](https://rdrr.io/r/base/library.html)`(`[`homomorpheR`](https://bnaras.github.io/homomorpheR/)`)`` `` `[`library`](https://rdrr.io/r/base/library.html)`(`[`CVXR`](https://cvxr.rbind.io)`)`` `` `[`library`](https://rdrr.io/r/base/library.html)`(`[`S7`](https://rconsortium.github.io/S7/)`)`` ``}``)`` `` ``N`` ``<-`` ``3L`` ``p`` ``<-`` ``4L`` ``lam`` ``<-`` ``1`

`build_local_problem`` ``<-`` ``function``(``X_i``, ``y_i``, ``rho_val``)`` ``{`` `` ``x`` ``<-`` `[`Variable`](https://www.cvxgrp.org/CVXR/reference/Variable.html)`(``p``)`` `` ``zp`` ``<-`` `[`Parameter`](https://www.cvxgrp.org/CVXR/reference/Parameter.html)`(``p``)`` `` ``up`` ``<-`` `[`Parameter`](https://www.cvxgrp.org/CVXR/reference/Parameter.html)`(``p``)`` `` ``y_signs`` ``<-`` ``2`` ``*`` ``y_i`` ``-`` ``1`` `` ``margins`` ``<-`` ``-``y_signs`` ``*`` ``(``X_i`` `[`%*%`](https://rdrr.io/r/base/matmult.html)` ``x``)`` `` ``local_loss`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`logistic`](https://www.cvxgrp.org/CVXR/reference/logistic.html)`(``margins``)``)`` ``+`` `` ``(``lam`` ``/`` ``(``2`` ``*`` ``N``)``)`` ``*`` `[`sum_squares`](https://www.cvxgrp.org/CVXR/reference/sum_squares.html)`(``x``)`` `` ``augmented`` ``<-`` ``(``rho_val`` ``/`` ``2``)`` ``*`` `[`sum_squares`](https://www.cvxgrp.org/CVXR/reference/sum_squares.html)`(``x`` ``-`` ``zp`` ``+`` ``up``)`` `` ``prob`` ``<-`` `[`Problem`](https://www.cvxgrp.org/CVXR/reference/Problem.html)`(`[`Minimize`](https://www.cvxgrp.org/CVXR/reference/Minimize.html)`(``local_loss`` ``+`` ``augmented``)``)`` `` `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``zp``)`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p``)``; `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``up``)`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p``)`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``prob ``=`` ``prob``, x ``=`` ``x``, zp ``=`` ``zp``, up ``=`` ``up``)`` ``}`` `` ``ConsensusSite`` ``<-`` `[`new_class`](https://rconsortium.github.io/S7/reference/new_class.html)`(``"ConsensusSite"``,`` `` properties ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` name ``=`` ``class_character``,`` `` n ``=`` ``class_integer``,`` `` state ``=`` ``class_any``)``)`` `` ``make_consensus_site`` ``<-`` ``function``(``name``, ``X_i``, ``y_i``, ``rho_val``)`` ``{`` `` ``st`` ``<-`` `[`new.env`](https://rdrr.io/r/base/environment.html)`(``parent ``=`` `[`emptyenv`](https://rdrr.io/r/base/environment.html)`(``)``)`` `` ``st``$``X`` ``<-`` ``X_i`` `` ``st``$``y`` ``<-`` ``y_i`` `` ``built`` ``<-`` ``build_local_problem``(``X_i``, ``y_i``, ``rho_val``)`` `` ``st``$``prob`` ``<-`` ``built``$``prob`` `` ``st``$``x_var`` ``<-`` ``built``$``x`` `` ``st``$``zp`` ``<-`` ``built``$``zp`` `` ``st``$``up`` ``<-`` ``built``$``up`` `` ``st``$``x_curr`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``X_i``)``)`` `` ``st``$``u_curr`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``X_i``)``)`` `` ``ConsensusSite``(``name ``=`` ``name``, n ``=`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``X_i``)``, state ``=`` ``st``)`` ``}`` `` ``local_update`` ``<-`` ``function``(``site``, ``z_curr``)`` ``{`` `` ``st`` ``<-`` ``site``@``state`` `` `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``st``$``zp``)`` ``<-`` ``z_curr`` `` `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``st``$``up``)`` ``<-`` ``st``$``u_curr`` `` `[`suppressMessages`](https://rdrr.io/r/base/message.html)`(`[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(`[`psolve`](https://www.cvxgrp.org/CVXR/reference/psolve.html)`(``st``$``prob``, solver ``=`` ``"CLARABEL"``)``)``)`` `` ``if`` ``(``!`[`status`](https://www.cvxgrp.org/CVXR/reference/status.html)`(``st``$``prob``)`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"optimal"``, ``"optimal_inaccurate"``)``)`` `` `[`stop`](https://rdrr.io/r/base/stop.html)`(``"Local CVXR solve at "``, ``site``@``name``, ``" did not reach optimal status."``)`` `` ``st``$``x_curr`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``st``$``x_var``)``)`` `` `[`invisible`](https://rdrr.io/r/base/invisible.html)`(``st``$``x_curr``)`` ``}`

## Simulated cohort

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``20260412``)`` ``n_per_site`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``500L``, ``1000L``, ``1500L``)`` ``beta_true`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``intercept ``=`` ``-``0.5``, age ``=`` ``0.4``, bmi ``=`` ``-``0.3``, sex ``=`` ``0.6``)`` `` ``make_site_data`` ``<-`` ``function``(``n``)`` ``{`` `` ``X`` ``<-`` `[`cbind`](https://rdrr.io/r/base/cbind.html)`(``1``, `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)``, `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)``, `[`rbinom`](https://rdrr.io/r/stats/Binomial.html)`(``n``, ``1``, ``0.5``)``)`` `` ``pr`` ``<-`` `[`plogis`](https://rdrr.io/r/stats/Logistic.html)`(`[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``X`` `[`%*%`](https://rdrr.io/r/base/matmult.html)` ``beta_true``)``)`` `` ``y`` ``<-`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(`[`runif`](https://rdrr.io/r/stats/Uniform.html)`(``n``)`` ``<`` ``pr``)`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``X ``=`` ``X``, y ``=`` ``y``)`` ``}`` ``site_data`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``n_per_site``, ``make_site_data``)`

## Cleartext $`\rho`$ sweep

Pick $`\rho`$ programmatically — same idiom as the lossless ADMM
vignette.

`tol`` ``<-`` ``1e-3`` ``max_iter`` ``<-`` ``60L`` `` ``sweep_one_rho`` ``<-`` ``function``(``rho_val``)`` ``{`` `` ``built`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``site_data``,`` `` ``function``(``s``)`` ``build_local_problem``(``s``$``X``, ``s``$``y``, ``rho_val``)``)`` `` ``x_curr`` ``<-`` ``u_curr`` ``<-`` `[`replicate`](https://rdrr.io/r/base/lapply.html)`(``N``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p``)``, simplify ``=`` ``FALSE``)`` `` ``z`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p``)`` `` ``k_conv`` ``<-`` ``NA_integer_`` `` ``for`` ``(``k`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``max_iter``)``)`` ``{`` `` ``for`` ``(``i`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``N``)``)`` ``{`` `` `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``built``[[``i``]``]``$``zp``)`` ``<-`` ``z`` `` `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``built``[[``i``]``]``$``up``)`` ``<-`` ``u_curr``[[``i``]``]`` `` `[`suppressMessages`](https://rdrr.io/r/base/message.html)`(`[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(`` `` `[`psolve`](https://www.cvxgrp.org/CVXR/reference/psolve.html)`(``built``[[``i``]``]``$``prob``, solver ``=`` ``"CLARABEL"``)``)``)`` `` ``x_curr``[[``i``]``]`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``built``[[``i``]``]``$``x``)``)`` `` ``}`` `` ``z_prev`` ``<-`` ``z`` `` ``z`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`Map`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``x_curr``, ``u_curr``)``)`` ``/`` ``N`` `` ``for`` ``(``i`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``N``)``)`` ``u_curr``[[``i``]``]`` ``<-`` ``u_curr``[[``i``]``]`` ``+`` ``x_curr``[[``i``]``]`` ``-`` ``z`` `` ``pri`` ``<-`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`sum`](https://rdrr.io/r/base/sum.html)`(`[`vapply`](https://rdrr.io/r/base/lapply.html)`(`[`seq_len`](https://rdrr.io/r/base/seq.html)`(``N``)``,`` `` ``function``(``i``)`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``(``x_curr``[[``i``]``]`` ``-`` ``z``)``^``2``)``, ``0``)``)`` ``/`` ``N``)`` `` ``dua`` ``<-`` ``rho_val`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`sum`](https://rdrr.io/r/base/sum.html)`(``(``z`` ``-`` ``z_prev``)``^``2``)``)`` `` ``if`` ``(``pri`` ``<`` ``tol`` ``&&`` ``dua`` ``<`` ``tol``)`` ``{`` ``k_conv`` ``<-`` ``k``; ``break`` ``}`` `` ``}`` `` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``rho ``=`` ``rho_val``,`` `` iters ``=`` ``if`` ``(`[`is.na`](https://rdrr.io/r/base/NA.html)`(``k_conv``)``)`` ``max_iter`` ``else`` ``k_conv``,`` `` converged ``=`` ``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``k_conv``)``)`` ``}`` `` ``rho_grid`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``10``, ``20``, ``50``, ``100``, ``500``)`` ``rho_sweep`` ``<-`` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``rho_grid``, ``sweep_one_rho``)``)`` ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``rho_sweep``, caption ``=`` ``"Cleartext consensus-ADMM convergence"``)`

| rho | iters | converged |
|----:|------:|:----------|
|  10 |    60 | FALSE     |
|  20 |    59 | TRUE      |
|  50 |    30 | TRUE      |
| 100 |    30 | TRUE      |
| 500 |    60 | FALSE     |

Cleartext consensus-ADMM convergence {.table}

`converged_rows`` ``<-`` ``rho_sweep``[``rho_sweep``$``converged``, ``]`` ``if`` ``(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``converged_rows``)`` ``==`` ``0L``)`` `` `[`stop`](https://rdrr.io/r/base/stop.html)`(``"No rho in the grid converged within max_iter."``)`` `` ``rho_chosen`` ``<-`` ``converged_rows``$``rho``[`[`which.min`](https://rdrr.io/r/base/which.min.html)`(``converged_rows``$``iters``)``]`` ``T_fixed`` ``<-`` ``converged_rows``$``iters``[``converged_rows``$``rho`` ``==`` ``rho_chosen``]`` `[`cat`](https://rdrr.io/r/base/cat.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"Chosen rho = %g (T = %d iterations cleartext).\n"``,`` `` ``rho_chosen``, ``T_fixed``)``)`

    ## Chosen rho = 50 (T = 30 iterations cleartext).

The DP-ADMM loop below runs for exactly $`T = 30`$ iterations regardless
of residuals.

## Threshold-FHE setup

`cc`` ``<-`` ``openfhe.R``::`[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`` `` multiplicative_depth ``=`` ``1L``,`` `` scaling_mod_size ``=`` ``59L``,`` `` first_mod_size ``=`` ``60L``,`` `` batch_size ``=`` ``8L``,`` `` features ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``openfhe.R``::`[`Feature`](https://openfheorg.github.io/openfhe.R/reference/Feature.html)`$``MULTIPARTY``)``)`

The DP version of the consensus step. The only change from the lossless
ADMM vignette’s `encrypted_consensus()` is the
`rnorm(p, ..., sd = sigma * sqrt(Nv))` term inside the per-site loop:

`encrypted_consensus_dp`` ``<-`` ``function``(``threshold_master``, ``sites``, ``sigma``)`` ``{`` `` ``Nv`` ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(``sites``)`` `` ``cts`` ``<-`` `[`vector`](https://rdrr.io/r/base/vector.html)`(``"list"``, ``Nv``)`` `` ``for`` ``(``i`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``sites``)``)`` ``{`` `` ``st`` ``<-`` ``sites``[[``i``]``]``@``state`` `` ``val`` ``<-`` ``st``$``x_curr`` ``+`` ``st``$``u_curr`` ``+`` `` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``p``, mean ``=`` ``0``, sd ``=`` ``sigma`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``Nv``)``)`` `` ``cts``[[``i``]``]`` ``<-`` `[`master_encrypt`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md)`(``threshold_master``, ``val``)`` `` ``}`` `` ``ct_sum`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``cts``)`` `` ``ct_avg`` ``<-`` ``ct_sum`` ``*`` ``(``1`` ``/`` ``Nv``)`` `` `[`master_decrypt`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)`(``threshold_master``, ``ct_avg``, len ``=`` ``p``)`` ``}`

## The DP-ADMM loop

`run_dp_admm`` ``<-`` ``function``(``sigma``, ``T_iter`` ``=`` ``T_fixed``, ``seed`` ``=`` ``NULL``)`` ``{`` `` ``if`` ``(``!`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``seed``)``)`` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``seed``)`` `` ``master`` ``<-`` `[`make_threshold_master`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)`(``"Aggregator"``,`` `` crypto_context ``=`` ``cc``,`` `` n_sites ``=`` ``N``)`` `` ``sites`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` ``make_consensus_site``(``"Site 1"``, ``site_data``[[``1``]``]``$``X``, ``site_data``[[``1``]``]``$``y``, ``rho_chosen``)``,`` `` ``make_consensus_site``(``"Site 2"``, ``site_data``[[``2``]``]``$``X``, ``site_data``[[``2``]``]``$``y``, ``rho_chosen``)``,`` `` ``make_consensus_site``(``"Site 3"``, ``site_data``[[``3``]``]``$``X``, ``site_data``[[``3``]``]``$``y``, ``rho_chosen``)``)`` `` `` ``z_curr`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p``)`` `` ``z_hist`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(``NA_real_``, nrow ``=`` ``T_iter``, ncol ``=`` ``p``,`` `` dimnames ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``NULL``, `[`names`](https://rdrr.io/r/base/names.html)`(``beta_true``)``)``)`` `` ``for`` ``(``k`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``T_iter``)``)`` ``{`` `` ``for`` ``(``s`` ``in`` ``sites``)`` ``local_update``(``s``, ``z_curr``)`` `` ``z_curr`` ``<-`` ``encrypted_consensus_dp``(``master``, ``sites``, ``sigma``)`` `` ``for`` ``(``s`` ``in`` ``sites``)`` ``{`` `` ``s``@``state``$``u_curr`` ``<-`` ``s``@``state``$``u_curr`` ``+`` ``(``s``@``state``$``x_curr`` ``-`` ``z_curr``)`` `` ``}`` `` ``z_hist``[``k``, ``]`` ``<-`` ``z_curr`` `` ``}`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``z ``=`` ``z_curr``, z_hist ``=`` ``z_hist``)`` ``}`

## Centralized CVXR fit

`X_pooled`` ``<-`` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``site_data``, ``` `[[` ```, ``"X"``)``)`` ``y_pooled`` ``<-`` `[`unlist`](https://rdrr.io/r/base/unlist.html)`(`[`lapply`](https://rdrr.io/r/base/lapply.html)`(``site_data``, ``` `[[` ```, ``"y"``)``)`` ``beta_var`` ``<-`` `[`Variable`](https://www.cvxgrp.org/CVXR/reference/Variable.html)`(``p``)`` ``y_signs_p`` ``<-`` ``2`` ``*`` ``y_pooled`` ``-`` ``1`` ``margins_p`` ``<-`` ``-``y_signs_p`` ``*`` ``(``X_pooled`` `[`%*%`](https://rdrr.io/r/base/matmult.html)` ``beta_var``)`` `[`suppressMessages`](https://rdrr.io/r/base/message.html)`(`[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(`` `` `[`psolve`](https://www.cvxgrp.org/CVXR/reference/psolve.html)`(`[`Problem`](https://www.cvxgrp.org/CVXR/reference/Problem.html)`(`[`Minimize`](https://www.cvxgrp.org/CVXR/reference/Minimize.html)`(`[`sum`](https://rdrr.io/r/base/sum.html)`(`[`logistic`](https://www.cvxgrp.org/CVXR/reference/logistic.html)`(``margins_p``)``)`` ``+`` `` ``(``lam`` ``/`` ``2``)`` ``*`` `[`sum_squares`](https://www.cvxgrp.org/CVXR/reference/sum_squares.html)`(``beta_var``)``)``)``,`` `` solver ``=`` ``"CLARABEL"``)``)``)`

    ## [1] 1932.792

`beta_central`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``beta_var``)``)`` `[`names`](https://rdrr.io/r/base/names.html)`(``beta_central``)`` ``<-`` `[`names`](https://rdrr.io/r/base/names.html)`(``beta_true``)`

## The $`\sigma`$ sweep

Six $`\sigma`$ values from zero to one. The $`\sigma = 0`$ row is the
sanity check that the DP mechanism is a no-op when off.

`sigma_grid`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1e-4``, ``1e-3``, ``1e-2``, ``1e-1``, ``1``)`` ``sweep_results`` ``<-`` `[`vector`](https://rdrr.io/r/base/vector.html)`(``"list"``, `[`length`](https://rdrr.io/r/base/length.html)`(``sigma_grid``)``)`` ``for`` ``(``j`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``sigma_grid``)``)`` ``{`` `` ``sweep_results``[[``j``]``]`` ``<-`` ``run_dp_admm``(``sigma ``=`` ``sigma_grid``[``j``]``, seed ``=`` ``100L`` ``+`` ``j``)`` ``}`` `[`names`](https://rdrr.io/r/base/names.html)`(``sweep_results``)`` ``<-`` `[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"sigma=%.0e"``, ``sigma_grid``)`

## $`\sigma = 0`$ sanity check

`clean_dev`` ``<-`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``sweep_results``[[``1``]``]``$``z`` ``-`` ``beta_central``)``)`` ``agree_tol`` ``<-`` ``10`` ``*`` ``tol`` ``if`` ``(``clean_dev`` ``>`` ``agree_tol``)`` `` `[`stop`](https://rdrr.io/r/base/stop.html)`(``"DP-ADMM at sigma = 0 disagrees with the centralized fit."``)`` `[`cat`](https://rdrr.io/r/base/cat.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"Sigma = 0 max coefficient deviation: %.2e (tol %.0e)\n"``,`` `` ``clean_dev``, ``agree_tol``)``)`

    ## Sigma = 0 max coefficient deviation: 8.65e-05 (tol 1e-02)

## Summary table

`summary_df`` ``<-`` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`seq_along`](https://rdrr.io/r/base/seq.html)`(``sigma_grid``)``, ``function``(``j``)`` ``{`` `` ``z`` ``<-`` ``sweep_results``[[``j``]``]``$``z`` `` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``sigma ``=`` ``sigma_grid``[``j``]``,`` `` intercept ``=`` ``z``[``1``]``,`` `` age ``=`` ``z``[``2``]``,`` `` bmi ``=`` ``z``[``3``]``,`` `` sex ``=`` ``z``[``4``]``,`` `` max_dev ``=`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z`` ``-`` ``beta_central``)``)``)`` ``}``)``)`` ``central_row`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``sigma ``=`` ``NA``, intercept ``=`` ``beta_central``[``1``]``,`` `` age ``=`` ``beta_central``[``2``]``, bmi ``=`` ``beta_central``[``3``]``,`` `` sex ``=`` ``beta_central``[``4``]``, max_dev ``=`` ``0``)`` ``summary_table`` ``<-`` `[`rbind`](https://rdrr.io/r/base/cbind.html)`(``summary_df``, ``central_row``)`` `[`rownames`](https://rdrr.io/r/base/colnames.html)`(``summary_table``)`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"sigma=%g"``, ``sigma_grid``)``, ``"centralized"``)`` ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``summary_table``, digits ``=`` ``6``,`` `` caption ``=`` ``"DP-ADMM coefficients vs centralized CVXR"``)`

|              | sigma | intercept |       age |       bmi |      sex |  max_dev |
|:-------------|------:|----------:|----------:|----------:|---------:|---------:|
| sigma=0      | 0e+00 | -0.591018 |  0.402617 | -0.325959 | 0.641609 | 0.000086 |
| sigma=0.0001 | 1e-04 | -0.591025 |  0.402669 | -0.325837 | 0.641469 | 0.000146 |
| sigma=0.001  | 1e-03 | -0.591691 |  0.401580 | -0.326374 | 0.643028 | 0.001464 |
| sigma=0.01   | 1e-02 | -0.605935 |  0.407191 | -0.318838 | 0.657241 | 0.015677 |
| sigma=0.1    | 1e-01 | -0.777970 |  0.401483 | -0.382908 | 0.519775 | 0.186865 |
| sigma=1      | 1e+00 | -0.461572 | -0.677927 | -0.499757 | 1.302194 | 1.080601 |
| centralized  |    NA | -0.591104 |  0.402674 | -0.325983 | 0.641564 | 0.000000 |

DP-ADMM coefficients vs centralized CVXR {.table style="width:100%;"}

The deviation grows roughly linearly with $`\sigma`$ in the
small-$`\sigma`$ regime and then leaves the linear band — at
$`\sigma = 1`$ the recovered coefficients drift far from the centralized
fit. The protocol is faithfully running the same algorithm at every
$`\sigma`$; the deterioration is what the noise mechanism does to the
optimization, not what the cryptographic channel does.

## Privacy budget

Per-iteration zCDP: $`\rho_{\text{iter}} = (\Delta/\sigma)^2/2`$ per
*site* per coordinate. Across $`T = 30`$ iterations and $`N = 3`$ sites,
total $`\rho = T \cdot N \cdot (\Delta/\sigma)^2/2`$. Convert to
$`(\varepsilon, \delta)`$ via the standard formula.

`zcdp_to_eps`` ``<-`` ``function``(``rho``, ``delta`` ``=`` ``1e-5``)`` ``rho`` ``+`` ``2`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``rho`` ``*`` `[`log`](https://rdrr.io/r/base/Log.html)`(``1`` ``/`` ``delta``)``)`` `` ``budget`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``sigma ``=`` ``sigma_grid``[``sigma_grid`` ``>`` ``0``]``)`` ``budget``$``rho_total`` ``<-`` ``T_fixed`` ``*`` ``N`` ``*`` ``(``1`` ``/`` ``budget``$``sigma``)``^``2`` ``/`` ``2`` ``budget``$``epsilon_at_delta_1e_minus_5`` ``<-`` ``zcdp_to_eps``(``budget``$``rho_total``)`` ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``budget``, digits ``=`` ``4``,`` `` caption ``=`` ``"zCDP composition; sensitivity Delta = 1, delta = 1e-5"``)`

| sigma | rho_total | epsilon_at_delta_1e_minus_5 |
|------:|----------:|----------------------------:|
| 1e-04 |   4.5e+09 |                4.500455e+09 |
| 1e-03 |   4.5e+07 |                4.504552e+07 |
| 1e-02 |   4.5e+05 |                4.545523e+05 |
| 1e-01 |   4.5e+03 |                4.955228e+03 |
| 1e+00 |   4.5e+01 |                9.052280e+01 |

zCDP composition; sensitivity Delta = 1, delta = 1e-5 {.table}

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

## Limitations

- **Tight sensitivity bounds** for the consensus update. $`\Delta = 1`$
  is a placeholder.
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
