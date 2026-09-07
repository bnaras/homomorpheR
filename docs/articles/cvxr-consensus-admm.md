# Federated Consensus ADMM with CVXR

## Introduction

The `cox` and `cox-threshold` vignettes show that *function-call*
optimizers — [`mle()`](https://rdrr.io/r/stats4/mle.html),
[`optim()`](https://rdrr.io/r/stats/optim.html), `coxph`’s
Newton-Raphson via callback — compose with FHE: hand them a callback
that performs an encrypted master/worker round, and the optimizer cannot
tell.

That pattern fits any objective with a sum-decomposable log-likelihood,
but it does not generalize to arbitrary convex programs. The richer R
toolkit for convex optimization — CVXR — operates on a *symbolic*
problem, not a callback. We cannot hand CVXR a function that secretly
does FHE on every call, because CVXR does not call our function: it
canonicalizes the problem once and ships the canonical form to a
numerical solver.

The replacement is **federated consensus ADMM**. We split the global
problem into per-site local subproblems each handled by CVXR in
cleartext, and use the threshold-FHE channel only to compute the
cross-site **consensus update**. The local CVXR solves exploit DPP
(disciplined parametric programming) and stay fast because their
Parameter values change but the problem structure does not. The
encrypted channel earns its keep on the consensus aggregation, where the
per-site $`(x_i + u_i)`$ vectors are summed and averaged under
encryption and the result is recovered via threshold decryption.

This is the architectural complement to `cox-threshold.Rmd`:

|  | `cox-threshold` | `cvxr-consensus-admm` |
|----|----|----|
| Optimizer | [`stats4::mle()`](https://rdrr.io/r/stats4/mle.html) (BFGS) | Consensus ADMM (one outer loop) |
| Local solver | `coxph(iter.max = 0)` | [`CVXR::psolve()`](https://www.cvxgrp.org/CVXR/reference/psolve.html) |
| FHE call site | every BFGS function evaluation | every ADMM iteration |
| Topology | master/worker fan-out + fan-in | peer-to-peer with threshold aggregator |
| Threshold keys | yes (n-of-n) | yes (n-of-n) |

Both compose existing R machinery with a threshold-FHE channel without
rewriting the optimizer.

## The global problem and its consensus split

We pick L2-regularised logistic regression. With $`N`$ sites, local data
$`(X_i, y_i)`$ at site $`i`$, a shared coefficient
$`x \in \mathbb{R}^p`$, the global problem is

``` math
\min_{x \in \mathbb{R}^p}
\sum_{i=1}^{N} \ell_i(x; X_i, y_i)
+ \frac{\lambda}{2}\, \lVert x \rVert_2^2
```

where $`\ell_i`$ is the logistic loss on site $`i`$. The standard
consensus split (Boyd, Parikh, Chu, Peleato, Eckstein, 2011) introduces
local copies $`x_i \in \mathbb{R}^p`$ and a single global consensus
$`z \in \mathbb{R}^p`$:

``` math
\min_{\{x_i\}, z}
\sum_i \ell_i(x_i; X_i, y_i) + \frac{\lambda}{2}\, \lVert z \rVert_2^2
\quad \text{s.t.}\quad x_i = z, \ \forall i.
```

The augmented-Lagrangian iteration is:

``` math
\begin{aligned}
x_i^{k+1} &= \arg\min_{x_i}\, \ell_i(x_i) + \frac{\lambda}{2N}\lVert x_i\rVert_2^2 + \frac{\rho}{2}\lVert x_i - z^k + u_i^k\rVert_2^2, \\
z^{k+1} &= \frac{1}{N}\sum_i (x_i^{k+1} + u_i^k), \\
u_i^{k+1} &= u_i^k + (x_i^{k+1} - z^{k+1}).
\end{aligned}
```

The $`x`$-update is local at each site; the $`z`$-update is the
consensus average that has to traverse the encrypted channel; the
$`u`$-update is local again. Only the $`z`$-update needs cryptography.

## The local CVXR subproblem

[`library`](https://rdrr.io/r/base/library.html)`(`[`homomorpheR`](https://bnaras.github.io/homomorpheR/)`)`` `[`suppressPackageStartupMessages`](https://rdrr.io/r/base/message.html)`(`[`library`](https://rdrr.io/r/base/library.html)`(`[`CVXR`](https://cvxr.rbind.io)`)``)`` `` ``N`` ``<-`` ``3L`` ``p`` ``<-`` ``4L`` ``lam`` ``<-`` ``1`

We build the local problem so that DPP can engage: $`z`$ and $`u`$ are
`Parameter`s (so canonicalization is shared across iterations); $`X_i`$,
$`y_i`$, and $`\rho`$ are *constants* baked in at construction time (so
the augmented Lagrangian term enters affinely in the parameters and the
DPP fast path is not broken).

`build_local_problem`` ``<-`` ``function``(``X_i``, ``y_i``, ``rho_val``)`` ``{`` `` ``n_i`` ``<-`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``X_i``)`` `` ``p_i`` ``<-`` `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``X_i``)`` `` `` ``x`` ``<-`` `[`Variable`](https://www.cvxgrp.org/CVXR/reference/Variable.html)`(``p_i``)`` `` ``zp`` ``<-`` `[`Parameter`](https://www.cvxgrp.org/CVXR/reference/Parameter.html)`(``p_i``)`` `` ``up`` ``<-`` `[`Parameter`](https://www.cvxgrp.org/CVXR/reference/Parameter.html)`(``p_i``)`` `` `` ``y_signs`` ``<-`` ``2`` ``*`` ``y_i`` ``-`` ``1`` `` ``margins`` ``<-`` ``-``y_signs`` ``*`` ``(``X_i`` `[`%*%`](https://rdrr.io/r/base/matmult.html)` ``x``)`` `` `` ``local_loss`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`logistic`](https://www.cvxgrp.org/CVXR/reference/logistic.html)`(``margins``)``)`` ``+`` `` ``(``lam`` ``/`` ``(``2`` ``*`` ``N``)``)`` ``*`` `[`sum_squares`](https://www.cvxgrp.org/CVXR/reference/sum_squares.html)`(``x``)`` `` ``augmented`` ``<-`` ``(``rho_val`` ``/`` ``2``)`` ``*`` `[`sum_squares`](https://www.cvxgrp.org/CVXR/reference/sum_squares.html)`(``x`` ``-`` ``zp`` ``+`` ``up``)`` `` `` ``prob`` ``<-`` `[`Problem`](https://www.cvxgrp.org/CVXR/reference/Problem.html)`(`[`Minimize`](https://www.cvxgrp.org/CVXR/reference/Minimize.html)`(``local_loss`` ``+`` ``augmented``)``)`` `` `` `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``zp``)`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p_i``)`` `` `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``up``)`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p_i``)`` `` `` `[`list`](https://rdrr.io/r/base/list.html)`(``prob ``=`` ``prob``, x ``=`` ``x``, zp ``=`` ``zp``, up ``=`` ``up``)`` ``}`

A vignette-local site class wraps the local CVXR problem plus the ADMM
state. The exported `Site` from this package is shaped for the
master/worker pattern; ADMM is peer-to-peer and needs its own per-site
state, so we define `ConsensusSite` inline.

[`library`](https://rdrr.io/r/base/library.html)`(`[`S7`](https://rconsortium.github.io/S7/)`)`` `` ``ConsensusSite`` ``<-`` `[`new_class`](https://rconsortium.github.io/S7/reference/new_class.html)`(``"ConsensusSite"``,`` `` properties ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` name ``=`` ``class_character``,`` `` n ``=`` ``class_integer``,`` `` state ``=`` ``class_any`` `` ``)`` ``)`` `` ``make_consensus_site`` ``<-`` ``function``(``name``, ``X_i``, ``y_i``, ``rho_val``)`` ``{`` `` ``st`` ``<-`` `[`new.env`](https://rdrr.io/r/base/environment.html)`(``parent ``=`` `[`emptyenv`](https://rdrr.io/r/base/environment.html)`(``)``)`` `` ``st``$``X`` ``<-`` ``X_i`` `` ``st``$``y`` ``<-`` ``y_i`` `` ``built`` ``<-`` ``build_local_problem``(``X_i``, ``y_i``, ``rho_val``)`` `` ``st``$``prob`` ``<-`` ``built``$``prob`` `` ``st``$``x_var`` ``<-`` ``built``$``x`` `` ``st``$``zp`` ``<-`` ``built``$``zp`` `` ``st``$``up`` ``<-`` ``built``$``up`` `` ``st``$``x_curr`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``X_i``)``)`` `` ``st``$``u_curr`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``X_i``)``)`` `` ``ConsensusSite``(``name ``=`` ``name``, n ``=`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``X_i``)``, state ``=`` ``st``)`` ``}`` `` ``local_update`` ``<-`` ``function``(``site``, ``z_curr``)`` ``{`` `` ``st`` ``<-`` ``site``@``state`` `` `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``st``$``zp``)`` ``<-`` ``z_curr`` `` `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``st``$``up``)`` ``<-`` ``st``$``u_curr`` `` `[`psolve`](https://www.cvxgrp.org/CVXR/reference/psolve.html)`(``st``$``prob``, solver ``=`` ``"CLARABEL"``)`` `` ``if`` ``(``!`[`status`](https://www.cvxgrp.org/CVXR/reference/status.html)`(``st``$``prob``)`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"optimal"``, ``"optimal_inaccurate"``)``)`` `` `[`stop`](https://rdrr.io/r/base/stop.html)`(``"Local solve at "``, ``site``@``name``, ``" did not reach optimal status."``)`` `` ``st``$``x_curr`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``st``$``x_var``)``)`` `` `[`invisible`](https://rdrr.io/r/base/invisible.html)`(``st``$``x_curr``)`` ``}`

## The threshold-FHE consensus aggregation

The consensus update needs to compute
$`z^{k+1} = \frac{1}{N}\sum_i (x_i^{k+1} + u_i^k)`$. This is a
length-$`p`$ vector average: pack each site’s $`x_i + u_i`$ into the
slots of one value, encrypt under the joint public key, sum the
encrypted vectors, multiply by the unencrypted constant $`1/N`$ (one
multiplication by a cleartext value, costing a single level of the
precision budget), and threshold-decrypt the result.

We use the same threshold infrastructure as `cox-threshold.Rmd`: a CKKS
context with `Feature$MULTIPARTY` enabled and
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md),
which runs the chained `multiparty_key_gen()` setup automatically. The
consensus function calls
[`master_encrypt()`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md)
to encrypt each site’s contribution under the joint public key and
[`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
to threshold-decrypt the result; the partial-decrypt fan-in across sites
is handled inside the master class.

`cc`` ``<-`` ``openfhe.R``::`[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`` `` multiplicative_depth ``=`` ``1L``,`` `` scaling_mod_size ``=`` ``59L``,`` `` first_mod_size ``=`` ``60L``,`` `` batch_size ``=`` ``8L``,`` `` features ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``openfhe.R``::`[`Feature`](https://openfheorg.github.io/openfhe.R/reference/Feature.html)`$``MULTIPARTY``)``)`

`encrypted_consensus`` ``<-`` ``function``(``threshold_master``, ``sites``)`` ``{`` `` ``cts`` ``<-`` `[`vector`](https://rdrr.io/r/base/vector.html)`(``"list"``, `[`length`](https://rdrr.io/r/base/length.html)`(``sites``)``)`` `` ``for`` ``(``i`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``sites``)``)`` ``{`` `` ``st`` ``<-`` ``sites``[[``i``]``]``@``state`` `` ``val`` ``<-`` ``st``$``x_curr`` ``+`` ``st``$``u_curr`` `` ``cts``[[``i``]``]`` ``<-`` `[`master_encrypt`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md)`(``threshold_master``, ``val``)`` `` ``}`` `` ``ct_sum`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``cts``)`` `` ``ct_avg`` ``<-`` ``ct_sum`` ``*`` ``(``1`` ``/`` `[`length`](https://rdrr.io/r/base/length.html)`(``sites``)``)`` `` `[`master_decrypt`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)`(``threshold_master``, ``ct_avg``, len ``=`` ``p``)`` ``}`

What this protocol hides and reveals: every ADMM iteration the
aggregator recovers the *new consensus* $`z^k`$ exactly. The per-site
$`(x_i + u_i)`$ vectors never appear in the clear anywhere — additions
and the scalar multiply happen under encryption, and the decryption is
n-of-n. A subpoena to the aggregator yields the trajectory $`\{z^k\}`$
but no individual site’s contribution. A subpoena to any one site yields
that site’s local $`X_i, y_i`$ and its own secret share, but no other
site’s contribution and no decryption power.

## Simulated cohort

Three sites with deliberately uneven sample sizes; same true
coefficients across sites.

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``98765``)`` `` ``beta_true`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.5``, ``-``1.0``, ``0.3``, ``0.8``)`` ``n_per`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``400``, ``250``, ``350``)`` `` ``site_data`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`seq_len`](https://rdrr.io/r/base/seq.html)`(``N``)``, ``function``(``i``)`` ``{`` `` ``X`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n_per``[``i``]`` ``*`` ``p``)``, nrow ``=`` ``n_per``[``i``]``, ncol ``=`` ``p``)`` `` ``eta`` ``<-`` ``X`` `[`%*%`](https://rdrr.io/r/base/matmult.html)` ``beta_true`` `` ``y`` ``<-`` `[`rbinom`](https://rdrr.io/r/stats/Binomial.html)`(``n_per``[``i``]``, size ``=`` ``1``, prob ``=`` ``1`` ``/`` ``(``1`` ``+`` `[`exp`](https://rdrr.io/r/base/Log.html)`(``-``eta``)``)``)`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``X ``=`` ``X``, y ``=`` ``y``)`` ``}``)`` `` ``X_all`` ``<-`` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``site_data``, ``` `[[` ```, ``"X"``)``)`` ``y_all`` ``<-`` `[`unlist`](https://rdrr.io/r/base/unlist.html)`(`[`lapply`](https://rdrr.io/r/base/lapply.html)`(``site_data``, ``` `[[` ```, ``"y"``)``)`

## Centralized CVXR fit

For the comparison at the end of the run, we fit the same problem
centrally. This is the target the federated fit should match.

`x_central`` ``<-`` `[`Variable`](https://www.cvxgrp.org/CVXR/reference/Variable.html)`(``p``)`` ``y_signs`` ``<-`` ``2`` ``*`` ``y_all`` ``-`` ``1`` ``margins`` ``<-`` ``-``y_signs`` ``*`` ``(``X_all`` `[`%*%`](https://rdrr.io/r/base/matmult.html)` ``x_central``)`` ``central_prob`` ``<-`` `[`Problem`](https://www.cvxgrp.org/CVXR/reference/Problem.html)`(`[`Minimize`](https://www.cvxgrp.org/CVXR/reference/Minimize.html)`(`` `` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`logistic`](https://www.cvxgrp.org/CVXR/reference/logistic.html)`(``margins``)``)`` ``+`` ``(``lam`` ``/`` ``2``)`` ``*`` `[`sum_squares`](https://www.cvxgrp.org/CVXR/reference/sum_squares.html)`(``x_central``)`` ``)``)`` `[`psolve`](https://www.cvxgrp.org/CVXR/reference/psolve.html)`(``central_prob``, solver ``=`` ``"CLARABEL"``)`

    ## [1] 552.3928

`beta_central`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``x_central``)``)`

## Tuning $`\rho`$ before the loop

ADMM convergence depends on $`\rho`$. We sweep three candidate values
against a *cleartext* copy of the protocol (no FHE) and pick the one
that converges fastest. The chosen value is fixed for the encrypted run.

`rho_grid`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``10``, ``50``, ``200``)`` ``sweep_iters`` ``<-`` `[`integer`](https://rdrr.io/r/base/integer.html)`(`[`length`](https://rdrr.io/r/base/length.html)`(``rho_grid``)``)`` `` ``for`` ``(``g`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``rho_grid``)``)`` ``{`` `` ``rho_val`` ``<-`` ``rho_grid``[``g``]`` `` ``sites`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`seq_len`](https://rdrr.io/r/base/seq.html)`(``N``)``, ``function``(``i``)`` `` ``make_consensus_site``(`[`paste0`](https://rdrr.io/r/base/paste.html)`(``"Site "``, ``i``)``,`` `` ``site_data``[[``i``]``]``$``X``, ``site_data``[[``i``]``]``$``y``,`` `` rho_val ``=`` ``rho_val``)``)`` `` ``z_curr`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p``)`` `` ``converged`` ``<-`` ``FALSE`` `` ``for`` ``(``k`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``40``)``)`` ``{`` `` ``for`` ``(``s`` ``in`` ``sites``)`` ``local_update``(``s``, ``z_curr``)`` `` ``means_xu`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```,`` `` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``function``(``s``)`` ``s``@``state``$``x_curr`` ``+`` ``s``@``state``$``u_curr``)``)`` ``/`` ``N`` `` ``z_new`` ``<-`` ``means_xu`` `` ``for`` ``(``s`` ``in`` ``sites``)`` ``{`` `` ``s``@``state``$``u_curr`` ``<-`` ``s``@``state``$``u_curr`` ``+`` ``(``s``@``state``$``x_curr`` ``-`` ``z_new``)`` `` ``}`` `` ``primal_res`` ``<-`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`mean`](https://rdrr.io/r/base/mean.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(`[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``function``(``s``)`` `` `[`sum`](https://rdrr.io/r/base/sum.html)`(``(``s``@``state``$``x_curr`` ``-`` ``z_new``)``^``2``)``)``)``)``)`` `` ``dual_res`` ``<-`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``N``)`` ``*`` ``rho_val`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`sum`](https://rdrr.io/r/base/sum.html)`(``(``z_new`` ``-`` ``z_curr``)``^``2``)``)`` `` ``z_curr`` ``<-`` ``z_new`` `` ``if`` ``(``primal_res`` ``<`` ``1e-3`` ``&&`` ``dual_res`` ``<`` ``1e-3``)`` ``{`` `` ``converged`` ``<-`` ``TRUE`` `` ``sweep_iters``[``g``]`` ``<-`` ``k`` `` ``break`` `` ``}`` `` ``}`` `` ``if`` ``(``!``converged``)`` ``sweep_iters``[``g``]`` ``<-`` ``NA_integer_`` ``}`` `` ``sweep_table`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``rho ``=`` ``rho_grid``, iters ``=`` ``sweep_iters``)`` ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``sweep_table``, caption ``=`` ``"Cleartext ADMM iterations to convergence"``)`

| rho | iters |
|----:|------:|
|  10 |    32 |
|  50 |    19 |
| 200 |    NA |

Cleartext ADMM iterations to convergence {.table}

`rho_chosen`` ``<-`` ``rho_grid``[`[`which.min`](https://rdrr.io/r/base/which.min.html)`(``sweep_iters``)``]`` `[`cat`](https://rdrr.io/r/base/cat.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"Selected rho = %g (converged in %d iterations).\n"``,`` `` ``rho_chosen``, `[`min`](https://rdrr.io/r/base/Extremes.html)`(``sweep_iters``, na.rm ``=`` ``TRUE``)``)``)`

    ## Selected rho = 50 (converged in 19 iterations).

## The encrypted ADMM loop

Threshold key generation, sites built with the chosen $`\rho`$, then the
main loop:

`master`` ``<-`` `[`make_threshold_master`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)`(``"Aggregator"``,`` `` crypto_context ``=`` ``cc``,`` `` n_sites ``=`` ``N``)`` `` ``sites`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`seq_len`](https://rdrr.io/r/base/seq.html)`(``N``)``, ``function``(``i``)`` `` ``make_consensus_site``(`[`paste0`](https://rdrr.io/r/base/paste.html)`(``"Site "``, ``i``)``,`` `` ``site_data``[[``i``]``]``$``X``, ``site_data``[[``i``]``]``$``y``,`` `` rho_val ``=`` ``rho_chosen``)``)`` `` ``z_curr`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``p``)`` ``max_iter`` ``<-`` ``40`` ``reltol`` ``<-`` ``1e-3`` ``converged`` ``<-`` ``FALSE`` ``trajectory`` ``<-`` `[`vector`](https://rdrr.io/r/base/vector.html)`(``"list"``, ``max_iter``)`` `` ``for`` ``(``k`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``max_iter``)``)`` ``{`` `` ``for`` ``(``s`` ``in`` ``sites``)`` ``local_update``(``s``, ``z_curr``)`` `` ``z_new`` ``<-`` ``encrypted_consensus``(``master``, ``sites``)`` `` ``for`` ``(``s`` ``in`` ``sites``)`` ``{`` `` ``s``@``state``$``u_curr`` ``<-`` ``s``@``state``$``u_curr`` ``+`` ``(``s``@``state``$``x_curr`` ``-`` ``z_new``)`` `` ``}`` `` ``primal_res`` ``<-`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`mean`](https://rdrr.io/r/base/mean.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(`[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``function``(``s``)`` `` `[`sum`](https://rdrr.io/r/base/sum.html)`(``(``s``@``state``$``x_curr`` ``-`` ``z_new``)``^``2``)``)``)``)``)`` `` ``dual_res`` ``<-`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``N``)`` ``*`` ``rho_chosen`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`sum`](https://rdrr.io/r/base/sum.html)`(``(``z_new`` ``-`` ``z_curr``)``^``2``)``)`` `` ``trajectory``[[``k``]``]`` ``<-`` ``z_new`` `` ``z_curr`` ``<-`` ``z_new`` `` ``if`` ``(``primal_res`` ``<`` ``reltol`` ``&&`` ``dual_res`` ``<`` ``reltol``)`` ``{`` `` ``converged`` ``<-`` ``TRUE`` `` ``trajectory`` ``<-`` ``trajectory``[`[`seq_len`](https://rdrr.io/r/base/seq.html)`(``k``)``]`` `` ``break`` `` ``}`` ``}`` `` ``if`` ``(``!``converged``)`` `` `[`stop`](https://rdrr.io/r/base/stop.html)`(``"Encrypted ADMM did not converge within max_iter; rerun the rho sweep."``)`` `` `[`cat`](https://rdrr.io/r/base/cat.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"Encrypted ADMM converged in %d iterations.\n"``, `[`length`](https://rdrr.io/r/base/length.html)`(``trajectory``)``)``)`

    ## Encrypted ADMM converged in 19 iterations.

## Comparison with the centralized fit

`comparison`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`` `` coefficient ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"beta_"``, `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``p``)``)``,`` `` threshold_distributed ``=`` ``z_curr``,`` `` centralized_cvxr ``=`` ``beta_central``,`` `` abs_diff ``=`` `[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_curr`` ``-`` ``beta_central``)`` ``)`` ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``comparison``, digits ``=`` ``6``,`` `` caption ``=`` ``"Threshold-FHE consensus ADMM vs. centralized CVXR"``)`

| coefficient | threshold_distributed | centralized_cvxr | abs_diff |
|:------------|----------------------:|-----------------:|---------:|
| beta_1      |              0.419682 |         0.419687 |    5e-06 |
| beta_2      |             -0.939203 |        -0.939209 |    6e-06 |
| beta_3      |              0.384038 |         0.384041 |    4e-06 |
| beta_4      |              0.659835 |         0.659840 |    5e-06 |

Threshold-FHE consensus ADMM vs. centralized CVXR {.table}

`max_diff`` ``<-`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_curr`` ``-`` ``beta_central``)``)`` `[`cat`](https://rdrr.io/r/base/cat.html)`(`[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"Max absolute coefficient difference vs. centralized fit: %.2e\n"``,`` `` ``max_diff``)``)`

    ## Max absolute coefficient difference vs. centralized fit: 5.58e-06

`if`` ``(``max_diff`` ``>`` ``10`` ``*`` ``reltol``)`` `` `[`stop`](https://rdrr.io/r/base/stop.html)`(``"Encrypted ADMM agreement with the aggregated cleartext fit is too loose; "``,`` `` ``"investigate before publishing this run."``)`

## Discussion

1.  **CVXR symbolic problems compose with threshold FHE.** The local
    CVXR solve runs cleartext at each site; only the cross-site
    consensus update goes through the encrypted channel. The reader does
    not have to rewrite their CVXR model for an encrypted setting — the
    same `Problem(Minimize(...))` they would write for cleartext data is
    used here verbatim.
2.  **No single party holds the secret key.**
    [`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
    distributes the secret across all sites; the aggregator holds only
    the joint public key. Encrypted intermediates are undecryptable by
    any single party, and the consensus result appears only after the
    n-of-n partial-decryption fusion.
3.  **DPP keeps the inner loop fast.** Each site’s CVXR problem is built
    once at setup; ADMM iterations only update the parameter values.
    Without DPP, the canonicalization would re-run every iteration and
    the vignette would be infeasible.

## Limitations

- **Honest-but-curious trust.** A site that misreports its local
  $`x_i + u_i`$ can corrupt the consensus. Detecting this requires
  commitments / zero-knowledge proofs that this vignette does not
  implement.
- **The aggregator sees the trajectory $`\{z^k\}`$.** Per-iteration
  consensus values are revealed in the clear (after fusion) so the
  optimizer can decide convergence. A subpoena to the aggregator yields
  this trajectory.
- **No output privacy.** The released $`\hat\beta = z^\star`$ is the
  same coefficient vector as the centralized fit. Output-level attacks
  are out of scope here; the companion DP vignettes (not yet ported)
  demonstrate output DP composition.
