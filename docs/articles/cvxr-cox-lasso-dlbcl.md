# Federated Cox-Lasso via Consensus ADMM on DLBCL

## Introduction

This vignette runs the threshold-FHE consensus-ADMM protocol on a real,
high-dimensional survival problem: a stratified **Cox lasso** on the
diffuse large-B-cell lymphoma (DLBCL) gene-expression cohort of
Rosenwald et al. (2002), the dataset Bayle et al. (2025) use to motivate
distributed Cox estimation.

The `cox` and `cox-threshold` vignettes give an optimizer such as
[`mle()`](https://rdrr.io/r/stats4/mle.html) an objective function that
runs one encrypted round across the sites. That does not work for
[CVXR](https://cvxr.rbind.io), which does not call a function we supply:
it converts a symbolic problem once to a standard form and passes that
to a solver. Instead we use **federated consensus ADMM** (Boyd, Parikh,
Chu, Peleato and Eckstein, 2011). The global problem is split into
per-site subproblems, each solved by [CVXR](https://cvxr.rbind.io) in
the clear, and only the cross-site **consensus average** is encrypted.
Disciplined parametrized programming (DPP) keeps the per-site solves
fast: between iterations only the `Parameter` values change, not the
problem structure.

We develop the fit in two passes. First we run the whole thing **in the
clear** to fix the target: standardize, screen, and solve the consensus
ADMM with an ordinary unencrypted average, checking against the
centralized [CVXR](https://cvxr.rbind.io) solve. Then we put it **under
threshold FHE**, replacing each cross-site sum — the standardization
moments, the screening statistics, and the per-iteration consensus
average — with one round of the encrypted-summation primitive, and
confirm the encrypted fit reproduces the in-the-clear reference.

> **Reproducibility and verification.** The code chunks in this vignette
> *are* the pipeline; they are gated `eval = RECOMPUTE` and do not run
> when the vignette builds (the encrypted ADMM takes ~150 iterations of
> per-site [CVXR](https://cvxr.rbind.io) solves). The displayed numbers
> come from `data(cvxr_consensus)`, which was produced by extracting
> these chunks with
> [`knitr::purl()`](https://rdrr.io/pkg/knitr/man/knit.html) and running
> them. To verify the results, do the same:
>
> \
> `vig`` ``<-`` `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"doc"``, ``"cvxr-cox-lasso-dlbcl.Rmd"``,`\
> `                   package ``=`` ``"homomorpheR"``)`\
> `RECOMPUTE`` ``<-`` ``TRUE``                                  ``# un-gate the chunks`\
> `src`` ``<-`` ``knitr``::`[`purl`](https://rdrr.io/pkg/knitr/man/knit.html)`(``vig``, output ``=`` `[`tempfile`](https://rdrr.io/r/base/tempfile.html)`(``fileext ``=`` ``".R"``)``, quiet ``=`` ``TRUE``)`\
> [`source`](https://rdrr.io/r/base/source.html)`(``src``)``                                        ``# runs the pipeline (minutes)`\
> `fresh`` ``<-`` ``cvxr_consensus``                            ``# just recomputed`\
> [`data`](https://rdrr.io/r/utils/data.html)`(``cvxr_consensus``, package ``=`` ``"homomorpheR"``)``      ``# the shipped copy`\
> [`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``fresh``$``z_enc`` ``-`` ``cvxr_consensus``$``z_enc``)``)``       ``# small, not zero`
>
> Threshold key generation is randomized, so a re-run reproduces the
> shipped coefficients up to CKKS approximation noise, not bit-for-bit.
> This `purl()`-then-[`source()`](https://rdrr.io/r/base/source.html) is
> exactly what `data-raw/cvxr_consensus.R` does to build the shipped
> object.

## The global problem and its consensus split

The three molecular subgroups (GCB, ABC, Type III) are our sites: they
arise from different cells of origin and have systematically different
prognosis. Using them as sites is a choice made for this demonstration.
With \\N\\ sites, per-site standardized design matrices \\X_k \in
\mathbb{R}^{n_k \times p}\\, event times \\t_k\\, status \\\delta_k\\, a
shared coefficient \\\beta \in \mathbb{R}^p\\, and the per-stratum Cox
partial log-likelihood \\\ell_k\\ in Breslow form, the global problem is

\\ \min\_{\beta \in \mathbb{R}^p}\\
-\sum\_{k=1}^{N}\ell_k(\beta)\\+\\\lambda\lVert\beta\rVert_1 . \\

Because the partial likelihood factorizes additively across strata, the
consensus split introduces per-site copies \\x_k\\ and a global
consensus \\z\\:

\\ \min\_{\\x_k\\,\\z}\\ \sum_k\bigl(-\ell_k(x_k)\bigr) + \lambda\lVert
z\rVert_1 \quad\text{s.t.}\quad x_k = z,\\ \forall k, \\

with augmented-Lagrangian iteration

\\ \begin{aligned} x_k^{t+1} &= \arg\min_x -\ell_k(x) +
\tfrac{\rho}{2}\lVert x - z^t + u_k^t\rVert_2^2,\\ z^{t+1} &=
S\_{\lambda/(N\rho)}\\\Bigl(\tfrac{1}{N}\sum_k
(x_k^{t+1}+u_k^t)\Bigr),\\ u_k^{t+1} &= u_k^t + (x_k^{t+1}-z^{t+1}),
\end{aligned} \\

where \\S\_\tau(v)=\operatorname{sign}(v)\max(\|v\|-\tau,0)\\ is
soft-thresholding (the proximal map of \\\tau\lVert\cdot\rVert_1\\). The
\\x\\-update is the per-site [CVXR](https://cvxr.rbind.io) solve; the
\\z\\-update needs the cross-site average \\\bar
w=\tfrac1N\sum_k(x_k+u_k)\\ — that average is the only quantity that
traverses the encrypted channel; the soft-threshold is closed-form at
the aggregator and adds no cryptographic depth.

The pipeline needs `survival` and [CVXR](https://cvxr.rbind.io)
alongside the encryption packages.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`survival`](https://github.com/therneau/survival)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`CVXR`](https://cvxr.rbind.io)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`openfhe.R`](https://openfheorg.github.io/openfhe.R/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`homomorpheR`](https://bnaras.github.io/homomorpheR/)`)`

## The federated fit in the clear

The three subgroups are our sites. We load the cohort, split it by
subgroup, and keep each site’s raw expression matrix, event times, and
status.

\
[`data`](https://rdrr.io/r/utils/data.html)`(``DLBCL``,     package ``=`` ``"homomorpheR"``)``  ``# 235 patients: survival + signatures`\
[`data`](https://rdrr.io/r/utils/data.html)`(``DLBCL_gex``, package ``=`` ``"homomorpheR"``)``  ``# 235 patients x 6416 Lymphochip probes`\
`dlbcl`` ``<-`` ``DLBCL`\
`dlbcl``$``Subgroup`` ``<-`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``dlbcl``$``Subgroup``, levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"GCB"``,``"ABC"``,``"Type III"``)``)`\
[`stopifnot`](https://rdrr.io/r/base/stopifnot.html)`(`[`identical`](https://rdrr.io/r/base/identical.html)`(`[`as.character`](https://rdrr.io/r/base/character.html)`(``dlbcl``$``ID``)``, `[`rownames`](https://rdrr.io/r/base/colnames.html)`(``DLBCL_gex``)``)``)``  ``# rows aligned`\
\
`sites_raw`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`levels`](https://rdrr.io/r/base/levels.html)`(``dlbcl``$``Subgroup``)``, ``function``(``s``)`` ``{`\
`    ``idx`` ``<-`` `[`which`](https://rdrr.io/r/base/which.html)`(``dlbcl``$``Subgroup`` ``==`` ``s``)`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``name ``=`` ``s``, X ``=`` ``DLBCL_gex``[``idx``, ``]``, time ``=`` ``dlbcl``$``time``[``idx``]``,`\
`         status ``=`` ``dlbcl``$``status``[``idx``]``)`\
`}``)`\
[`names`](https://rdrr.io/r/base/names.html)`(``sites_raw``)`` ``<-`` `[`levels`](https://rdrr.io/r/base/levels.html)`(``dlbcl``$``Subgroup``)`\
`N_sites`` ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(``sites_raw``)`\
`N_total`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`vapply`](https://rdrr.io/r/base/lapply.html)`(``sites_raw``, ``function``(``s``)`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``s``$``X``)``, ``1L``)``)`\
`P_raw``   ``<-`` `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``DLBCL_gex``)`

We standardize the features so the L1 penalty applies uniformly across
predictors on different scales. The pooled mean and variance are
additive over sites: site \\k\\ contributes \\S_k=\sum\_{i\in k}x_i\\
and \\Q_k=\sum\_{i\in k}x_i^2\\, from which
\\\mu=\tfrac1{N\_{\mathrm{tot}}}\sum_k S_k\\ and
\\\sigma^2=\tfrac1{N\_{\mathrm{tot}}}\sum_k Q_k-\mu^2\\. In the clear
this is a pair of `colSums`.

\
`pool_plain`` ``<-`` ``function``(``sites``, ``n_total``)`` ``{`\
`    ``s`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``function``(``s``)`` `[`colSums`](https://rdrr.io/r/base/colSums.html)`(``s``$``X``)``)``)`\
`    ``q`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``function``(``s``)`` `[`colSums`](https://rdrr.io/r/base/colSums.html)`(``s``$``X``^``2``)``)``)`\
`    ``mu``     ``<-`` ``s`` ``/`` ``n_total`\
`    ``sigma2`` ``<-`` `[`pmax`](https://rdrr.io/r/base/Extremes.html)`(``q`` ``/`` ``n_total`` ``-`` ``mu``^``2``, ``.Machine``$``double.eps``)`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``mu ``=`` ``mu``, sigma ``=`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``sigma2``)``)`\
`}`\
`pool``      ``<-`` ``pool_plain``(``sites_raw``, ``N_total``)`\
`sites_std`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites_raw``, ``function``(``s``)`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    name ``=`` ``s``$``name``,`\
`    X    ``=`` `[`sweep`](https://rdrr.io/r/base/sweep.html)`(`[`sweep`](https://rdrr.io/r/base/sweep.html)`(``s``$``X``, ``2``, ``pool``$``mu``, ``"-"``)``, ``2``, ``pool``$``sigma``, ``"/"``)``,`\
`    time ``=`` ``s``$``time``, status ``=`` ``s``$``status``)``)`

Solving the full Cox-lasso at \\p = 6416\\ exhausts memory during
[CVXR](https://cvxr.rbind.io) canonicalization, so we pre-screen to the
\\K = 100\\ probes with the strongest univariate association with
survival — the screen-then-fit pattern Bayle et al. (2025) use on this
cohort. For probe \\g\\ on stratum \\k\\ in event-time order with risk
set \\R_i^{(k)}\\ at event \\i\\, \\U_g^{(k)}=\sum\_{i\in
k,\delta_i=1}(X\_{i,g}-\overline X\_{R_i^{(k)},g})\\ is the score and
\\I_g^{(k)}=\sum\_{i\in
k,\delta_i=1}\widehat{\operatorname{Var}}\_{R_i^{(k)}}(X\_{:,g})\\ the
information; both sum across sites, and the screen ranks probes by
\\\|Z_g\|=\|U_g\|/\sqrt{I_g}\\.

\
`K`` ``<-`` ``100L`\
\
`score_info_at_zero`` ``<-`` ``function``(``X``, ``time``, ``status``)`` ``{`\
`    ``p`` ``<-`` `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``X``)`\
`    ``ord`` ``<-`` `[`order`](https://rdrr.io/r/base/order.html)`(``time``, ``-``status``)`\
`    ``X_o`` ``<-`` ``X``[``ord``, , drop ``=`` ``FALSE``]``; ``stat_o`` ``<-`` ``status``[``ord``]`\
`    ``n`` ``<-`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``X_o``)``; ``U`` ``<-`` `[`numeric`](https://rdrr.io/r/base/numeric.html)`(``p``)``; ``I`` ``<-`` `[`numeric`](https://rdrr.io/r/base/numeric.html)`(``p``)`\
`    ``for`` ``(``i`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``n``)``)`` ``{`\
`        ``if`` ``(``stat_o``[``i``]`` ``==`` ``1L``)`` ``{`\
`            ``risk`` ``<-`` ``X_o``[``i``:``n``, , drop ``=`` ``FALSE``]`\
`            ``mu_R`` ``<-`` `[`colMeans`](https://rdrr.io/r/base/colSums.html)`(``risk``)`\
`            ``U`` ``<-`` ``U`` ``+`` ``(``X_o``[``i``, ``]`` ``-`` ``mu_R``)`\
`            ``I`` ``<-`` ``I`` ``+`` `[`colSums`](https://rdrr.io/r/base/colSums.html)`(`[`sweep`](https://rdrr.io/r/base/sweep.html)`(``risk``, ``2``, ``mu_R``, ``"-"``)``^``2``)`` ``/`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``risk``)`\
`        ``}`\
`    ``}`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``U ``=`` ``U``, I ``=`` ``I``)`\
`}`\
\
`screen_plain`` ``<-`` ``function``(``sites``, ``K``)`` ``{`\
`    ``UI`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``function``(``s``)`` ``score_info_at_zero``(``s``$``X``, ``s``$``time``, ``s``$``status``)``)`\
`    ``U``  ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``UI``, ``` `[[` ```, ``"U"``)``)`\
`    ``I``  ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``UI``, ``` `[[` ```, ``"I"``)``)`\
`    ``Z``  ``<-`` ``U`` ``/`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`pmax`](https://rdrr.io/r/base/Extremes.html)`(``I``, ``.Machine``$``double.eps``)``)`\
`    `[`order`](https://rdrr.io/r/base/order.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``Z``)``, decreasing ``=`` ``TRUE``)``[`[`seq_len`](https://rdrr.io/r/base/seq.html)`(``K``)``]`\
`}`\
`top_idx``  ``<-`` ``screen_plain``(``sites_std``, ``K``)`\
`sites_KS`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites_std``, ``function``(``s``)`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    name ``=`` ``s``$``name``, X ``=`` ``s``$``X``[``, ``top_idx``]``,`\
`    time ``=`` ``s``$``time``, status ``=`` ``s``$``status``)``)`\
`sigma_K``  ``<-`` ``pool``$``sigma``[``top_idx``]`

`sites_KS` now holds each stratum on the \\K = 100\\ screened probes,
and `sigma_K` keeps their pooled SDs for the back-transform to the
original gene-expression scale.

The centralized stratified Cox-lasso fit — our ground truth — is a
single [CVXR](https://cvxr.rbind.io) solve summing the per-stratum
Breslow partial likelihoods plus the L1 penalty.

\
`LAMBDA`` ``<-`` ``5`\
`build_cox_breslow_nll`` ``<-`` ``function``(``beta_var``, ``X_s``, ``time_s``, ``status_s``)`` ``{`\
`    ``ord`` ``<-`` `[`order`](https://rdrr.io/r/base/order.html)`(``time_s``, ``-``status_s``)`\
`    ``X_o`` ``<-`` ``X_s``[``ord``, , drop ``=`` ``FALSE``]``; ``stat_o`` ``<-`` ``status_s``[``ord``]`\
`    ``n`` ``<-`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``X_o``)``; ``eta_o`` ``<-`` ``X_o`` `[`%*%`](https://rdrr.io/r/base/matmult.html)` ``beta_var`\
`    ``terms`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`\
`    ``for`` ``(``i`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``n``)``)`` ``{`\
`        ``if`` ``(``stat_o``[``i``]`` ``==`` ``1L``)`` ``{`\
`            ``terms``[[`[`length`](https://rdrr.io/r/base/length.html)`(``terms``)`` ``+`` ``1L``]``]`` ``<-`\
`                `[`log_sum_exp`](https://www.cvxgrp.org/CVXR/reference/log_sum_exp.html)`(``eta_o``[``i``:``n``, ``1``]``)`` ``-`` ``eta_o``[``i``, ``1``]`\
`        ``}`\
`    ``}`\
`    `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``terms``)`\
`}`\
\
`beta_var`` ``<-`` `[`Variable`](https://www.cvxgrp.org/CVXR/reference/Variable.html)`(``K``, name ``=`` ``"beta"``)`\
`nll_per``  ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites_KS``, ``function``(``s``)`\
`    ``build_cox_breslow_nll``(``beta_var``, ``s``$``X``, ``s``$``time``, ``s``$``status``)``)`\
`agg_prob`` ``<-`` `[`Problem`](https://www.cvxgrp.org/CVXR/reference/Problem.html)`(`[`Minimize`](https://www.cvxgrp.org/CVXR/reference/Minimize.html)`(`[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``nll_per``)`` ``+`\
`                                 ``LAMBDA`` ``*`` `[`p_norm`](https://www.cvxgrp.org/CVXR/reference/p_norm.html)`(``beta_var``, ``1``)``)``)`\
[`suppressMessages`](https://rdrr.io/r/base/message.html)`(`[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(`\
`    `[`psolve`](https://www.cvxgrp.org/CVXR/reference/psolve.html)`(``agg_prob``, solver ``=`` ``"CLARABEL"``, verbose ``=`` ``FALSE``)``)``)`\
`agg_beta`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``beta_var``)``)`

The distributed fit splits this objective into per-site subproblems tied
by a consensus variable. Each local problem is built so DPP applies:
\\z\\ and \\u\\ enter as `Parameter`s whose values change every ADMM
iteration while the symbolic structure does not; \\X_k\\, time, status,
and \\\rho\\ are constants.

\
`RHO`` ``<-`` ``50`\
\
`build_local`` ``<-`` ``function``(``X_k``, ``time_k``, ``status_k``, ``rho``)`` ``{`\
`    ``p`` ``<-`` `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``X_k``)`\
`    ``x``  ``<-`` `[`Variable`](https://www.cvxgrp.org/CVXR/reference/Variable.html)`(``p``)``; ``zp`` ``<-`` `[`Parameter`](https://www.cvxgrp.org/CVXR/reference/Parameter.html)`(``p``)``; ``up`` ``<-`` `[`Parameter`](https://www.cvxgrp.org/CVXR/reference/Parameter.html)`(``p``)`\
`    ``nll`` ``<-`` ``build_cox_breslow_nll``(``x``, ``X_k``, ``time_k``, ``status_k``)`\
`    ``aug`` ``<-`` ``(``rho`` ``/`` ``2``)`` ``*`` `[`sum_squares`](https://www.cvxgrp.org/CVXR/reference/sum_squares.html)`(``x`` ``-`` ``zp`` ``+`` ``up``)`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``prob ``=`` `[`Problem`](https://www.cvxgrp.org/CVXR/reference/Problem.html)`(`[`Minimize`](https://www.cvxgrp.org/CVXR/reference/Minimize.html)`(``nll`` ``+`` ``aug``)``)``, x ``=`` ``x``, zp ``=`` ``zp``, up ``=`` ``up``)`\
`}`\
`sites_problem`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites_KS``, ``function``(``s``)`\
`    ``build_local``(``s``$``X``, ``s``$``time``, ``s``$``status``, ``RHO``)``)`

The ADMM driver below is the whole federated algorithm. How the
cross-site average is formed is left to its `consensus` argument, a
function of the per-site \\(x_k,u_k)\\ vectors. Everything else (the
local [CVXR](https://cvxr.rbind.io) solves, the soft-threshold
\\z\\-update, the dual update, the stopping rule) is ordinary R. We call
it now with an unencrypted average, and later, unchanged, with an
encrypted one.

A note on the constants. We fix \\\rho = 50\\ and a cap of 200
iterations. The dual residual \\\rho\lVert z^{t+1}-z^t\rVert\\ is the
binding term here and decays slowly; with \\\rho = 50\\ the absolute
stopping rule `primal < TOL && dual < TOL` (with `TOL = 0.005`) trips at
iteration 147. Smaller \\\rho\\ reaches the tolerance in fewer
iterations but at a looser fit, so we keep \\\rho = 50\\ for the
tightest agreement with the centralized solve.

\
`MAX_ITER`` ``<-`` ``200L``; ``TOL`` ``<-`` ``5e-3`\
`soft_threshold`` ``<-`` ``function``(``v``, ``tau``)`` `[`sign`](https://rdrr.io/r/base/sign.html)`(``v``)`` ``*`` `[`pmax`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``v``)`` ``-`` ``tau``, ``0``)`\
\
`run_admm`` ``<-`` ``function``(``sites_problem``, ``consensus``)`` ``{`\
`    ``site_x`` ``<-`` `[`replicate`](https://rdrr.io/r/base/lapply.html)`(``N_sites``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``K``)``, simplify ``=`` ``FALSE``)`\
`    ``site_u`` ``<-`` `[`replicate`](https://rdrr.io/r/base/lapply.html)`(``N_sites``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``K``)``, simplify ``=`` ``FALSE``)`\
`    ``z_curr`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``K``)``; ``trajectory`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`\
`    ``for`` ``(``iter`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``MAX_ITER``)``)`` ``{`\
`        ``for`` ``(``i`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``N_sites``)``)`` ``{`\
`            `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``sites_problem``[[``i``]``]``$``zp``)`` ``<-`` ``z_curr`\
`            `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``sites_problem``[[``i``]``]``$``up``)`` ``<-`` ``site_u``[[``i``]``]`\
`            `[`suppressMessages`](https://rdrr.io/r/base/message.html)`(`[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(`\
`                `[`psolve`](https://www.cvxgrp.org/CVXR/reference/psolve.html)`(``sites_problem``[[``i``]``]``$``prob``, solver ``=`` ``"CLARABEL"``,`\
`                       verbose ``=`` ``FALSE``)``)``)`\
`            ``site_x``[[``i``]``]`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``sites_problem``[[``i``]``]``$``x``)``)`\
`        ``}`\
`        ``w_avg``  ``<-`` ``consensus``(``site_x``, ``site_u``)`\
`        ``z_new``  ``<-`` ``soft_threshold``(``w_avg``, ``LAMBDA`` ``/`` ``(``N_sites`` ``*`` ``RHO``)``)`\
`        ``site_u`` ``<-`` `[`Map`](https://rdrr.io/r/base/funprog.html)`(``function``(``u``, ``x``)`` ``u`` ``+`` ``(``x`` ``-`` ``z_new``)``, ``site_u``, ``site_x``)`\
`        ``primal`` ``<-`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`mean`](https://rdrr.io/r/base/mean.html)`(`[`vapply`](https://rdrr.io/r/base/lapply.html)`(``site_x``, ``function``(``x``)`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``(``x`` ``-`` ``z_new``)``^``2``)``, ``0``)``)``)`\
`        ``dual``   ``<-`` ``RHO`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`sum`](https://rdrr.io/r/base/sum.html)`(``(``z_new`` ``-`` ``z_curr``)``^``2``)``)`\
`        ``z_curr`` ``<-`` ``z_new``; ``trajectory``[[``iter``]``]`` ``<-`` ``z_new`\
`        ``if`` ``(``primal`` ``<`` ``TOL`` ``&&`` ``dual`` ``<`` ``TOL``)`` ``break`\
`    ``}`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``z ``=`` ``z_curr``, trajectory ``=`` ``trajectory``)`\
`}`\
\
`plain_consensus`` ``<-`` ``function``(``site_x``, ``site_u``)`\
`    `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`Map`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``site_x``, ``site_u``)``)`` ``/`` `[`length`](https://rdrr.io/r/base/length.html)`(``site_x``)`\
\
`ref``   ``<-`` ``run_admm``(``sites_problem``, ``plain_consensus``)`\
`z_ref`` ``<-`` ``ref``$``z`

In the clear the consensus is a single line — the average of the
\\(x_k+u_k)\\ vectors. The unencrypted ADMM converges in 147 iterations
and matches the centralized [CVXR](https://cvxr.rbind.io) fit to
8.3 × 10⁻⁴ in maximum absolute coefficient difference, so `agg_beta` —
equivalently `z_ref` — is the target the encrypted protocol must
reproduce.

## The same fit under threshold FHE

Only three quantities ever cross a site boundary: the standardization
moments \\(S_k,Q_k)\\, the screening statistics \\(U^{(k)},I^{(k)})\\,
and, at each ADMM iteration, the consensus sum \\\sum_k(x_k+u_k)\\. Each
is a sum over sites, so each becomes one round of the same threshold-FHE
summation primitive the master/worker fits use: every site encrypts its
contribution under the joint public key, the aggregator adds the
encrypted contributions, and the total is recovered by \\n\\-of-\\n\\
partial decryption. The local [CVXR](https://cvxr.rbind.io) work and
`run_admm` are untouched.

We use the same threshold infrastructure as `cox-threshold.Rmd`: a CKKS
context with `Feature$MULTIPARTY` and
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md),
which walks the
[`multiparty_key_gen()`](https://openfheorg.github.io/openfhe.R/reference/multiparty_key_gen.html)
chain across the sites. Each site generates its own share and keeps it;
only public keys move along the chain, so no party — the aggregator
included — ever holds the joint secret.

So the sites have to exist before the aggregator does. Here we give each
subgroup a site object to hold its share.

\
`cc`` ``<-`` `[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`\
`                  multiplicative_depth ``=`` ``1L``,`\
`                  scaling_mod_size     ``=`` ``59L``,`\
`                  first_mod_size       ``=`` ``60L``,`\
`                  batch_size           ``=`` ``8192L``,`\
`                  features             ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``Feature``$``MULTIPARTY``)``)`\
\
`key_sites`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`levels`](https://rdrr.io/r/base/levels.html)`(``dlbcl``$``Subgroup``)``, ``function``(``s``)`\
`    `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``s``, data ``=`` ``NULL``, contribution_fn ``=`` ``function``(``data``, ``theta``)`` ``NULL``)``)`\
\
`master`` ``<-`` `[`make_threshold_master`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)`(``"Aggregator"``,`\
`                                crypto_context ``=`` ``cc``, sites ``=`` ``key_sites``)`\
\
`## What each site kept from that one exchange: its own secret share,`\
`## and a copy of the public parameters -- the crypto context and the`\
`` ## joint public key, and no share of anyone else's. `site_params()` ``\
`## asks a site what it holds; it involves no aggregator.`\
`pub`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``key_sites``, ``site_params``)`

Apart from the encrypted values in each round, this setup is the only
exchange between the aggregator and the sites. Everything below encrypts
with `pub`, which each site already holds.

Each element of `pub` is an `OpenFHEParams` object. Its class has two
properties, the crypto context and the joint public key, and none for a
key share:

\
`homomorpheR``::`[`OpenFHEParams`](https://bnaras.github.io/homomorpheR/reference/OpenFHEParams.md)

    ## <homomorpheR::OpenFHEParams> class
    ## @ parent     : <homomorpheR::PublicParams>
    ## @ constructor: function(cc, pk) {...}
    ## @ validator  : <NULL>
    ## @ properties :
    ##  $ cc: <openfhe.R::CryptoContext>
    ##  $ pk: <openfhe.R::PublicKey>

The standardization round is `pool_plain` with the two `colSums`
encrypted: each site encrypts \\S_k\\ and \\Q_k\\, the aggregator sums
under encryption and threshold-decrypts the pooled moments. (For brevity
we treat \\N\_{\mathrm{tot}}\\ as known to the aggregator; hiding the
per-site head-counts is one more sum of the same kind.)

\
`## Site side. Each site forms its own moments and encrypts them under`\
`## the joint key; what leaves is encrypted. Encrypting at the`\
`## aggregator instead would mean handing it the per-site column sums in`\
`## the clear first, which is the disclosure this round exists to avoid.`\
`site_moments`` ``<-`` ``function``(``s``, ``params``)`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``sum   ``=`` `[`encrypt`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)`(``params``, `[`colSums`](https://rdrr.io/r/base/colSums.html)`(``s``$``X``)``)``,`\
`         sumsq ``=`` `[`encrypt`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)`(``params``, `[`colSums`](https://rdrr.io/r/base/colSums.html)`(``s``$``X``^``2``)``)``)`\
\
`## Aggregator side. It reduces encrypted values and decrypts only the total.`\
`encrypt_pool`` ``<-`` ``function``(``master``, ``sites``, ``n_total``, ``p_raw``)`` ``{`\
`    ``## Each site encrypts with the parameters it kept from wiring.`\
`    ``parts`` ``<-`` `[`Map`](https://rdrr.io/r/base/funprog.html)`(``site_moments``, ``sites``, ``pub``)`\
`    ``pooled_sum``   ``<-`` `[`decrypt`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)`(`\
`        ``master``, `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``parts``, ``` `[[` ```, ``"sum"``)``)``,   len ``=`` ``p_raw``)`\
`    ``pooled_sumsq`` ``<-`` `[`decrypt`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)`(`\
`        ``master``, `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``parts``, ``` `[[` ```, ``"sumsq"``)``)``, len ``=`` ``p_raw``)`\
`    ``mu``     ``<-`` ``pooled_sum`` ``/`` ``n_total`\
`    ``sigma2`` ``<-`` `[`pmax`](https://rdrr.io/r/base/Extremes.html)`(``pooled_sumsq`` ``/`` ``n_total`` ``-`` ``mu``^``2``, ``.Machine``$``double.eps``)`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``mu ``=`` ``mu``, sigma ``=`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``sigma2``)``)`\
`}`\
`fhe_pool`` ``<-`` ``encrypt_pool``(``master``, ``sites_raw``, ``N_total``, ``P_raw``)`

The encrypted moments agree with the unencrypted `pool` to 4.1 × 10⁻¹⁶
(mean) and 3.0 × 10⁻¹⁵ (SD), the CKKS approximation error. The screening
round works the same way: the `score_info_at_zero` summands
\\(U^{(k)},I^{(k)})\\ are encrypted and summed, and we confirm it
selects the same probes.

\
`## Site side: compute the score and information at beta = 0 on the`\
`## site's own rows, and encrypt both before returning them.`\
`site_score_info`` ``<-`` ``function``(``s``, ``params``)`` ``{`\
`    ``z`` ``<-`` ``score_info_at_zero``(``s``$``X``, ``s``$``time``, ``s``$``status``)`\
`    `[`list`](https://rdrr.io/r/base/list.html)`(``U ``=`` `[`encrypt`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)`(``params``, ``z``$``U``)``, I ``=`` `[`encrypt`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)`(``params``, ``z``$``I``)``)`\
`}`\
\
`## Aggregator side: sum the encrypted (U, I) and decrypt the totals.`\
`encrypt_screen`` ``<-`` ``function``(``master``, ``sites``, ``p_raw``, ``K``)`` ``{`\
`    ``UI``  ``<-`` `[`Map`](https://rdrr.io/r/base/funprog.html)`(``site_score_info``, ``sites``, ``pub``)`\
`    ``U``   ``<-`` `[`decrypt`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)`(``master``, `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``UI``, ``` `[[` ```, ``"U"``)``)``,`\
`                   len ``=`` ``p_raw``)`\
`    ``I``   ``<-`` `[`decrypt`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)`(``master``, `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``UI``, ``` `[[` ```, ``"I"``)``)``,`\
`                   len ``=`` ``p_raw``)`\
`    ``Z``   ``<-`` ``U`` ``/`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`pmax`](https://rdrr.io/r/base/Extremes.html)`(``I``, ``.Machine``$``double.eps``)``)`\
`    `[`order`](https://rdrr.io/r/base/order.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``Z``)``, decreasing ``=`` ``TRUE``)``[`[`seq_len`](https://rdrr.io/r/base/seq.html)`(``K``)``]`\
`}`\
`fhe_top`` ``<-`` ``encrypt_screen``(``master``, ``sites_std``, ``P_raw``, ``K``)`\
[`stopifnot`](https://rdrr.io/r/base/stopifnot.html)`(`[`setequal`](https://rdrr.io/r/base/sets.html)`(``fhe_top``, ``top_idx``)``)``   ``# same probes as the cleartext screen`

Standardization and screening give the same design as in the clear, so
the consensus round is the only piece left to encrypt. It mirrors
`plain_consensus`. Each site encrypts its own \\(x_k+u_k)\\. The
aggregator sums the encrypted values, scales by \\1/N\\ (one
multiplication by an unencrypted constant, which uses one level of
multiplicative depth), and threshold-decrypts the length-\\K\\ average.
Soft-thresholding is applied to that average in the clear at the
aggregator.

\
`## Site side: form x_k + u_k and encrypt it there. The per-site vector`\
`## never exists in the clear outside this function.`\
`site_consensus_term`` ``<-`` ``function``(``x_k``, ``u_k``, ``params``)`\
`    `[`encrypt`](https://openfheorg.github.io/openfhe.R/reference/encrypt.html)`(``params``, ``x_k`` ``+`` ``u_k``)`\
\
`## Aggregator side: add the encrypted values, scale by 1/N, decrypt the`\
`## average. It sees no individual (x_k + u_k).`\
`encrypted_consensus`` ``<-`` ``function``(``site_x``, ``site_u``)`` ``{`\
`    ``cts``    ``<-`` `[`Map`](https://rdrr.io/r/base/funprog.html)`(``site_consensus_term``, ``site_x``, ``site_u``, ``pub``)`\
`    ``ct_avg`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``cts``)`` ``*`` ``(``1`` ``/`` `[`length`](https://rdrr.io/r/base/length.html)`(``site_x``)``)`\
`    `[`decrypt`](https://openfheorg.github.io/openfhe.R/reference/decrypt.html)`(``master``, ``ct_avg``, len ``=`` ``K``)`\
`}`\
`fhe``        ``<-`` ``run_admm``(``sites_problem``, ``encrypted_consensus``)`\
`z_curr``     ``<-`` ``fhe``$``z`\
`trajectory`` ``<-`` ``fhe``$``trajectory`

The only change is passing `encrypted_consensus` in place of
`plain_consensus`. The encrypted ADMM ran for 147 iterations, and its
coefficients differ from the unencrypted run by at most 2.0 × 10⁻⁷, the
CKKS approximation error.

## Comparison with the centralized fit

We compare the threshold-FHE consensus to the centralized
[CVXR](https://cvxr.rbind.io) fit on both the standardized scale (where
ADMM lives) and the back-transformed original gene-expression scale.

\
`beta_orig_agg`` ``<-`` ``agg_beta`` ``/`` ``sigma_K`\
`beta_orig_enc`` ``<-`` ``z_enc``    ``/`` ``sigma_K`\
`cmp`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`    check.names    ``=`` ``FALSE``,`\
`    Scale          ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"standardized"``, ``"original"``)``,`\
``     `Max abs diff`  ```=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_enc`` ``-`` ``agg_beta``)``)``,`\
`                       `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``beta_orig_enc`` ``-`` ``beta_orig_agg``)``)``)``,`\
``     `L1 diff`       ```=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`sum`](https://rdrr.io/r/base/sum.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_enc`` ``-`` ``agg_beta``)``)``,`\
`                       `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``beta_orig_enc`` ``-`` ``beta_orig_agg``)``)``)``)`\
`ktab``(``cmp``, digits ``=`` ``4``,`\
`             col.names ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Scale"``,`\
`                           ``"$\\max_j \\lvert \\hat\\beta_j^{\\text{ADMM}} - \\hat\\beta_j^{\\text{centralized}} \\rvert$"``,`\
`                           ``"$\\lVert \\hat\\beta^{\\text{ADMM}} - \\hat\\beta^{\\text{centralized}} \\rVert_1$"``)``,`\
`             caption ``=`` ``"Threshold-FHE consensus ADMM vs. centralized Cox-lasso"``)`

| Scale | \\\max_j \lvert \hat\beta_j^{\text{ADMM}} - \hat\beta_j^{\text{centralized}} \rvert\\ | \\\lVert \hat\beta^{\text{ADMM}} - \hat\beta^{\text{centralized}} \rVert_1\\ |
|:---|---:|---:|
| standardized | 0.0008 | 0.0043 |
| original | 0.0016 | 0.0071 |

Threshold-FHE consensus ADMM vs. centralized Cox-lasso {.table .table
.table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

\
`n_agg``   ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``agg_beta``)`` ``>`` ``1e-7``)`\
`n_enc``   ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_enc``)``    ``>`` ``1e-7``)`\
`n_inter`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``agg_beta``)`` ``>`` ``1e-7``)`` ``&`` ``(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_enc``)`` ``>`` ``1e-7``)``)`

The active set has 38 nonzero coefficients in the centralized fit and 38
in the encrypted ADMM fit; the intersection is 38 — every probe selected
by the centralized fit is recovered by the encrypted distributed
protocol.

The figure below shows the consensus trajectory \\z^t\\ for the eight
probes with largest \\\|z\|\\ at convergence, with the centralized fit
drawn as a horizontal reference. The encrypted iterates converge along
the path the centralized solver would take.

\
`top8`` ``<-`` `[`order`](https://rdrr.io/r/base/order.html)`(``-`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_enc``)``)``[``1``:``8``]`\
`labs`` ``<-`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``"probe"``, `[`colnames`](https://rdrr.io/r/base/colnames.html)`(``DLBCL_gex``)``[``top_idx``]``[``top8``]``)`\
`op`` ``<-`` `[`par`](https://rdrr.io/r/graphics/par.html)`(``mfrow ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2``, ``4``)``, mar ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``4``, ``4``, ``2``, ``1``)``, cex ``=`` ``0.8``)`\
`for`` ``(``j`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``top8``)``)`` ``{`\
`    ``vals`` ``<-`` `[`vapply`](https://rdrr.io/r/base/lapply.html)`(``trajectory``, ``function``(``z``)`` ``z``[``top8``[``j``]``]``, `[`numeric`](https://rdrr.io/r/base/numeric.html)`(``1``)``)`\
`    `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(`[`seq_along`](https://rdrr.io/r/base/seq.html)`(``vals``)``, ``vals``, type ``=`` ``"l"``, lwd ``=`` ``1.6``, col ``=`` ``"steelblue4"``,`\
`         xlab ``=`` ``"ADMM iteration"``, ylab ``=`` `[`expression`](https://rdrr.io/r/base/expression.html)`(``z``^``t``)``, main ``=`` ``labs``[``j``]``)`\
`    `[`abline`](https://rdrr.io/r/graphics/abline.html)`(``h ``=`` ``agg_beta``[``top8``[``j``]``]``, lty ``=`` ``2``)`\
`}`

![Consensus trajectories for the eight largest-magnitude coefficients.
Solid lines are the encrypted ADMM iterates; dashed lines are the
centralized CVXR fit. Standardized
scale.](cvxr-cox-lasso-dlbcl_files/figure-html/cox-fig-1.png)

Consensus trajectories for the eight largest-magnitude coefficients.
Solid lines are the encrypted ADMM iterates; dashed lines are the
centralized CVXR fit. Standardized scale.

\
[`par`](https://rdrr.io/r/graphics/par.html)`(``op``)`

## What the protocol hides and reveals

The aggregator learns the pooled per-probe mean and SD (round 1), a
per-probe stratified univariate Cox \\\|Z\|\\ statistic and the indices
of the top-100 screened probes (round 2), and the consensus \\z^t\\ at
every ADMM iteration. Each is an aggregated population-level statistic
over 235 patients, not patient-level data. None of the per-site design
matrices \\X_k\\, the per-site \\(\mu,\sigma^2)\\ contributions, the
per-site \\(U,I)\\ contributions, or the per-site \\(x_k+u_k)\\ vectors
ever appear in cleartext anywhere in the protocol. Decryption at every
step is \\n\\-of-\\n\\ threshold: no single party — the aggregator
included — can recover any intermediate quantity unilaterally.

Two steps in this vignette would not exist in a real deployment. The
unencrypted reference pass forms the per-site averages that the protocol
keeps encrypted, and the centralized solve pools all the data. We can
run both only because this vignette holds all three sites’ data in one R
session. The choice \\\rho = 50\\ depends on them too: it was kept
because it agreed best with the centralized solve, which a deployment
cannot compute.

A deployment can instead choose \\\rho\\ on a surrogate cohort built
from public design facts, as
[`vignette("cvxr-consensus-admm-dp")`](https://bnaras.github.io/homomorpheR/articles/cvxr-consensus-admm-dp.md)
does. When the protocol also spends a privacy budget, as in
[`vignette("cvxr-consensus-admm-dp")`](https://bnaras.github.io/homomorpheR/articles/cvxr-consensus-admm-dp.md),
encrypting the sweep is not enough, and the constants have to be chosen
from data that does not count against the budget.

## Discussion

1.  **CVXR problems work unchanged.** Each site solves its local
    Cox-lasso problem in the clear. Only the consensus update is
    encrypted. The [CVXR](https://cvxr.rbind.io)
    `Problem(Minimize(...))` is the same as in the clear, and `run_admm`
    is called with `encrypted_consensus` instead of `plain_consensus`.
2.  **Encryption does not change the fit.** The encrypted fit matches
    the unencrypted ADMM to 2.0 × 10⁻⁷ and has the same active set. The
    remaining gap to the centralized solve (8.3 × 10⁻⁴) comes from
    stopping ADMM at a finite tolerance, not from the encryption.
3.  **No single party holds the secret key.** Each site generated its
    own share during key generation and kept it. The aggregator holds
    only the joint public key. No single party can decrypt an
    intermediate value, and each round’s result is decrypted only when
    every site contributes a partial decryption.
4.  **DPP keeps the inner loop fast.** Each site’s
    [CVXR](https://cvxr.rbind.io) problem is built once at setup; ADMM
    iterations only update the `Parameter` values.

## Limitations

- **Honest-but-curious trust.** A site that misreports its local
  \\(x_k+u_k)\\ can corrupt the consensus; detecting this needs
  commitments / zero-knowledge proofs not implemented here.
- **The aggregator sees the trajectory \\\\z^t\\\\.** Per-iteration
  consensus values are revealed in the clear (after fusion) so the loop
  can decide convergence.
- **No output privacy.** The released \\\hat\beta = z^\star\\ is the
  same coefficient vector as the centralized fit; output-level
  protection is out of scope here and is demonstrated in the companion
  DP vignette.

Bayle, Pierre, Jianqing Fan, and Zhipeng Lou. 2025.
“Communication-Efficient Distributed Estimation and Inference for Cox’s
Model.” *Journal of the American Statistical Association*, ahead of
print. <https://doi.org/10.1080/01621459.2025.2516820>.

Rosenwald, Andreas, George Wright, Wing C. Chan, et al. 2002. “The Use
of Molecular Profiling to Predict Survival After Chemotherapy for
Diffuse Large-B-Cell Lymphoma.” *New England Journal of Medicine* 346
(25): 1937–47. <https://doi.org/10.1056/NEJMoa012914>.
