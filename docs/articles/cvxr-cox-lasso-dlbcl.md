# Federated Cox-Lasso via Consensus ADMM on DLBCL (Threshold CKKS)

## Introduction

The companion vignette [Federated Consensus
ADMM](https://bnaras.github.io/homomorpheR/articles/cvxr-consensus-admm.md)
develops the threshold-FHE consensus-ADMM protocol on a small simulated
L2-regularized logistic problem ($`p = 4`$, three sites). This vignette
takes the same protocol to a real, high-dimensional survival problem: a
stratified **Cox lasso** on the diffuse large-B-cell lymphoma (DLBCL)
gene-expression cohort of Rosenwald et al. (2002), the dataset Bayle et
al. (2025) use to motivate distributed Cox estimation.

The lesson is the same as in the `cox` and `cox-threshold` vignettes —
*function-call* optimizers such as
[`mle()`](https://rdrr.io/r/stats4/mle.html) or
[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)’s
Newton-Raphson compose with an encrypted master/worker round without the
optimizer noticing — but it does not extend to arbitrary convex
programs. [CVXR](https://cvxr.rbind.io) operates on a *symbolic*
problem: it canonicalizes once and ships the canonical form to a solver,
so we cannot hand it a callback that secretly performs FHE. The remedy
is **federated consensus ADMM** (Boyd, Parikh, Chu, Peleato and
Eckstein, 2011): split the global problem into per-site subproblems each
solved by [CVXR](https://cvxr.rbind.io) in the clear, and use the
encrypted channel only for the cross-site **consensus average**.
Disciplined parametric programming (DPP) keeps the per-site solves fast
because the `Parameter` values change every iteration but the symbolic
structure does not.

We develop the fit in two passes. First we run the whole thing **in the
clear** to fix the target: standardize, screen, and solve the consensus
ADMM with an ordinary plaintext average, checking against the
centralized [CVXR](https://cvxr.rbind.io) solve. Then we put it **under
threshold FHE**, replacing each cross-site sum — the standardization
moments, the screening statistics, and the per-iteration consensus
average — with one round of the encrypted-summation primitive, and
confirm the encrypted fit reproduces the plaintext reference.

> **Reproducibility and verification.** The code chunks in this vignette
> *are* the pipeline; they are gated `eval = RECOMPUTE` and do not run
> when the vignette builds (the encrypted ADMM takes ~150 iterations of
> per-site [CVXR](https://cvxr.rbind.io) solves). The displayed numbers
> come from `data(cvxr_consensus)`, which was produced by extracting
> these very chunks with
> [`knitr::purl()`](https://rdrr.io/pkg/knitr/man/knit.html) and running
> them. To verify the results, do exactly that yourself:
>
> `vig`` ``<-`` `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"doc"``, ``"cvxr-cox-lasso-dlbcl.Rmd"``,`` `` package ``=`` ``"homomorpheR"``)`` ``RECOMPUTE`` ``<-`` ``TRUE`` ``# un-gate the chunks`` ``src`` ``<-`` ``knitr``::`[`purl`](https://rdrr.io/pkg/knitr/man/knit.html)`(``vig``, output ``=`` `[`tempfile`](https://rdrr.io/r/base/tempfile.html)`(``fileext ``=`` ``".R"``)``, quiet ``=`` ``TRUE``)`` `[`source`](https://rdrr.io/r/base/source.html)`(``src``)`` ``# runs the pipeline (minutes)`` ``fresh`` ``<-`` ``cvxr_consensus`` ``# just recomputed`` `[`data`](https://rdrr.io/r/utils/data.html)`(``cvxr_consensus``, package ``=`` ``"homomorpheR"``)`` ``# the shipped copy`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``fresh``$``z_enc`` ``-`` ``cvxr_consensus``$``z_enc``)``)`` ``# ~1e-7`
>
> Threshold key generation is randomized, so a re-run reproduces the
> shipped coefficients up to CKKS approximation noise (~1e-7), not
> bit-for-bit. This
> `purl()`-then-[`source()`](https://rdrr.io/r/base/source.html) is
> exactly what `data-raw/cvxr_consensus.R` does to build the shipped
> object.

## The global problem and its consensus split

The three molecular subgroups (GCB, ABC, Type III) are our sites: they
arise from different cells of origin, have systematically different
prognosis, and are diagnosed at different referral centers, so the
subgroup is a biologically and operationally realistic site boundary.
With $`N`$ sites, per-site standardized design matrices
$`X_k \in \mathbb{R}^{n_k \times p}`$, event times $`t_k`$, status
$`\delta_k`$, a shared coefficient $`\beta \in \mathbb{R}^p`$, and the
per-stratum Cox partial log-likelihood $`\ell_k`$ in Breslow form, the
global problem is

``` math
\min_{\beta \in \mathbb{R}^p}\;
-\sum_{k=1}^{N}\ell_k(\beta)\;+\;\lambda\lVert\beta\rVert_1 .
```

Because the partial likelihood factorizes additively across strata, the
consensus split introduces per-site copies $`x_k`$ and a global
consensus $`z`$:

``` math
\min_{\{x_k\},\,z}\;
\sum_k\bigl(-\ell_k(x_k)\bigr) + \lambda\lVert z\rVert_1
\quad\text{s.t.}\quad x_k = z,\ \forall k,
```

with augmented-Lagrangian iteration

``` math
\begin{aligned}
x_k^{t+1} &= \arg\min_x -\ell_k(x) + \tfrac{\rho}{2}\lVert x - z^t + u_k^t\rVert_2^2,\\
z^{t+1}   &= S_{\lambda/(N\rho)}\!\Bigl(\tfrac{1}{N}\sum_k (x_k^{t+1}+u_k^t)\Bigr),\\
u_k^{t+1} &= u_k^t + (x_k^{t+1}-z^{t+1}),
\end{aligned}
```

where $`S_\tau(v)=\operatorname{sign}(v)\max(|v|-\tau,0)`$ is
soft-thresholding (the proximal map of $`\tau\lVert\cdot\rVert_1`$). The
$`x`$-update is the per-site [CVXR](https://cvxr.rbind.io) solve; the
$`z`$-update needs the cross-site average
$`\bar w=\tfrac1N\sum_k(x_k+u_k)`$ — that average is the only quantity
that traverses the encrypted channel; the soft-threshold is closed-form
at the aggregator and adds no cryptographic depth.

The pipeline needs `survival` and [CVXR](https://cvxr.rbind.io)
alongside the encryption packages.

[`library`](https://rdrr.io/r/base/library.html)`(`[`survival`](https://github.com/therneau/survival)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`CVXR`](https://cvxr.rbind.io)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`openfhe.R`](https://openfheorg.github.io/openfhe.R/)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`homomorpheR`](https://bnaras.github.io/homomorpheR/)`)`

## The federated fit in the clear

The three subgroups are our sites. We load the cohort, split it by
subgroup, and keep each site’s raw expression matrix, event times, and
status.

[`data`](https://rdrr.io/r/utils/data.html)`(``DLBCL``, package ``=`` ``"homomorpheR"``)`` ``# 235 patients: survival + signatures`` `[`data`](https://rdrr.io/r/utils/data.html)`(``DLBCL_gex``, package ``=`` ``"homomorpheR"``)`` ``# 235 patients x 6416 Lymphochip probes`` ``dlbcl`` ``<-`` ``DLBCL`` ``dlbcl``$``Subgroup`` ``<-`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``dlbcl``$``Subgroup``, levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"GCB"``,``"ABC"``,``"Type III"``)``)`` `[`stopifnot`](https://rdrr.io/r/base/stopifnot.html)`(`[`identical`](https://rdrr.io/r/base/identical.html)`(`[`as.character`](https://rdrr.io/r/base/character.html)`(``dlbcl``$``ID``)``, `[`rownames`](https://rdrr.io/r/base/colnames.html)`(``DLBCL_gex``)``)``)`` ``# rows aligned`` `` ``sites_raw`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`levels`](https://rdrr.io/r/base/levels.html)`(``dlbcl``$``Subgroup``)``, ``function``(``s``)`` ``{`` `` ``idx`` ``<-`` `[`which`](https://rdrr.io/r/base/which.html)`(``dlbcl``$``Subgroup`` ``==`` ``s``)`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``name ``=`` ``s``, X ``=`` ``DLBCL_gex``[``idx``, ``]``, time ``=`` ``dlbcl``$``time``[``idx``]``,`` `` status ``=`` ``dlbcl``$``status``[``idx``]``)`` ``}``)`` `[`names`](https://rdrr.io/r/base/names.html)`(``sites_raw``)`` ``<-`` `[`levels`](https://rdrr.io/r/base/levels.html)`(``dlbcl``$``Subgroup``)`` ``N_sites`` ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(``sites_raw``)`` ``N_total`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`vapply`](https://rdrr.io/r/base/lapply.html)`(``sites_raw``, ``function``(``s``)`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``s``$``X``)``, ``1L``)``)`` ``P_raw`` ``<-`` `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``DLBCL_gex``)`

We standardize the features so the L1 penalty applies uniformly across
predictors on different scales. The pooled mean and variance are
additive over sites: site $`k`$ contributes $`S_k=\sum_{i\in k}x_i`$ and
$`Q_k=\sum_{i\in k}x_i^2`$, from which
$`\mu=\tfrac1{N_{\mathrm{tot}}}\sum_k S_k`$ and
$`\sigma^2=\tfrac1{N_{\mathrm{tot}}}\sum_k Q_k-\mu^2`$. In the clear
this is a pair of `colSums`.

`pool_plain`` ``<-`` ``function``(``sites``, ``n_total``)`` ``{`` `` ``s`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``function``(``s``)`` `[`colSums`](https://rdrr.io/r/base/colSums.html)`(``s``$``X``)``)``)`` `` ``q`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``function``(``s``)`` `[`colSums`](https://rdrr.io/r/base/colSums.html)`(``s``$``X``^``2``)``)``)`` `` ``mu`` ``<-`` ``s`` ``/`` ``n_total`` `` ``sigma2`` ``<-`` `[`pmax`](https://rdrr.io/r/base/Extremes.html)`(``q`` ``/`` ``n_total`` ``-`` ``mu``^``2``, ``.Machine``$``double.eps``)`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``mu ``=`` ``mu``, sigma ``=`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``sigma2``)``)`` ``}`` ``pool`` ``<-`` ``pool_plain``(``sites_raw``, ``N_total``)`` ``sites_std`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites_raw``, ``function``(``s``)`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` name ``=`` ``s``$``name``,`` `` X ``=`` `[`sweep`](https://rdrr.io/r/base/sweep.html)`(`[`sweep`](https://rdrr.io/r/base/sweep.html)`(``s``$``X``, ``2``, ``pool``$``mu``, ``"-"``)``, ``2``, ``pool``$``sigma``, ``"/"``)``,`` `` time ``=`` ``s``$``time``, status ``=`` ``s``$``status``)``)`

Solving the full Cox-lasso at $`p = 6416`$ exhausts memory during
[CVXR](https://cvxr.rbind.io) canonicalization, so we pre-screen to the
$`K = 100`$ probes with the strongest univariate association with
survival — the screen-then-fit pattern Bayle et al. (2025) use on this
cohort. For probe $`g`$ on stratum $`k`$ in event-time order with risk
set $`R_i^{(k)}`$ at event $`i`$,
$`U_g^{(k)}=\sum_{i\in k,\delta_i=1}(X_{i,g}-\overline X_{R_i^{(k)},g})`$
is the score and
$`I_g^{(k)}=\sum_{i\in k,\delta_i=1}\widehat{\operatorname{Var}}_{R_i^{(k)}}(X_{:,g})`$
the information; both sum across sites, and the screen ranks probes by
$`|Z_g|=|U_g|/\sqrt{I_g}`$.

`K`` ``<-`` ``100L`` `` ``score_info_at_zero`` ``<-`` ``function``(``X``, ``time``, ``status``)`` ``{`` `` ``p`` ``<-`` `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``X``)`` `` ``ord`` ``<-`` `[`order`](https://rdrr.io/r/base/order.html)`(``time``, ``-``status``)`` `` ``X_o`` ``<-`` ``X``[``ord``, , drop ``=`` ``FALSE``]``; ``stat_o`` ``<-`` ``status``[``ord``]`` `` ``n`` ``<-`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``X_o``)``; ``U`` ``<-`` `[`numeric`](https://rdrr.io/r/base/numeric.html)`(``p``)``; ``I`` ``<-`` `[`numeric`](https://rdrr.io/r/base/numeric.html)`(``p``)`` `` ``for`` ``(``i`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``n``)``)`` ``{`` `` ``if`` ``(``stat_o``[``i``]`` ``==`` ``1L``)`` ``{`` `` ``risk`` ``<-`` ``X_o``[``i``:``n``, , drop ``=`` ``FALSE``]`` `` ``mu_R`` ``<-`` `[`colMeans`](https://rdrr.io/r/base/colSums.html)`(``risk``)`` `` ``U`` ``<-`` ``U`` ``+`` ``(``X_o``[``i``, ``]`` ``-`` ``mu_R``)`` `` ``I`` ``<-`` ``I`` ``+`` `[`colSums`](https://rdrr.io/r/base/colSums.html)`(`[`sweep`](https://rdrr.io/r/base/sweep.html)`(``risk``, ``2``, ``mu_R``, ``"-"``)``^``2``)`` ``/`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``risk``)`` `` ``}`` `` ``}`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``U ``=`` ``U``, I ``=`` ``I``)`` ``}`` `` ``screen_plain`` ``<-`` ``function``(``sites``, ``K``)`` ``{`` `` ``UI`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``function``(``s``)`` ``score_info_at_zero``(``s``$``X``, ``s``$``time``, ``s``$``status``)``)`` `` ``U`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``UI``, ``` `[[` ```, ``"U"``)``)`` `` ``I`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``UI``, ``` `[[` ```, ``"I"``)``)`` `` ``Z`` ``<-`` ``U`` ``/`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`pmax`](https://rdrr.io/r/base/Extremes.html)`(``I``, ``.Machine``$``double.eps``)``)`` `` `[`order`](https://rdrr.io/r/base/order.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``Z``)``, decreasing ``=`` ``TRUE``)``[`[`seq_len`](https://rdrr.io/r/base/seq.html)`(``K``)``]`` ``}`` ``top_idx`` ``<-`` ``screen_plain``(``sites_std``, ``K``)`` ``sites_KS`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites_std``, ``function``(``s``)`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` name ``=`` ``s``$``name``, X ``=`` ``s``$``X``[``, ``top_idx``]``,`` `` time ``=`` ``s``$``time``, status ``=`` ``s``$``status``)``)`` ``sigma_K`` ``<-`` ``pool``$``sigma``[``top_idx``]`

`sites_KS` now holds each stratum on the $`K = 100`$ screened probes,
and `sigma_K` keeps their pooled SDs for the back-transform to the
original gene-expression scale.

The centralized stratified Cox-lasso fit — our ground truth — is a
single [CVXR](https://cvxr.rbind.io) solve summing the per-stratum
Breslow partial likelihoods plus the L1 penalty.

`LAMBDA`` ``<-`` ``5`` ``build_cox_breslow_nll`` ``<-`` ``function``(``beta_var``, ``X_s``, ``time_s``, ``status_s``)`` ``{`` `` ``ord`` ``<-`` `[`order`](https://rdrr.io/r/base/order.html)`(``time_s``, ``-``status_s``)`` `` ``X_o`` ``<-`` ``X_s``[``ord``, , drop ``=`` ``FALSE``]``; ``stat_o`` ``<-`` ``status_s``[``ord``]`` `` ``n`` ``<-`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``X_o``)``; ``eta_o`` ``<-`` ``X_o`` `[`%*%`](https://rdrr.io/r/base/matmult.html)` ``beta_var`` `` ``terms`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`` `` ``for`` ``(``i`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``n``)``)`` ``{`` `` ``if`` ``(``stat_o``[``i``]`` ``==`` ``1L``)`` ``{`` `` ``terms``[[`[`length`](https://rdrr.io/r/base/length.html)`(``terms``)`` ``+`` ``1L``]``]`` ``<-`` `` `[`log_sum_exp`](https://www.cvxgrp.org/CVXR/reference/log_sum_exp.html)`(``eta_o``[``i``:``n``, ``1``]``)`` ``-`` ``eta_o``[``i``, ``1``]`` `` ``}`` `` ``}`` `` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``terms``)`` ``}`` `` ``beta_var`` ``<-`` `[`Variable`](https://www.cvxgrp.org/CVXR/reference/Variable.html)`(``K``, name ``=`` ``"beta"``)`` ``nll_per`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites_KS``, ``function``(``s``)`` `` ``build_cox_breslow_nll``(``beta_var``, ``s``$``X``, ``s``$``time``, ``s``$``status``)``)`` ``agg_prob`` ``<-`` `[`Problem`](https://www.cvxgrp.org/CVXR/reference/Problem.html)`(`[`Minimize`](https://www.cvxgrp.org/CVXR/reference/Minimize.html)`(`[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``nll_per``)`` ``+`` `` ``LAMBDA`` ``*`` `[`p_norm`](https://www.cvxgrp.org/CVXR/reference/p_norm.html)`(``beta_var``, ``1``)``)``)`` `[`suppressMessages`](https://rdrr.io/r/base/message.html)`(`[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(`` `` `[`psolve`](https://www.cvxgrp.org/CVXR/reference/psolve.html)`(``agg_prob``, solver ``=`` ``"CLARABEL"``, verbose ``=`` ``FALSE``)``)``)`` ``agg_beta`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``beta_var``)``)`

The distributed fit splits this objective into per-site subproblems tied
by a consensus variable. Each local problem is built so DPP applies:
$`z`$ and $`u`$ enter as `Parameter`s whose values change every ADMM
iteration while the symbolic structure does not; $`X_k`$, time, status,
and $`\rho`$ are constants.

`RHO`` ``<-`` ``50`` `` ``build_local`` ``<-`` ``function``(``X_k``, ``time_k``, ``status_k``, ``rho``)`` ``{`` `` ``p`` ``<-`` `[`ncol`](https://rdrr.io/r/base/nrow.html)`(``X_k``)`` `` ``x`` ``<-`` `[`Variable`](https://www.cvxgrp.org/CVXR/reference/Variable.html)`(``p``)``; ``zp`` ``<-`` `[`Parameter`](https://www.cvxgrp.org/CVXR/reference/Parameter.html)`(``p``)``; ``up`` ``<-`` `[`Parameter`](https://www.cvxgrp.org/CVXR/reference/Parameter.html)`(``p``)`` `` ``nll`` ``<-`` ``build_cox_breslow_nll``(``x``, ``X_k``, ``time_k``, ``status_k``)`` `` ``aug`` ``<-`` ``(``rho`` ``/`` ``2``)`` ``*`` `[`sum_squares`](https://www.cvxgrp.org/CVXR/reference/sum_squares.html)`(``x`` ``-`` ``zp`` ``+`` ``up``)`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``prob ``=`` `[`Problem`](https://www.cvxgrp.org/CVXR/reference/Problem.html)`(`[`Minimize`](https://www.cvxgrp.org/CVXR/reference/Minimize.html)`(``nll`` ``+`` ``aug``)``)``, x ``=`` ``x``, zp ``=`` ``zp``, up ``=`` ``up``)`` ``}`` ``sites_problem`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites_KS``, ``function``(``s``)`` `` ``build_local``(``s``$``X``, ``s``$``time``, ``s``$``status``, ``RHO``)``)`

The ADMM driver below is the whole federated algorithm, and it is
deliberately agnostic to *how* the cross-site average is formed: the
`consensus` argument is a function of the per-site $`(x_k,u_k)`$
vectors, and everything else — the local [CVXR](https://cvxr.rbind.io)
solves, the soft-threshold $`z`$-update, the dual update, the stopping
rule — is ordinary R. We call it once now with a plaintext average and,
unchanged, once more under encryption.

A note on the constants. We fix $`\rho = 50`$ and a cap of 200
iterations. The dual residual $`\rho\lVert z^{t+1}-z^t\rVert`$ is the
binding term here and decays slowly; with $`\rho = 50`$ the absolute
stopping rule `primal < TOL && dual < TOL` (with `TOL = 0.005`) trips at
iteration 147. Smaller $`\rho`$ reaches the tolerance in fewer
iterations but at a looser fit, so we keep $`\rho = 50`$ for the
tightest agreement with the centralized solve.

`MAX_ITER`` ``<-`` ``200L``; ``TOL`` ``<-`` ``5e-3`` ``soft_threshold`` ``<-`` ``function``(``v``, ``tau``)`` `[`sign`](https://rdrr.io/r/base/sign.html)`(``v``)`` ``*`` `[`pmax`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``v``)`` ``-`` ``tau``, ``0``)`` `` ``run_admm`` ``<-`` ``function``(``sites_problem``, ``consensus``)`` ``{`` `` ``site_x`` ``<-`` `[`replicate`](https://rdrr.io/r/base/lapply.html)`(``N_sites``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``K``)``, simplify ``=`` ``FALSE``)`` `` ``site_u`` ``<-`` `[`replicate`](https://rdrr.io/r/base/lapply.html)`(``N_sites``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``K``)``, simplify ``=`` ``FALSE``)`` `` ``z_curr`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``K``)``; ``trajectory`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`` `` ``for`` ``(``iter`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``MAX_ITER``)``)`` ``{`` `` ``for`` ``(``i`` ``in`` `[`seq_len`](https://rdrr.io/r/base/seq.html)`(``N_sites``)``)`` ``{`` `` `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``sites_problem``[[``i``]``]``$``zp``)`` ``<-`` ``z_curr`` `` `[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``sites_problem``[[``i``]``]``$``up``)`` ``<-`` ``site_u``[[``i``]``]`` `` `[`suppressMessages`](https://rdrr.io/r/base/message.html)`(`[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(`` `` `[`psolve`](https://www.cvxgrp.org/CVXR/reference/psolve.html)`(``sites_problem``[[``i``]``]``$``prob``, solver ``=`` ``"CLARABEL"``,`` `` verbose ``=`` ``FALSE``)``)``)`` `` ``site_x``[[``i``]``]`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`value`](https://www.cvxgrp.org/CVXR/reference/value.html)`(``sites_problem``[[``i``]``]``$``x``)``)`` `` ``}`` `` ``w_avg`` ``<-`` ``consensus``(``site_x``, ``site_u``)`` `` ``z_new`` ``<-`` ``soft_threshold``(``w_avg``, ``LAMBDA`` ``/`` ``(``N_sites`` ``*`` ``RHO``)``)`` `` ``site_u`` ``<-`` `[`Map`](https://rdrr.io/r/base/funprog.html)`(``function``(``u``, ``x``)`` ``u`` ``+`` ``(``x`` ``-`` ``z_new``)``, ``site_u``, ``site_x``)`` `` ``primal`` ``<-`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`mean`](https://rdrr.io/r/base/mean.html)`(`[`vapply`](https://rdrr.io/r/base/lapply.html)`(``site_x``, ``function``(``x``)`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``(``x`` ``-`` ``z_new``)``^``2``)``, ``0``)``)``)`` `` ``dual`` ``<-`` ``RHO`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`sum`](https://rdrr.io/r/base/sum.html)`(``(``z_new`` ``-`` ``z_curr``)``^``2``)``)`` `` ``z_curr`` ``<-`` ``z_new``; ``trajectory``[[``iter``]``]`` ``<-`` ``z_new`` `` ``if`` ``(``primal`` ``<`` ``TOL`` ``&&`` ``dual`` ``<`` ``TOL``)`` ``break`` `` ``}`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``z ``=`` ``z_curr``, trajectory ``=`` ``trajectory``)`` ``}`` `` ``plain_consensus`` ``<-`` ``function``(``site_x``, ``site_u``)`` `` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`Map`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``site_x``, ``site_u``)``)`` ``/`` `[`length`](https://rdrr.io/r/base/length.html)`(``site_x``)`` `` ``ref`` ``<-`` ``run_admm``(``sites_problem``, ``plain_consensus``)`` ``z_ref`` ``<-`` ``ref``$``z`

In the clear the consensus is a single line — the average of the
$`(x_k+u_k)`$ vectors. The plaintext ADMM converges in 147 iterations
and matches the centralized [CVXR](https://cvxr.rbind.io) fit to
8.3^{-4} in maximum absolute coefficient difference, so `agg_beta` —
equivalently `z_ref` — is the target the encrypted protocol must
reproduce.

## The same fit under threshold FHE

Only three quantities ever cross a site boundary: the standardization
moments $`(S_k,Q_k)`$, the screening statistics $`(U^{(k)},I^{(k)})`$,
and, at each ADMM iteration, the consensus sum $`\sum_k(x_k+u_k)`$. Each
is a sum over sites, so each becomes one round of the same threshold-FHE
summation primitive the master/worker fits use: every site encrypts its
contribution under the joint public key, the aggregator adds the
ciphertexts, and the total is recovered by $`n`$-of-$`n`$ partial
decryption. The local [CVXR](https://cvxr.rbind.io) work and `run_admm`
are untouched.

We use the same threshold infrastructure as `cox-threshold.Rmd`: a CKKS
context with `Feature$MULTIPARTY` and
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md),
which runs the chained
[`multiparty_key_gen()`](https://openfheorg.github.io/openfhe.R/reference/multiparty_key_gen.html)
ceremony internally so no single party ever holds the secret key.

`cc`` ``<-`` `[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`` `` multiplicative_depth ``=`` ``1L``,`` `` scaling_mod_size ``=`` ``59L``,`` `` first_mod_size ``=`` ``60L``,`` `` batch_size ``=`` ``8192L``,`` `` features ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``Feature``$``MULTIPARTY``)``)`` ``master`` ``<-`` `[`make_threshold_master`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)`(``"Aggregator"``,`` `` crypto_context ``=`` ``cc``, n_sites ``=`` ``N_sites``)`

The standardization round is `pool_plain` with the two `colSums`
encrypted: each site encrypts $`S_k`$ and $`Q_k`$, the aggregator sums
under encryption and threshold-decrypts the pooled moments. (For brevity
we treat $`N_{\mathrm{tot}}`$ as known to the aggregator; hiding the
per-site head-counts is one more sum of the same kind.)

`encrypt_pool`` ``<-`` ``function``(``master``, ``sites``, ``n_total``, ``p_raw``)`` ``{`` `` ``s_ct`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``function``(``s``)`` `[`master_encrypt`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md)`(``master``, `[`colSums`](https://rdrr.io/r/base/colSums.html)`(``s``$``X``)``)``)`` `` ``q_ct`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``function``(``s``)`` `[`master_encrypt`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md)`(``master``, `[`colSums`](https://rdrr.io/r/base/colSums.html)`(``s``$``X``^``2``)``)``)`` `` ``pooled_sum`` ``<-`` `[`master_decrypt`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)`(``master``, `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``s_ct``)``, len ``=`` ``p_raw``)`` `` ``pooled_sumsq`` ``<-`` `[`master_decrypt`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)`(``master``, `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``q_ct``)``, len ``=`` ``p_raw``)`` `` ``mu`` ``<-`` ``pooled_sum`` ``/`` ``n_total`` `` ``sigma2`` ``<-`` `[`pmax`](https://rdrr.io/r/base/Extremes.html)`(``pooled_sumsq`` ``/`` ``n_total`` ``-`` ``mu``^``2``, ``.Machine``$``double.eps``)`` `` `[`list`](https://rdrr.io/r/base/list.html)`(``mu ``=`` ``mu``, sigma ``=`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``sigma2``)``)`` ``}`` ``fhe_pool`` ``<-`` ``encrypt_pool``(``master``, ``sites_raw``, ``N_total``, ``P_raw``)`

The encrypted moments agree with the plaintext `pool` to 4.4^{-16}
(mean) and 3.1^{-15} (SD) — essentially machine precision, since these
are exact sums under CKKS. The screening round is structurally
identical, the same `score_info_at_zero` summands $`(U^{(k)},I^{(k)})`$
encrypted and summed the same way, so we show it compactly and confirm
it selects the same probes.

`encrypt_screen`` ``<-`` ``function``(``master``, ``sites``, ``p_raw``, ``K``)`` ``{`` `` ``enc`` ``<-`` ``function``(``v``)`` `[`master_encrypt`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md)`(``master``, ``v``)`` `` ``UI`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sites``, ``function``(``s``)`` ``score_info_at_zero``(``s``$``X``, ``s``$``time``, ``s``$``status``)``)`` `` ``U`` ``<-`` `[`master_decrypt`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)`(``master``, `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``UI``, ``function``(``z``)`` ``enc``(``z``$``U``)``)``)``,`` `` len ``=`` ``p_raw``)`` `` ``I`` ``<-`` `[`master_decrypt`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)`(``master``, `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``UI``, ``function``(``z``)`` ``enc``(``z``$``I``)``)``)``,`` `` len ``=`` ``p_raw``)`` `` ``Z`` ``<-`` ``U`` ``/`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(`[`pmax`](https://rdrr.io/r/base/Extremes.html)`(``I``, ``.Machine``$``double.eps``)``)`` `` `[`order`](https://rdrr.io/r/base/order.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``Z``)``, decreasing ``=`` ``TRUE``)``[`[`seq_len`](https://rdrr.io/r/base/seq.html)`(``K``)``]`` ``}`` ``fhe_top`` ``<-`` ``encrypt_screen``(``master``, ``sites_std``, ``P_raw``, ``K``)`` `[`stopifnot`](https://rdrr.io/r/base/stopifnot.html)`(`[`setequal`](https://rdrr.io/r/base/sets.html)`(``fhe_top``, ``top_idx``)``)`` ``# same probes as the plaintext screen`

With standardization and screening recovering the same design, the
consensus round is the only piece left to encrypt. It mirrors
`plain_consensus`: encrypt each $`(x_k+u_k)`$, sum the ciphertexts,
scale by $`1/N`$ under encryption (one ciphertext-plaintext multiply,
depth 1), and threshold-decrypt the length-$`K`$ average.
Soft-thresholding stays in the clear at the aggregator.

`encrypted_consensus`` ``<-`` ``function``(``site_x``, ``site_u``)`` ``{`` `` ``cts`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`seq_along`](https://rdrr.io/r/base/seq.html)`(``site_x``)``, ``function``(``i``)`` `` `[`master_encrypt`](https://bnaras.github.io/homomorpheR/reference/master_encrypt.md)`(``master``, ``site_x``[[``i``]``]`` ``+`` ``site_u``[[``i``]``]``)``)`` `` ``ct_avg`` ``<-`` `[`Reduce`](https://rdrr.io/r/base/funprog.html)`(``` `+` ```, ``cts``)`` ``*`` ``(``1`` ``/`` `[`length`](https://rdrr.io/r/base/length.html)`(``site_x``)``)`` `` `[`master_decrypt`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)`(``master``, ``ct_avg``, len ``=`` ``K``)`` ``}`` ``fhe`` ``<-`` ``run_admm``(``sites_problem``, ``encrypted_consensus``)`` ``z_curr`` ``<-`` ``fhe``$``z`` ``trajectory`` ``<-`` ``fhe``$``trajectory`

Passing `encrypted_consensus` in place of `plain_consensus` is the
*entire* change. The encrypted ADMM ran for 147 iterations and lands on
the same coefficients as the plaintext run, differing by only 1.4^{-7} —
the CKKS approximation noise.

## Comparison with the centralized fit

We compare the threshold-FHE consensus to the centralized
[CVXR](https://cvxr.rbind.io) fit on both the standardized scale (where
ADMM lives) and the back-transformed original gene-expression scale.

`beta_orig_agg`` ``<-`` ``agg_beta`` ``/`` ``sigma_K`` ``beta_orig_enc`` ``<-`` ``z_enc`` ``/`` ``sigma_K`` ``cmp`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`` `` check.names ``=`` ``FALSE``,`` `` Scale ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"standardized"``, ``"original"``)``,`` ```  `Max abs diff`  ```=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_enc`` ``-`` ``agg_beta``)``)``,`` `` `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``beta_orig_enc`` ``-`` ``beta_orig_agg``)``)``)``,`` ```  `L1 diff`  ```=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`sum`](https://rdrr.io/r/base/sum.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_enc`` ``-`` ``agg_beta``)``)``,`` `` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``beta_orig_enc`` ``-`` ``beta_orig_agg``)``)``)``)`` ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``cmp``, digits ``=`` ``4``,`` `` caption ``=`` ``"Threshold-FHE consensus ADMM vs. centralized Cox-lasso"``)`

| Scale        | Max abs diff | L1 diff |
|:-------------|-------------:|--------:|
| standardized |       0.0008 |  0.0043 |
| original     |       0.0016 |  0.0071 |

Threshold-FHE consensus ADMM vs. centralized Cox-lasso {.table}

`n_agg`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``agg_beta``)`` ``>`` ``1e-7``)`` ``n_enc`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_enc``)`` ``>`` ``1e-7``)`` ``n_inter`` ``<-`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``agg_beta``)`` ``>`` ``1e-7``)`` ``&`` ``(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_enc``)`` ``>`` ``1e-7``)``)`

The active set has 38 nonzero coefficients in the centralized fit and 38
in the encrypted ADMM fit; the intersection is 38 — every probe selected
by the centralized fit is recovered by the encrypted distributed
protocol.

The figure below shows the consensus trajectory $`z^t`$ for the eight
probes with largest $`|z|`$ at convergence, with the centralized fit
drawn as a horizontal reference. The encrypted iterates converge along
the path the centralized solver would take.

`top8`` ``<-`` `[`order`](https://rdrr.io/r/base/order.html)`(``-`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``z_enc``)``)``[``1``:``8``]`` ``labs`` ``<-`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``"probe"``, `[`colnames`](https://rdrr.io/r/base/colnames.html)`(``DLBCL_gex``)``[``top_idx``]``[``top8``]``)`` ``op`` ``<-`` `[`par`](https://rdrr.io/r/graphics/par.html)`(``mfrow ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2``, ``4``)``, mar ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``4``, ``4``, ``2``, ``1``)``, cex ``=`` ``0.8``)`` ``for`` ``(``j`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``top8``)``)`` ``{`` `` ``vals`` ``<-`` `[`vapply`](https://rdrr.io/r/base/lapply.html)`(``trajectory``, ``function``(``z``)`` ``z``[``top8``[``j``]``]``, `[`numeric`](https://rdrr.io/r/base/numeric.html)`(``1``)``)`` `` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(`[`seq_along`](https://rdrr.io/r/base/seq.html)`(``vals``)``, ``vals``, type ``=`` ``"l"``, lwd ``=`` ``1.6``, col ``=`` ``"steelblue4"``,`` `` xlab ``=`` ``"ADMM iteration"``, ylab ``=`` `[`expression`](https://rdrr.io/r/base/expression.html)`(``z``^``t``)``, main ``=`` ``labs``[``j``]``)`` `` `[`abline`](https://rdrr.io/r/graphics/abline.html)`(``h ``=`` ``agg_beta``[``top8``[``j``]``]``, lty ``=`` ``2``)`` ``}`

![Consensus trajectories for the eight largest-magnitude coefficients.
Solid lines are the encrypted ADMM iterates; dashed lines are the
centralized CVXR fit. Standardized
scale.](cvxr-cox-lasso-dlbcl_files/figure-html/cox-fig-1.png)

Consensus trajectories for the eight largest-magnitude coefficients.
Solid lines are the encrypted ADMM iterates; dashed lines are the
centralized CVXR fit. Standardized scale.

[`par`](https://rdrr.io/r/graphics/par.html)`(``op``)`

## What the protocol hides and reveals

The aggregator learns the pooled per-probe mean and SD (round 1), a
per-probe stratified univariate Cox $`|Z|`$ statistic and the indices of
the top-100 screened probes (round 2), and the consensus $`z^t`$ at
every ADMM iteration. Each is an aggregated population-level statistic
over 235 patients, not patient-level data. None of the per-site design
matrices $`X_k`$, the per-site $`(\mu,\sigma^2)`$ contributions, the
per-site $`(U,I)`$ contributions, or the per-site $`(x_k+u_k)`$ vectors
ever appear in cleartext anywhere in the protocol. Decryption at every
step is $`n`$-of-$`n`$ threshold: no single party — the aggregator
included — can recover any intermediate quantity unilaterally.

## Discussion

1.  **CVXR symbolic problems compose with threshold FHE.** The local
    Cox-lasso solve runs in the clear at each site; only the cross-site
    consensus update goes through the encrypted channel. The reader does
    not rewrite their [CVXR](https://cvxr.rbind.io) model for the
    encrypted setting — the same `Problem(Minimize(...))` is used
    verbatim, and `run_admm` is called with `encrypted_consensus`
    instead of `plain_consensus`.
2.  **The encrypted layer is lossless to working precision.** The
    encrypted fit reproduces the plaintext ADMM to 1.4^{-7} and recovers
    the identical active set; the residual gap to the one-shot
    centralized solve (8.3^{-4}) is the ADMM iteration budget, not the
    cryptography.
3.  **No single party holds the secret key.**
    [`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
    distributes the secret across all sites; encrypted intermediates are
    undecryptable by any one party, and each round’s result appears only
    after $`n`$-of-$`n`$ partial-decryption fusion.
4.  **DPP keeps the inner loop fast.** Each site’s
    [CVXR](https://cvxr.rbind.io) problem is built once at setup; ADMM
    iterations only update the `Parameter` values.

## Limitations

- **Honest-but-curious trust.** A site that misreports its local
  $`(x_k+u_k)`$ can corrupt the consensus; detecting this needs
  commitments / zero-knowledge proofs not implemented here.
- **The aggregator sees the trajectory $`\{z^t\}`$.** Per-iteration
  consensus values are revealed in plaintext (after fusion) so the loop
  can decide convergence.
- **No output privacy.** The released $`\hat\beta = z^\star`$ is the
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
