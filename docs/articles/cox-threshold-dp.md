# Threshold Cox + Output Differential Privacy (Demonstration)

## Introduction

This vignette is a demonstration. The previous
[`vignette("cox-threshold")`](https://bnaras.github.io/homomorpheR/articles/cox-threshold.md)
fits the Cox model under threshold FHE: the fit matches the cleartext
fit, no single party can decrypt, and the aggregator sees only the joint
log-likelihood \\\ell(\beta)\\ at each optimizer query.

Here we ask what happens if each site also adds noise, so that the
released values satisfy *output differential privacy*. Adding the noise
takes one extra [`rnorm()`](https://rdrr.io/r/stats/Normal.html) call
per site per query. With the optimizers used here, the fits are poor at
any noise level that gives a small \\\varepsilon\\. The vignette runs
the fits and reports the numbers.

## A brief output-DP primer

Given a query \\f: \mathcal{D} \to \mathbb{R}\\ on a dataset \\D\\, the
*Gaussian mechanism* (Dwork and Roth 2014, sec. 3.5.3) releases \\\tilde
f(D) = f(D) + \mathcal{N}(0, \sigma^2)\\. If \\f\\’s *sensitivity* (the
largest change \\f\\ can undergo when one record is added or removed) is
\\\Delta\\, then for any \\\varepsilon \in (0, 1)\\ and \\\delta \> 0\\,
choosing \\\sigma \geq \Delta \sqrt{2\ln(1.25/\delta)}/\varepsilon\\
makes a single release of \\\tilde f\\ satisfy \\(\varepsilon, \delta)\\
differential privacy.

The optimizer issues many queries, so the per-query budget composes. We
use *zCDP composition* (Bun and Steinke 2016): the Gaussian mechanism is
\\\rho\\-zCDP with \\\rho = (\Delta/\sigma)^2/2\\; \\T\\ queries compose
linearly to \\T \cdot \rho\\ in zCDP units; and that converts to
\\(\varepsilon, \delta)\\ via \\\varepsilon = \rho + 2\sqrt{\rho
\log(1/\delta)}\\.

## The protocol

The setup is identical to
[`vignette("cox-threshold")`](https://bnaras.github.io/homomorpheR/articles/cox-threshold.md),
except each site adds an independent Gaussian noise term to its local
contribution before encryption. Since variances of independent normals
add, the decrypted total is \\\ell(\beta) + \mathcal{N}(0, \sigma^2)\\
when each site adds \\\mathcal{N}(0, \sigma^2/N)\\.

## The Cox setup (same DLBCL data as the other Cox vignettes)

\
[`suppressPackageStartupMessages`](https://rdrr.io/r/base/message.html)`(`[`library`](https://rdrr.io/r/base/library.html)`(`[`survival`](https://github.com/therneau/survival)`)``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`homomorpheR`](https://bnaras.github.io/homomorpheR/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(``stats4``)`\
[`data`](https://rdrr.io/r/utils/data.html)`(``DLBCL``)`\
\
`cox_data`` ``<-`` `[`split`](https://rdrr.io/r/base/split.html)`(`\
`  ``DLBCL``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"time"``, ``"status"``, ``"GCB_sig"``, ``"LN_sig"``,`\
`            ``"Prolif_sig"``, ``"BMP6"``, ``"MHC2_sig"``, ``"Subgroup"``)``]``,`\
`  ``DLBCL``$``Subgroup``)`\
\
`agg_model`` ``<-`` `[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html)`(`[`Surv`](https://rdrr.io/pkg/survival/man/Surv.html)`(``time``, ``status``)`` ``~`` ``GCB_sig`` ``+`` ``LN_sig`` ``+`\
`                       ``Prolif_sig`` ``+`` ``BMP6`` ``+`` ``MHC2_sig`` ``+`\
`                       `[`strata`](https://rdrr.io/pkg/survival/man/strata.html)`(``Subgroup``)``,`\
`                   data ``=`` ``DLBCL``)`\
`agg_coef`` ``<-`` `[`coef`](https://rdrr.io/r/stats/coef.html)`(``agg_model``)`\
\
`cph_control`` ``<-`` `[`replace`](https://rdrr.io/r/base/replace.html)`(`[`coxph.control`](https://rdrr.io/pkg/survival/man/coxph.control.html)`(``)``, ``"iter.max"``, ``0``)`\
\
`local_cox_nll`` ``<-`` ``function``(``data``, ``beta``)`` ``{`\
`    ``fit`` ``<-`` `[`tryCatch`](https://rdrr.io/r/base/conditions.html)`(`\
`        `[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html)`(`[`Surv`](https://rdrr.io/pkg/survival/man/Surv.html)`(``time``, ``status``)`` ``~`` ``GCB_sig`` ``+`` ``LN_sig`` ``+`` ``Prolif_sig`` ``+`\
`                  ``BMP6`` ``+`` ``MHC2_sig``,`\
`              data    ``=`` ``data``,`\
`              init    ``=`` ``beta``,`\
`              control ``=`` ``cph_control``)``,`\
`        error ``=`` ``function``(``e``)`` ``NULL``)`\
`    ``if`` ``(`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``fit``)``)`` ``NA_real_`` ``else`` ``-``fit``$``loglik``[``1``]`\
`}`

## Threshold setup and DP-noised workers

The threshold setup is also the same as in `cox-threshold`. The only
change is in each worker’s `contribution_fn`: it adds an independent
\\\mathcal{N}(0, \sigma^2/N)\\ draw to its local nLL before returning
it. The noisy value is then encrypted as usual.

\
`cc`` ``<-`` ``openfhe.R``::`[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`\
`                           multiplicative_depth ``=`` ``1L``,`\
`                           scaling_mod_size     ``=`` ``59L``,`\
`                           first_mod_size       ``=`` ``60L``,`\
`                           batch_size           ``=`` ``8L``,`\
`                           features             ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``openfhe.R``::`[`Feature`](https://openfheorg.github.io/openfhe.R/reference/Feature.html)`$``MULTIPARTY``)``)`\
\
`n_sites`` ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(``cox_data``)`\
\
`build_dp_workers`` ``<-`` ``function``(``sigma``)`` ``{`\
`    `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``cox_data``)``, ``function``(``nm``)`` ``{`\
`        `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(`\
`            ``nm``,`\
`            data     ``=`` ``cox_data``[[``nm``]``]``,`\
`            contribution_fn ``=`` ``function``(``data``, ``beta``)`` ``{`\
`                ``nll`` ``<-`` ``local_cox_nll``(``data``, ``beta``)`\
`                ``if`` ``(`[`is.na`](https://rdrr.io/r/base/NA.html)`(``nll``)``)`` `[`return`](https://rdrr.io/r/base/function.html)`(``NA_real_``)`\
`                ``nll`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``1L``, mean ``=`` ``0``, sd ``=`` ``sigma`` ``/`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``n_sites``)``)`\
`            ``}``)`\
`    ``}``)`\
`}`\
\
`fit_at_sigma`` ``<-`` ``function``(``sigma``, ``method`` ``=`` ``"BFGS"``, ``seed`` ``=`` ``1L``)`` ``{`\
`    `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``seed``)``   ``# stabilize the DP-noise draws across runs`\
`    ``workers`` ``<-`` ``build_dp_workers``(``sigma``)`\
`    ``master``  ``<-`` `[`make_threshold_master`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)`(``"Aggregator"``,`\
`                                     crypto_context ``=`` ``cc``,`\
`                                     sites          ``=`` ``workers``)`\
`    ``dp_nLL`` ``<-`` ``function``(``GCB_sig``, ``LN_sig``, ``Prolif_sig``, ``BMP6``, ``MHC2_sig``)`\
`        `[`master_aggregate`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)`(``master``, `[`c`](https://rdrr.io/r/base/c.html)`(``GCB_sig``, ``LN_sig``, ``Prolif_sig``, ``BMP6``, ``MHC2_sig``)``)`\
`    ``stats4``::`[`mle`](https://rdrr.io/r/stats4/mle.html)`(``dp_nLL``,`\
`                start   ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``GCB_sig ``=`` ``0``, LN_sig ``=`` ``0``, Prolif_sig ``=`` ``0``,`\
`                               BMP6    ``=`` ``0``, MHC2_sig ``=`` ``0``)``,`\
`                method  ``=`` ``method``,`\
`                control ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``reltol ``=`` ``1e-7``)``)`\
`}`

## Mechanical correctness: \\\sigma = 0\\ reproduces `cox-threshold`

When the noise is zero the protocol reduces to the lossless threshold
protocol. The fitted coefficients match
[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) to
threshold-CKKS precision.

\
`fit_clean`` ``<-`` ``fit_at_sigma``(``0``)`\
`clean_check`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`    coefficient ``=`` `[`names`](https://rdrr.io/r/base/names.html)`(``agg_coef``)``,`\
`    cleartext   ``=`` `[`unname`](https://rdrr.io/r/base/unname.html)`(``agg_coef``)``,`\
`    protocol    ``=`` `[`unname`](https://rdrr.io/r/base/unname.html)`(`[`coef`](https://rdrr.io/r/stats/coef.html)`(``fit_clean``)``[`[`names`](https://rdrr.io/r/base/names.html)`(``agg_coef``)``]``)``,`\
`    abs_diff    ``=`` `[`abs`](https://rdrr.io/r/base/MathFun.html)`(`[`unname`](https://rdrr.io/r/base/unname.html)`(`[`coef`](https://rdrr.io/r/stats/coef.html)`(``fit_clean``)``[`[`names`](https://rdrr.io/r/base/names.html)`(``agg_coef``)``]`` ``-`` ``agg_coef``)``)`\
`)`\
`show_clean``(``clean_check``)`

| Coefficient | \\\hat\beta\\, [`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) | \\\hat\beta\\, protocol at \\\sigma = 0\\ | \\\lvert \text{difference} \rvert\\ |
|:---|---:|---:|---:|
| GCB_sig | -0.2638716 | -0.2638698 | \\1.822 \times 10^{-6}\\ |
| LN_sig | -0.2543592 | -0.2543587 | \\5.338 \times 10^{-7}\\ |
| Prolif_sig | 0.3031258 | 0.3031250 | \\7.481 \times 10^{-7}\\ |
| BMP6 | 0.3036375 | 0.3036367 | \\7.937 \times 10^{-7}\\ |
| MHC2_sig | -0.3191467 | -0.3191459 | \\8.417 \times 10^{-7}\\ |

Threshold-DP protocol at \\\sigma = 0\\ vs cleartext
[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) {.table .table
.table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

The maximum absolute coefficient difference at \\\sigma = 0\\ is
1.82 × 10⁻⁶. So any deviation from
[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) below comes
from the noise.

## Increasing noise: where does it break?

We sweep \\\sigma\\ across five orders of magnitude. Each fit is one
full BFGS run over the threshold-DP encrypted nLL. BFGS estimates the
gradient by finite differences of these nLL values.

\
`sigma_grid`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``1e-5``, ``1e-4``, ``1e-3``, ``1e-2``, ``1e-1``, ``1``)`\
\
`sweep_table`` ``<-`` ``function``(``fits``)`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`    sigma        ``=`` ``sigma_grid``,`\
`    n_evals      ``=`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(``fits``, ``function``(``f``)`` ``f``@``details``$``counts``[[``"function"``]``]``)``,`\
`    GCB_sig      ``=`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(``fits``, ``function``(``f``)`` `[`coef`](https://rdrr.io/r/stats/coef.html)`(``f``)``[[``"GCB_sig"``]``]``)``,`\
`    LN_sig       ``=`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(``fits``, ``function``(``f``)`` `[`coef`](https://rdrr.io/r/stats/coef.html)`(``f``)``[[``"LN_sig"``]``]``)``,`\
`    Prolif_sig   ``=`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(``fits``, ``function``(``f``)`` `[`coef`](https://rdrr.io/r/stats/coef.html)`(``f``)``[[``"Prolif_sig"``]``]``)``,`\
`    BMP6         ``=`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(``fits``, ``function``(``f``)`` `[`coef`](https://rdrr.io/r/stats/coef.html)`(``f``)``[[``"BMP6"``]``]``)``,`\
`    MHC2_sig     ``=`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(``fits``, ``function``(``f``)`` `[`coef`](https://rdrr.io/r/stats/coef.html)`(``f``)``[[``"MHC2_sig"``]``]``)``,`\
`    max_abs_diff ``=`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(``fits``, ``function``(``f``)`\
`        `[`max`](https://rdrr.io/r/base/Extremes.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(`[`coef`](https://rdrr.io/r/stats/coef.html)`(``f``)`` ``-`` ``agg_coef``[`[`names`](https://rdrr.io/r/base/names.html)`(`[`coef`](https://rdrr.io/r/stats/coef.html)`(``f``)``)``]``)``)``)``)`\
\
`fits_bfgs``  ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sigma_grid``, ``fit_at_sigma``, method ``=`` ``"BFGS"``)`\
`bfgs_table`` ``<-`` ``sweep_table``(``fits_bfgs``)`\
`show_sweep``(``bfgs_table``, ``"BFGS"``)`

| \\\sigma\\ | Evaluations | GCB_sig | LN_sig | Prolif_sig | BMP6 | MHC2_sig | \\\max_j \lvert \hat\beta_j - \hat\beta_j^{\text{coxph}} \rvert\\ |
|:---|---:|---:|---:|---:|---:|---:|---:|
| \\10^{-5}\\ | 45 | -0.263927 | -0.254319 | 0.303222 | 0.303581 | -0.319180 | 0.000096 |
| \\10^{-4}\\ | 31 | -0.263711 | -0.254310 | 0.301416 | 0.304232 | -0.320419 | 0.001710 |
| \\10^{-3}\\ | 112 | -0.255681 | -0.257783 | 0.284375 | 0.305741 | -0.320735 | 0.018751 |
| \\10^{-2}\\ | 225 | -0.207823 | -0.226972 | 0.346403 | 0.329006 | -0.350293 | 0.056049 |
| \\10^{-1}\\ | 149 | -0.475716 | -0.276219 | 0.506100 | 0.381061 | -0.188904 | 0.211845 |
| \\1\\ | 91 | -0.133843 | -0.389685 | 0.034282 | -0.045055 | -0.336133 | 0.348692 |

BFGS over the threshold-DP nLL at 6 values of \\\sigma\\ {.table .table
.table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

## Nelder–Mead at the same noise scales

The same sweep with Nelder–Mead.

\
`fits_nm``  ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``sigma_grid``, ``fit_at_sigma``, method ``=`` ``"Nelder-Mead"``)`\
`nm_table`` ``<-`` ``sweep_table``(``fits_nm``)`\
`show_sweep``(``nm_table``, ``"Nelder–Mead"``)`

| \\\sigma\\ | Evaluations | GCB_sig | LN_sig | Prolif_sig | BMP6 | MHC2_sig | \\\max_j \lvert \hat\beta_j - \hat\beta_j^{\text{coxph}} \rvert\\ |
|:---|---:|---:|---:|---:|---:|---:|---:|
| \\10^{-5}\\ | 204 | -0.263636 | -0.254196 | 0.302993 | 0.303441 | -0.319003 | 0.000235 |
| \\10^{-4}\\ | 503 | -0.260936 | -0.254388 | 0.300052 | 0.307205 | -0.321379 | 0.003568 |
| \\10^{-3}\\ | 503 | -0.248410 | -0.247910 | 0.338068 | 0.311995 | -0.313197 | 0.034942 |
| \\10^{-2}\\ | 503 | -0.251861 | -0.244768 | 0.344645 | 0.315905 | -0.313427 | 0.041519 |
| \\10^{-1}\\ | 503 | -0.248230 | -0.255815 | 0.352342 | 0.309011 | -0.330805 | 0.049217 |
| \\1\\ | 503 | -0.046219 | -0.169005 | 0.048707 | 0.472601 | -0.343019 | 0.254419 |

Nelder–Mead over the threshold-DP nLL at 6 values of \\\sigma\\ {.table
.table .table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

For both optimizers, fidelity decays monotonically as expected.

## Privacy budget

For the fits at the first three values of \\\sigma\\, with sensitivity
\\\Delta = 1\\ (placeholder) and target \\\delta = 10^{-5}\\, zCDP
composition gives:

\
`zcdp_to_eps`` ``<-`` ``function``(``rho``, ``delta`` ``=`` ``1e-5``)`` ``rho`` ``+`` ``2`` ``*`` `[`sqrt`](https://rdrr.io/r/base/MathFun.html)`(``rho`` ``*`` `[`log`](https://rdrr.io/r/base/Log.html)`(``1`` ``/`` ``delta``)``)`\
\
`budget_rows`` ``<-`` ``function``(``optimizer``, ``tab``)`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`    optimizer ``=`` ``optimizer``,`\
`    sigma     ``=`` ``tab``$``sigma``[``1``:``3``]``,`\
`    n_queries ``=`` ``tab``$``n_evals``[``1``:``3``]``)`\
`budget`` ``<-`` `[`rbind`](https://rdrr.io/r/base/cbind.html)`(``budget_rows``(``"BFGS"``, ``bfgs_table``)``,`\
`                ``budget_rows``(``"Nelder–Mead"``, ``nm_table``)``)`\
`budget``$``rho_per_query``              ``<-`` ``(``1`` ``/`` ``budget``$``sigma``)``^``2`` ``/`` ``2`\
`budget``$``rho_total``                  ``<-`` ``budget``$``n_queries`` ``*`` ``budget``$``rho_per_query`\
`budget``$``epsilon_at_delta_1e_minus_5`` ``<-`` ``zcdp_to_eps``(``budget``$``rho_total``)`\
`show_budget``(``budget``)`

| Optimizer | \\\sigma\\ | Queries \\k\\ | \\\rho\\ per query | \\\rho\_{\text{total}} = k\rho\\ | \\\varepsilon\\ at \\\delta = 10^{-5}\\ |
|:---|---:|---:|---:|---:|---:|
| BFGS | \\10^{-5}\\ | 45 | \\5 \times 10^{9}\\ | \\2.25 \times 10^{11}\\ | \\2.25 \times 10^{11}\\ |
| BFGS | \\10^{-4}\\ | 31 | \\5 \times 10^{7}\\ | \\1.55 \times 10^{9}\\ | \\1.55 \times 10^{9}\\ |
| BFGS | \\10^{-3}\\ | 112 | \\5 \times 10^{5}\\ | \\5.6 \times 10^{7}\\ | \\5.605 \times 10^{7}\\ |
| Nelder–Mead | \\10^{-5}\\ | 204 | \\5 \times 10^{9}\\ | \\1.02 \times 10^{12}\\ | \\1.02 \times 10^{12}\\ |
| Nelder–Mead | \\10^{-4}\\ | 503 | \\5 \times 10^{7}\\ | \\2.515 \times 10^{10}\\ | \\2.515 \times 10^{10}\\ |
| Nelder–Mead | \\10^{-3}\\ | 503 | \\5 \times 10^{5}\\ | \\2.515 \times 10^{8}\\ | \\2.516 \times 10^{8}\\ |

zCDP composition; sensitivity \\\Delta = 1\\, target \\\delta =
10^{-5}\\ {.table .table .table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

The \\\varepsilon\\ column shows that at noise levels where the fits are
still close, \\\varepsilon\\ is large. Whether that is acceptable
depends on the application.

One could also explore tighter sensitivity bound \\\Delta\\ or allow far
fewer queries, etc. We don’t do that here.

## References

Bun, Mark, and Thomas Steinke. 2016. “Concentrated Differential Privacy:
Simplifications, Extensions, and Lower Bounds.” *Theory of Cryptography
Conference (TCC)*, 635–58.
<https://doi.org/10.1007/978-3-662-53641-4_24>.

Dwork, Cynthia, and Aaron Roth. 2014. *The Algorithmic Foundations of
Differential Privacy*. Foundations and Trends in Theoretical Computer
Science 9(3–4). Now Publishers. <https://doi.org/10.1561/0400000042>.
