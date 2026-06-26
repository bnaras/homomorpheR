# Threshold Cox + Output Differential Privacy (Demonstration)

## Introduction

This vignette is a **demonstration**, not a recommendation.
[`vignette("cox-threshold")`](https://bnaras.github.io/homomorpheR/articles/cox-threshold.md)
shows the lossless threshold-FHE protocol: bit-identical to
[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html), no single
decrypter, and the aggregator sees only $`\ell(\beta)`$ at each
optimizer query. That trajectory of joint log-likelihood values is the
residual exposure the lossless protocol accepts as the cost of
preserving accuracy.

A natural question is whether one can compose the threshold-FHE channel
with **output differential privacy** to bound that residual exposure
formally. The answer in this simulation is “yes, the cryptographic
protocol composes cleanly and the implementation is one extra
[`rnorm()`](https://rdrr.io/r/stats/Normal.html) call per site per
query, but the accuracy/privacy trade-off does not produce a usable
point at small $`\varepsilon`$ given the optimizers we use here.” This
vignette runs that trade-off end-to-end and reports the numbers.

`feedback_he_not_dp` in the package’s notes records the framing
explicitly: lossless threshold-FHE is the package’s primary path; DP
demonstrations show *what happens if* a user composes output DP on top,
not the recommended deployment for precision-critical workloads.

## A brief output-DP primer

Given a query $`f: \mathcal{D} \to \mathbb{R}`$ on a dataset $`D`$, the
**Gaussian mechanism** releases
$`\tilde f(D) = f(D) + \mathcal{N}(0, \sigma^2)`$. If $`f`$’s
*sensitivity* (the largest change $`f`$ can undergo when one record is
added or removed) is $`\Delta`$, then for any $`\varepsilon \in (0, 1)`$
and $`\delta > 0`$, choosing
$`\sigma \geq \Delta \sqrt{2\ln(1.25/\delta)}/\varepsilon`$ makes a
single release of $`\tilde f`$ satisfy $`(\varepsilon, \delta)`$
differential privacy.

The optimizer issues many queries, so the per-query budget composes. We
use **zCDP composition** \[Bun & Steinke, 2016\]: the Gaussian mechanism
is $`\rho`$-zCDP with $`\rho = (\Delta/\sigma)^2/2`$; $`T`$ queries
compose linearly to $`T \cdot \rho`$ in zCDP units; and that converts to
$`(\varepsilon, \delta)`$ via
$`\varepsilon = \rho + 2\sqrt{\rho \log(1/\delta)}`$.

## The protocol

The setup is identical to
[`vignette("cox-threshold")`](https://bnaras.github.io/homomorpheR/articles/cox-threshold.md),
except each site adds an independent Gaussian noise term to its local
contribution before encryption. Since variances of independent normals
add, the decrypted total is $`\ell(\beta) + \mathcal{N}(0, \sigma^2)`$
when each site adds $`\mathcal{N}(0, \sigma^2/N)`$.

## The Cox setup (same DLBCL data as the other Cox vignettes)

``` r

suppressPackageStartupMessages(library(survival))
library(homomorpheR)
library(stats4)
data(DLBCL)

cox_data <- split(
  DLBCL[, c("time", "status", "GCB_sig", "LN_sig",
            "Prolif_sig", "BMP6", "MHC2_sig", "Subgroup")],
  DLBCL$Subgroup)

agg_model <- coxph(Surv(time, status) ~ GCB_sig + LN_sig +
                       Prolif_sig + BMP6 + MHC2_sig +
                       strata(Subgroup),
                   data = DLBCL)
agg_coef <- coef(agg_model)

cph_control <- replace(coxph.control(), "iter.max", 0)

local_cox_nll <- function(data, beta) {
    fit <- tryCatch(
        coxph(Surv(time, status) ~ GCB_sig + LN_sig + Prolif_sig +
                  BMP6 + MHC2_sig,
              data    = data,
              init    = beta,
              control = cph_control),
        error = function(e) NULL)
    if (is.null(fit)) NA_real_ else -fit$loglik[1]
}
```

## Threshold setup and DP-noised workers

The threshold infrastructure is the same as `cox-threshold`. The DP
twist lives entirely inside the per-worker `local_fn`: each worker adds
an independent $`\mathcal{N}(0, \sigma^2/N)`$ draw to its local nLL
before returning. The master/worker runner sees only the noisy local
value and encrypts it as usual.

``` r

cc <- openfhe.R::fhe_context("CKKS",
                           multiplicative_depth = 1L,
                           scaling_mod_size     = 59L,
                           first_mod_size       = 60L,
                           batch_size           = 8L,
                           features             = c(openfhe.R::Feature$MULTIPARTY))

n_sites <- length(cox_data)

build_dp_workers <- function(sigma) {
    lapply(names(cox_data), function(nm) {
        make_worker(
            nm,
            data     = cox_data[[nm]],
            local_fn = function(data, beta) {
                nll <- local_cox_nll(data, beta)
                if (is.na(nll)) return(NA_real_)
                nll + rnorm(1L, mean = 0, sd = sigma / sqrt(n_sites))
            })
    })
}

fit_at_sigma <- function(sigma, method = "BFGS", seed = 1L) {
    set.seed(seed)   # stabilize the DP-noise draws across runs
    master <- make_threshold_master("Aggregator",
                                    crypto_context = cc,
                                    n_sites        = n_sites)
    set_workers(master, build_dp_workers(sigma))
    dp_nLL <- function(GCB_sig, LN_sig, Prolif_sig, BMP6, MHC2_sig)
        master_aggregate(master, c(GCB_sig, LN_sig, Prolif_sig, BMP6, MHC2_sig))
    stats4::mle(dp_nLL,
                start   = list(GCB_sig = 0, LN_sig = 0, Prolif_sig = 0,
                               BMP6    = 0, MHC2_sig = 0),
                method  = method,
                control = list(reltol = 1e-7))
}
```

## Mechanical correctness: $`\sigma = 0`$ reproduces `cox-threshold`

When the noise is zero the protocol reduces to the lossless threshold
protocol. The fitted coefficients match
[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) to
threshold-CKKS precision.

``` r

fit_clean <- fit_at_sigma(0)
clean_check <- data.frame(
    coefficient = names(agg_coef),
    cleartext   = unname(agg_coef),
    protocol    = unname(coef(fit_clean)[names(agg_coef)]),
    abs_diff    = abs(unname(coef(fit_clean)[names(agg_coef)] - agg_coef))
)
knitr::kable(clean_check, digits = 9,
             caption = "Threshold-DP protocol at sigma = 0 vs cleartext coxph()")
```

| coefficient |  cleartext |   protocol |  abs_diff |
|:------------|-----------:|-----------:|----------:|
| GCB_sig     | -0.2638716 | -0.2638698 | 1.822e-06 |
| LN_sig      | -0.2543592 | -0.2543587 | 5.340e-07 |
| Prolif_sig  |  0.3031258 |  0.3031250 | 7.480e-07 |
| BMP6        |  0.3036375 |  0.3036367 | 7.940e-07 |
| MHC2_sig    | -0.3191467 | -0.3191459 | 8.420e-07 |

Threshold-DP protocol at sigma = 0 vs cleartext coxph() {.table}

The maximum absolute coefficient difference at $`\sigma = 0`$ is
1.82^{-6}. The DP layer adds no measurable error when the noise is zero,
so the cryptographic channel is doing exactly what it does in the
lossless vignette. Every deviation from
[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) below is
attributable to the noise.

## BFGS at increasing noise: where does it break?

We sweep $`\sigma`$ across four orders of magnitude. Each fit is one
full BFGS run over the threshold-DP encrypted nLL.

``` r

sigma_grid_bfgs <- c(1e-4, 1e-3, 1e-2, 1e-1, 1)
fits_bfgs <- lapply(sigma_grid_bfgs, fit_at_sigma, method = "BFGS")
names(fits_bfgs) <- sprintf("sigma_%g", sigma_grid_bfgs)

bfgs_table <- data.frame(
    sigma        = sigma_grid_bfgs,
    n_evals      = sapply(fits_bfgs, function(f) f@details$counts[["function"]]),
    GCB_sig      = sapply(fits_bfgs, function(f) coef(f)[["GCB_sig"]]),
    LN_sig       = sapply(fits_bfgs, function(f) coef(f)[["LN_sig"]]),
    Prolif_sig   = sapply(fits_bfgs, function(f) coef(f)[["Prolif_sig"]]),
    BMP6         = sapply(fits_bfgs, function(f) coef(f)[["BMP6"]]),
    MHC2_sig     = sapply(fits_bfgs, function(f) coef(f)[["MHC2_sig"]]),
    max_abs_diff = sapply(fits_bfgs, function(f)
        max(abs(coef(f) - agg_coef[names(coef(f))]))))
knitr::kable(bfgs_table, digits = 6,
             caption = "BFGS over the threshold-DP nLL at five noise scales")
```

|  | sigma | n_evals | GCB_sig | LN_sig | Prolif_sig | BMP6 | MHC2_sig | max_abs_diff |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| sigma_0.0001 | 1e-04 | 31 | -0.263711 | -0.254310 | 0.301416 | 0.304232 | -0.320419 | 0.001710 |
| sigma_0.001 | 1e-03 | 95 | -0.255596 | -0.257797 | 0.284336 | 0.305723 | -0.320768 | 0.018790 |
| sigma_0.01 | 1e-02 | 297 | -0.240806 | -0.266638 | 0.296517 | 0.312648 | -0.340970 | 0.023065 |
| sigma_0.1 | 1e-01 | 146 | -0.475716 | -0.276219 | 0.506100 | 0.381061 | -0.188904 | 0.211845 |
| sigma_1 | 1e+00 | 91 | -0.133843 | -0.389685 | 0.034282 | -0.045055 | -0.336133 | 0.348692 |

BFGS over the threshold-DP nLL at five noise scales {.table}

Read `max_abs_diff`: it grows roughly linearly with $`\sigma`$ in the
small-$`\sigma`$ regime (1e-4 → 1e-2) and the qualitative answer
collapses between $`\sigma = 0.1`$ and $`\sigma = 1`$. At $`\sigma = 1`$
BFGS exits cleanly but the fit is unusable.

### Why BFGS breaks

BFGS estimates gradients by finite differences. With default
`ndeps = 1e-3` the gradient noise per coordinate inflates from the
function-value noise of $`\sigma`$ to roughly $`\sigma\sqrt{2}/(2h)
\approx 707\,\sigma`$. Near the optimum the true gradient is small and
the noise dominates. Any optimizer that takes derivatives by
differencing nearby function values will have this exact failure mode
against any DP mechanism that adds enough noise to be meaningful.

This is not a defect of the cryptographic protocol. The protocol is
doing exactly what it claims: releasing $`\ell(\beta) +
\mathcal{N}(0, \sigma^2)`$ at every query.

## Nelder–Mead at the same noise scale

Nelder–Mead is gradient-free; it never amplifies noise through a $`1/h`$
divisor. Noise still hurts it, but more gently than BFGS.

``` r

fit_nm_1 <- fit_at_sigma(1, method = "Nelder-Mead")

bfgs1 <- coef(fits_bfgs[["sigma_1"]])
nm1   <- coef(fit_nm_1)
coefs <- names(agg_coef)

nm_compare <- data.frame(
    coefficient  = coefs,
    coxph_agg = unname(agg_coef[coefs]),
    BFGS_sigma1  = unname(bfgs1[coefs]),
    NM_sigma1    = unname(nm1[coefs])
)
knitr::kable(nm_compare, digits = 6,
             caption = sprintf(
                 "BFGS vs Nelder-Mead at sigma = 1 (BFGS %d evals, NM %d evals)",
                 fits_bfgs[["sigma_1"]]@details$counts[["function"]],
                 fit_nm_1@details$counts[["function"]]))
```

| coefficient | coxph_agg | BFGS_sigma1 | NM_sigma1 |
|:------------|----------:|------------:|----------:|
| GCB_sig     | -0.263872 |   -0.133843 | -0.046219 |
| LN_sig      | -0.254359 |   -0.389685 | -0.169005 |
| Prolif_sig  |  0.303126 |    0.034282 |  0.048707 |
| BMP6        |  0.303638 |   -0.045055 |  0.472601 |
| MHC2_sig    | -0.319147 |   -0.336133 | -0.343019 |

BFGS vs Nelder-Mead at sigma = 1 (BFGS 91 evals, NM 503 evals) {.table}

At $`\sigma = 1`$ BFGS is unusable: at least one signature lands at the
wrong sign relative to the aggregated cleartext fit. Nelder–Mead at the
same $`\sigma`$ keeps every coefficient on the side of zero that the
centralized fit puts it on. The price is more function evaluations,
which feeds back into the privacy budget below.

## Privacy budget

For each fit, with sensitivity $`\Delta = 1`$ (placeholder) and target
$`\delta = 10^{-5}`$, zCDP composition gives:

``` r

zcdp_to_eps <- function(rho, delta = 1e-5) rho + 2 * sqrt(rho * log(1 / delta))

bfgs_budget <- data.frame(
    optimizer = "BFGS",
    sigma     = sigma_grid_bfgs,
    n_queries = sapply(fits_bfgs, function(f) f@details$counts[["function"]]),
    stringsAsFactors = FALSE)
nm_budget <- data.frame(
    optimizer = "Nelder-Mead",
    sigma     = 1,
    n_queries = fit_nm_1@details$counts[["function"]])
budget <- rbind(bfgs_budget, nm_budget)
budget$rho_per_query              <- (1 / budget$sigma)^2 / 2
budget$rho_total                  <- budget$n_queries * budget$rho_per_query
budget$epsilon_at_delta_1e_minus_5 <- zcdp_to_eps(budget$rho_total)
knitr::kable(budget, digits = 4,
             caption = "zCDP composition; sensitivity Delta = 1, target delta = 1e-5")
```

|  | optimizer | sigma | n_queries | rho_per_query | rho_total | epsilon_at_delta_1e_minus_5 |
|:---|:---|---:|---:|---:|---:|---:|
| sigma_0.0001 | BFGS | 1e-04 | 31 | 5e+07 | 1.550e+09 | 1.550267e+09 |
| sigma_0.001 | BFGS | 1e-03 | 95 | 5e+05 | 4.750e+07 | 4.754677e+07 |
| sigma_0.01 | BFGS | 1e-02 | 297 | 5e+03 | 1.485e+06 | 1.493270e+06 |
| sigma_0.1 | BFGS | 1e-01 | 146 | 5e+01 | 7.300e+03 | 7.879808e+03 |
| sigma_1 | BFGS | 1e+00 | 91 | 5e-01 | 4.550e+01 | 9.127500e+01 |
| 1 | Nelder-Mead | 1e+00 | 503 | 5e-01 | 2.515e+02 | 3.591197e+02 |

zCDP composition; sensitivity Delta = 1, target delta = 1e-5 {.table}

Read the `epsilon_at_delta_1e_minus_5` column. At noise scales the
optimizers can tolerate without producing a broken fit, the resulting
$`\varepsilon`$ is large. Whether large $`\varepsilon`$ is acceptable is
a use-case decision; we report the numbers and stop.

### What it would take to push $`\varepsilon`$ smaller

Three directions, none pursued here:

1.  **Tighter sensitivity bound.** $`\Delta = 1`$ is conservative;
    careful Cox-specific analysis could reduce it.
2.  **Far fewer queries.** An optimizer that found the MLE in $`\le 10`$
    queries would improve the budget by $`\sim 10\times`$; DP-SGD-style
    methods achieve this via subsampling and momentum.
3.  **Subsampling amplification.** Per-record subsampling with the
    appropriate amplification accountant — incompatible with the current
    “compute the whole nLL at every site at every query” shape, but
    adaptable.

These are research questions, not package-level concerns. The package’s
main story is the lossless cox progression.

## What this demonstrates

1.  **The cryptographic protocol is correct under the noise.** At
    $`\sigma = 0`$ it reproduces `cox-threshold` exactly.
2.  **The two layers compose without interaction.** Threshold FHE
    handles “who holds the key”; output DP handles “what the released
    values reveal.” Adding the DP layer is one
    [`rnorm()`](https://rdrr.io/r/stats/Normal.html) call per site per
    query and zero new homomorpheR machinery.
3.  **The accuracy/noise trade-off is characterized end-to-end.** The
    vignette runs five $`\sigma`$ values with BFGS and one with
    Nelder–Mead, and reports the recovered fits and the privacy budgets
    in tables.
4.  **The optimizer-vs-noise interaction shapes the trade-off.** BFGS
    amplifies function-value noise through finite-difference gradients
    and breaks at moderate $`\sigma`$; Nelder–Mead at $`\sigma = 1`$
    recovers a usable estimate at the cost of more queries. The
    cryptographic layer is not the limit — the optimizer is.

## Limitations

- **Tight Cox sensitivity.** $`\Delta = 1`$ is a placeholder.
- **DP-SGD or stochastic gradient methods.** Standard alternative for
  tight DP budgets; out of scope here.
- **Adaptive noise schedules.** All queries here use the same
  $`\sigma`$; adaptive schedules can be tighter.
- **Malicious-site protection.** A hostile site can corrupt either its
  $`\text{nLL}_i`$ or its $`z_i`$. Out of scope.

Readers who need lossless fits should use
[`vignette("cox-threshold")`](https://bnaras.github.io/homomorpheR/articles/cox-threshold.md)
and accept the residual released-function leakage that comes with it.

## References

- Dwork & Roth (2014). *The Algorithmic Foundations of Differential
  Privacy.* §3.5.3 (Gaussian mechanism).
- Bun & Steinke (2016). *Concentrated Differential Privacy.* TCC. Source
  for the zCDP composition bound used above.
- Abadi et al. (2016). *Deep Learning with Differential Privacy.* CCS.
  The DP-SGD paper.
