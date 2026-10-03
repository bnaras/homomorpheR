# Threshold Cox with Gaussian Noise (Demonstration)

## Introduction

This vignette is a demonstration. The previous
[`vignette("cox-threshold")`](https://bnaras.github.io/homomorpheR/articles/cox-threshold.md)
fits the Cox model under threshold FHE: the fit matches the cleartext
fit, no single party can decrypt, and the aggregator sees only the joint
log-likelihood \\\ell(\beta)\\ at each optimizer query.

Here we ask what happens if each site also adds Gaussian noise of the
kind an *output differential privacy* mechanism uses. The sensitivity
\\\Delta = 1\\ below is a placeholder, so the \\\varepsilon\\ values are
not a privacy guarantee. Adding the noise takes one extra
[`rnorm()`](https://rdrr.io/r/stats/Normal.html) call per site per
query. With the optimizers used here, the fits are poor at any noise
level that gives a small \\\varepsilon\\. The vignette runs the fits and
reports the numbers.

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

The threshold setup is also the same as in `cox-threshold`. The only
change is in each worker’s `contribution_fn`: it adds an independent
\\\mathcal{N}(0, \sigma^2/N)\\ draw to its local nLL before returning
it. The noisy value is then encrypted as usual.

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
            contribution_fn = function(data, beta) {
                nll <- local_cox_nll(data, beta)
                if (is.na(nll)) return(NA_real_)
                nll + rnorm(1L, mean = 0, sd = sigma / sqrt(n_sites))
            })
    })
}

fit_at_sigma <- function(sigma, method = "BFGS", seed = 1L) {
    set.seed(seed)   # stabilize the DP-noise draws across runs
    workers <- build_dp_workers(sigma)
    master  <- make_threshold_master("Aggregator",
                                     crypto_context = cc,
                                     sites          = workers)
    ## Every call is one decrypted release, including the calls optim()
    ## makes for finite-difference gradients and mle() for the Hessian.
    n_queries <- 0L
    dp_nLL <- function(GCB_sig, LN_sig, Prolif_sig, BMP6, MHC2_sig) {
        n_queries <<- n_queries + 1L
        master_aggregate(master, c(GCB_sig, LN_sig, Prolif_sig, BMP6, MHC2_sig))
    }
    fit <- stats4::mle(dp_nLL,
                       start   = list(GCB_sig = 0, LN_sig = 0, Prolif_sig = 0,
                                      BMP6    = 0, MHC2_sig = 0),
                       method  = method,
                       control = list(reltol = 1e-7))
    list(fit = fit, n_queries = n_queries)
}
```

## Mechanical correctness: \\\sigma = 0\\ reproduces `cox-threshold`

When the noise is zero the protocol reduces to the lossless threshold
protocol. The fitted coefficients match
[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) to
threshold-CKKS precision.

``` r

fit_clean <- fit_at_sigma(0)$fit
clean_check <- data.frame(
    coefficient = names(agg_coef),
    cleartext   = unname(agg_coef),
    protocol    = unname(coef(fit_clean)[names(agg_coef)]),
    abs_diff    = abs(unname(coef(fit_clean)[names(agg_coef)] - agg_coef))
)
show_clean(clean_check)
```

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

``` r

sigma_grid <- c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1)

sweep_table <- function(fits) data.frame(
    sigma        = sigma_grid,
    n_queries    = sapply(fits, `[[`, "n_queries"),
    GCB_sig      = sapply(fits, function(f) coef(f$fit)[["GCB_sig"]]),
    LN_sig       = sapply(fits, function(f) coef(f$fit)[["LN_sig"]]),
    Prolif_sig   = sapply(fits, function(f) coef(f$fit)[["Prolif_sig"]]),
    BMP6         = sapply(fits, function(f) coef(f$fit)[["BMP6"]]),
    MHC2_sig     = sapply(fits, function(f) coef(f$fit)[["MHC2_sig"]]),
    max_abs_diff = sapply(fits, function(f)
        max(abs(coef(f$fit) - agg_coef[names(coef(f$fit))]))))

fits_bfgs  <- lapply(sigma_grid, fit_at_sigma, method = "BFGS")
bfgs_table <- sweep_table(fits_bfgs)
show_sweep(bfgs_table, "BFGS")
```

| \\\sigma\\ | Queries | GCB_sig | LN_sig | Prolif_sig | BMP6 | MHC2_sig | \\\max_j \lvert \hat\beta_j - \hat\beta_j^{\text{coxph}} \rvert\\ |
|:---|---:|---:|---:|---:|---:|---:|---:|
| \\10^{-5}\\ | 225 | -0.263927 | -0.254319 | 0.303222 | 0.303581 | -0.319180 | 0.000096 |
| \\10^{-4}\\ | 221 | -0.263711 | -0.254310 | 0.301416 | 0.304232 | -0.320419 | 0.001710 |
| \\10^{-3}\\ | 385 | -0.255596 | -0.257797 | 0.284336 | 0.305723 | -0.320768 | 0.018790 |
| \\10^{-2}\\ | 640 | -0.261541 | -0.246078 | 0.296769 | 0.311191 | -0.310249 | 0.008897 |
| \\10^{-1}\\ | 397 | -0.475716 | -0.276219 | 0.506100 | 0.381061 | -0.188904 | 0.211845 |
| \\1\\ | 250 | -0.133843 | -0.389685 | 0.034282 | -0.045055 | -0.336133 | 0.348692 |

BFGS over the threshold-DP nLL at 6 values of \\\sigma\\ {.table .table
.table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

## Nelder–Mead at the same noise scales

The same sweep with Nelder–Mead.

``` r

fits_nm  <- lapply(sigma_grid, fit_at_sigma, method = "Nelder-Mead")
nm_table <- sweep_table(fits_nm)
show_sweep(nm_table, "Nelder–Mead")
```

| \\\sigma\\ | Queries | GCB_sig | LN_sig | Prolif_sig | BMP6 | MHC2_sig | \\\max_j \lvert \hat\beta_j - \hat\beta_j^{\text{coxph}} \rvert\\ |
|:---|---:|---:|---:|---:|---:|---:|---:|
| \\10^{-5}\\ | 304 | -0.263636 | -0.254196 | 0.302993 | 0.303441 | -0.319003 | 0.000235 |
| \\10^{-4}\\ | 603 | -0.260936 | -0.254388 | 0.300052 | 0.307205 | -0.321379 | 0.003568 |
| \\10^{-3}\\ | 603 | -0.248410 | -0.247910 | 0.338068 | 0.311995 | -0.313197 | 0.034942 |
| \\10^{-2}\\ | 603 | -0.251861 | -0.244768 | 0.344645 | 0.315905 | -0.313427 | 0.041519 |
| \\10^{-1}\\ | 603 | -0.248230 | -0.255815 | 0.352342 | 0.309011 | -0.330805 | 0.049217 |
| \\1\\ | 603 | -0.046219 | -0.169005 | 0.048707 | 0.472601 | -0.343019 | 0.254419 |

Nelder–Mead over the threshold-DP nLL at 6 values of \\\sigma\\ {.table
.table .table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

Fidelity decays as \\\sigma\\ grows.

## Privacy budget

For the fits at the first three values of \\\sigma\\, with sensitivity
\\\Delta = 1\\ (placeholder) and target \\\delta = 10^{-5}\\, zCDP
composition gives:

``` r

zcdp_to_eps <- function(rho, delta = 1e-5) rho + 2 * sqrt(rho * log(1 / delta))

budget_rows <- function(optimizer, tab) data.frame(
    optimizer = optimizer,
    sigma     = tab$sigma[1:3],
    n_queries = tab$n_queries[1:3])
budget <- rbind(budget_rows("BFGS", bfgs_table),
                budget_rows("Nelder–Mead", nm_table))
budget$rho_per_query              <- (1 / budget$sigma)^2 / 2
budget$rho_total                  <- budget$n_queries * budget$rho_per_query
budget$epsilon_at_delta_1e_minus_5 <- zcdp_to_eps(budget$rho_total)
show_budget(budget)
```

| Optimizer | \\\sigma\\ | Queries \\k\\ | \\\rho\\ per query | \\\rho\_{\text{total}} = k\rho\\ | \\\varepsilon\\ at \\\delta = 10^{-5}\\ |
|:---|---:|---:|---:|---:|---:|
| BFGS | \\10^{-5}\\ | 225 | \\5 \times 10^{9}\\ | \\1.125 \times 10^{12}\\ | \\1.125 \times 10^{12}\\ |
| BFGS | \\10^{-4}\\ | 221 | \\5 \times 10^{7}\\ | \\1.105 \times 10^{10}\\ | \\1.105 \times 10^{10}\\ |
| BFGS | \\10^{-3}\\ | 385 | \\5 \times 10^{5}\\ | \\1.925 \times 10^{8}\\ | \\1.926 \times 10^{8}\\ |
| Nelder–Mead | \\10^{-5}\\ | 304 | \\5 \times 10^{9}\\ | \\1.52 \times 10^{12}\\ | \\1.52 \times 10^{12}\\ |
| Nelder–Mead | \\10^{-4}\\ | 603 | \\5 \times 10^{7}\\ | \\3.015 \times 10^{10}\\ | \\3.015 \times 10^{10}\\ |
| Nelder–Mead | \\10^{-3}\\ | 603 | \\5 \times 10^{5}\\ | \\3.015 \times 10^{8}\\ | \\3.016 \times 10^{8}\\ |

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
