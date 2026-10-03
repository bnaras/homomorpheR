# Consensus ADMM with Gaussian Noise (Demonstration)

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

Here we ask what happens if each site also adds Gaussian noise of the
kind an *output differential privacy* mechanism uses. The sensitivity
\\\Delta = 1\\ below is a placeholder, so the \\\varepsilon\\ values are
not a privacy guarantee. Adding the noise takes one extra
[`rnorm()`](https://rdrr.io/r/stats/Normal.html) call per site per
iteration. With this optimizer, the fits are poor at any noise level
that gives a small \\\varepsilon\\. The vignette runs the fits and
reports the numbers.

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

``` r

suppressPackageStartupMessages({
    library(homomorpheR)
    library(CVXR)
    library(S7)
})

N   <- 3L
p   <- 4L
lam <- 1
```

``` r

build_local_problem <- function(X_i, y_i, rho_val) {
    x  <- Variable(p)
    zp <- Parameter(p)
    up <- Parameter(p)
    y_signs <- 2 * y_i - 1
    margins <- -y_signs * (X_i %*% x)
    local_loss <- sum(logistic(margins)) +
                  (lam / (2 * N)) * sum_squares(x)
    augmented  <- (rho_val / 2) * sum_squares(x - zp + up)
    prob <- Problem(Minimize(local_loss + augmented))
    value(zp) <- rep(0, p); value(up) <- rep(0, p)
    list(prob = prob, x = x, zp = zp, up = up)
}

## Inherits homomorpheR's abstract `Site` (which supplies `name` and the
## `state` environment), so it can take part in threshold key generation
## and keep its own share.
ConsensusSite <- new_class("ConsensusSite",
    parent     = homomorpheR::Site,
    properties = list(n = class_integer))

make_consensus_site <- function(name, X_i, y_i, rho_val) {
    st        <- new.env(parent = emptyenv())
    st$X      <- X_i
    st$y      <- y_i
    built     <- build_local_problem(X_i, y_i, rho_val)
    st$prob   <- built$prob
    st$x_var  <- built$x
    st$zp     <- built$zp
    st$up     <- built$up
    st$x_curr <- rep(0, ncol(X_i))
    st$u_curr <- rep(0, ncol(X_i))
    ConsensusSite(name = name, n = nrow(X_i), state = st)
}

local_update <- function(site, z_curr) {
    st <- site@state
    value(st$zp) <- z_curr
    value(st$up) <- st$u_curr
    suppressMessages(suppressWarnings(psolve(st$prob, solver = "CLARABEL")))
    if (!status(st$prob) %in% c("optimal", "optimal_inaccurate"))
        stop("Local CVXR solve at ", site@name, " did not reach optimal status.")
    st$x_curr <- as.numeric(value(st$x_var))
    invisible(st$x_curr)
}
```

## Simulated cohort

``` r

set.seed(20260412)
n_per_site <- c(500L, 1000L, 1500L)
beta_true  <- c(intercept = -0.5, age = 0.4, bmi = -0.3, sex = 0.6)

make_site_data <- function(n) {
    X  <- cbind(1, rnorm(n), rnorm(n), rbinom(n, 1, 0.5))
    pr <- plogis(as.numeric(X %*% beta_true))
    y  <- as.integer(runif(n) < pr)
    list(X = X, y = y)
}
site_data <- lapply(n_per_site, make_site_data)
```

## Choosing \\\rho\\ and \\T\\ without touching the cohort

Tuning \\\rho\\ on the real data would itself be a release: the chosen
\\\rho\\ and \\T\\ depend on the records, and the budget below counts
only the \\T\\ Gaussian releases. So the sweep runs on a *surrogate
cohort* built only from facts the protocol already treats as public: the
number of sites, their sizes, and the covariate schema. The effect sizes
are nominal values fixed in the analysis plan. No record from any site
enters it, so the sweep needs no encryption.

``` r

tol      <- 1e-3
max_iter <- 60L

## Public design facts: three sites of these sizes, four
## covariates of these types. Nominal effect sizes, not the cohort's.
beta_nominal   <- c(0, 0.5, 0.5, 0.5)
surrogate_seed <- 20260413L

set.seed(surrogate_seed)
surrogate_data <- lapply(n_per_site, function(n) {
    X  <- cbind(1, rnorm(n), rnorm(n), rbinom(n, 1, 0.5))
    pr <- plogis(as.numeric(X %*% beta_nominal))
    list(X = X, y = as.integer(runif(n) < pr))
})

sweep_one_rho <- function(cohort, rho_val) {
    built <- lapply(cohort,
                    function(s) build_local_problem(s$X, s$y, rho_val))
    x_curr <- u_curr <- replicate(N, rep(0, p), simplify = FALSE)
    z      <- rep(0, p)
    k_conv <- NA_integer_
    for (k in seq_len(max_iter)) {
        for (i in seq_len(N)) {
            value(built[[i]]$zp) <- z
            value(built[[i]]$up) <- u_curr[[i]]
            suppressMessages(suppressWarnings(
                psolve(built[[i]]$prob, solver = "CLARABEL")))
            x_curr[[i]] <- as.numeric(value(built[[i]]$x))
        }
        z_prev <- z
        z <- Reduce(`+`, Map(`+`, x_curr, u_curr)) / N
        for (i in seq_len(N)) u_curr[[i]] <- u_curr[[i]] + x_curr[[i]] - z
        pri <- sqrt(sum(vapply(seq_len(N),
            function(i) sum((x_curr[[i]] - z)^2), 0)) / N)
        dua <- rho_val * sqrt(sum((z - z_prev)^2))
        if (pri < tol && dua < tol) { k_conv <- k; break }
    }
    data.frame(rho = rho_val,
               iters = if (is.na(k_conv)) max_iter else k_conv,
               converged = !is.na(k_conv))
}

rho_grid  <- c(10, 20, 50, 100, 500)
rho_sweep <- do.call(rbind,
                     lapply(rho_grid,
                            function(r) sweep_one_rho(surrogate_data, r)))
show_rho_sweep(rho_sweep)

converged_rows <- rho_sweep[rho_sweep$converged, ]
if (nrow(converged_rows) == 0L)
    stop("No rho in the grid converged within max_iter on the surrogate.")

rho_chosen <- converged_rows$rho[which.min(converged_rows$iters)]
T_fixed    <- converged_rows$iters[converged_rows$rho == rho_chosen]
```

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

``` r

cc <- openfhe.R::fhe_context("CKKS",
                           multiplicative_depth = 1L,
                           scaling_mod_size     = 59L,
                           first_mod_size       = 60L,
                           batch_size           = 8L,
                           features             = c(openfhe.R::Feature$MULTIPARTY))
```

The DP version of the consensus step. The only change from the lossless
ADMM vignette’s `encrypted_consensus()` is the
`rnorm(p, ..., sd = sigma * sqrt(Nv))` term inside the per-site loop:

``` r

## Site-side: the site draws its own noise, adds it, and encrypts with
## the public parameters it received at setup, all before anything
## leaves the site. The noiseless x_i + u_i never leaves.
site_contribution_dp <- function(site, sigma, Nv) {
    st <- site@state
    noised <- st$x_curr + st$u_curr + rnorm(p, mean = 0, sd = sigma * sqrt(Nv))
    encrypt(site, noised)
}

## Aggregator-side: sum the encrypted values, scale, threshold-decrypt. The
## 1/N scaling contracts the summed noise variance back to sigma^2.
encrypted_consensus_dp <- function(threshold_master, sites, sigma) {
    Nv  <- length(sites)
    cts <- lapply(sites, site_contribution_dp, sigma = sigma, Nv = Nv)
    ct_avg <- Reduce(`+`, cts) * (1 / Nv)
    decrypt(threshold_master, ct_avg, len = p)
}
```

## The DP-ADMM loop

``` r

run_dp_admm <- function(sigma, T_iter = T_fixed, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    ## The sites exist first: the joint public key is built from them,
    ## each keeping the share it generates.
    sites <- list(
        make_consensus_site("Site 1", site_data[[1]]$X, site_data[[1]]$y, rho_chosen),
        make_consensus_site("Site 2", site_data[[2]]$X, site_data[[2]]$y, rho_chosen),
        make_consensus_site("Site 3", site_data[[3]]$X, site_data[[3]]$y, rho_chosen))
    master <- make_threshold_master("Aggregator",
                                    crypto_context = cc,
                                    sites          = sites)

    z_curr <- rep(0, p)
    z_hist <- matrix(NA_real_, nrow = T_iter, ncol = p,
                     dimnames = list(NULL, names(beta_true)))
    for (k in seq_len(T_iter)) {
        for (s in sites) local_update(s, z_curr)
        z_curr <- encrypted_consensus_dp(master, sites, sigma)
        for (s in sites) {
            s@state$u_curr <- s@state$u_curr + (s@state$x_curr - z_curr)
        }
        z_hist[k, ] <- z_curr
    }
    list(z = z_curr, z_hist = z_hist)
}
```

## Centralized CVXR fit

This fit pools the raw data, so it is not part of the protocol. We use
it only to measure how far the noise moves the answer, in the last
column of the table below. It is not released, so it is not charged to
the budget.

``` r

X_pooled  <- do.call(rbind, lapply(site_data, `[[`, "X"))
y_pooled  <- unlist(lapply(site_data, `[[`, "y"))
beta_var  <- Variable(p)
y_signs_p <- 2 * y_pooled - 1
margins_p <- -y_signs_p * (X_pooled %*% beta_var)
suppressMessages(suppressWarnings(
    psolve(Problem(Minimize(sum(logistic(margins_p)) +
                            (lam / 2) * sum_squares(beta_var))),
           solver = "CLARABEL")))
```

    ## [1] 1932.792

``` r

beta_central <- as.numeric(value(beta_var))
names(beta_central) <- names(beta_true)
```

## The \\\sigma\\ sweep

Six \\\sigma\\ values from zero to one. The \\\sigma = 0\\ row checks
that the protocol without noise matches the centralized fit.

``` r

sigma_grid    <- c(0, 1e-4, 1e-3, 1e-2, 1e-1, 1)
sweep_results <- vector("list", length(sigma_grid))
for (j in seq_along(sigma_grid)) {
    sweep_results[[j]] <- run_dp_admm(sigma = sigma_grid[j], seed = 100L + j)
}
names(sweep_results) <- sprintf("sigma=%.0e", sigma_grid)
```

``` r

clean_dev <- max(abs(sweep_results[[1]]$z - beta_central))
agree_tol <- 10 * tol
if (clean_dev > agree_tol)
    stop("DP-ADMM at sigma = 0 disagrees with the centralized fit.")
```

At \\\sigma = 0\\ the largest coefficient deviation from the centralized
fit is 2.91 × 10⁻⁵, within \\10 \times\\ the ADMM tolerance of 0.001.

``` r

summary_df <- do.call(rbind, lapply(seq_along(sigma_grid), function(j) {
    z <- sweep_results[[j]]$z
    data.frame(sigma     = sigma_grid[j],
               intercept = z[1],
               age       = z[2],
               bmi       = z[3],
               sex       = z[4],
               max_dev   = max(abs(z - beta_central)))
}))
central_row <- data.frame(sigma = NA, intercept = beta_central[1],
                         age = beta_central[2], bmi = beta_central[3],
                         sex = beta_central[4], max_dev = 0)
summary_table <- rbind(summary_df, central_row)
rownames(summary_table) <- c(sprintf("sigma=%g", sigma_grid), "centralized")
show_summary(summary_table)
```

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

``` r

zcdp_to_eps <- function(rho, delta = 1e-5) rho + 2 * sqrt(rho * log(1 / delta))

budget <- data.frame(sigma = sigma_grid[sigma_grid > 0])
budget$rho_total                  <- T_fixed * (1 / budget$sigma)^2 / 2
budget$epsilon_at_delta_1e_minus_5 <- zcdp_to_eps(budget$rho_total)
show_budget(budget, T_fixed)
```

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
