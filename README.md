homomorpheR
===========

The published version may be found on
[CRAN](https://cran.r-project.org/package=homomorpheR) and can be
installed as usual.

## Development version

Install this development version by cutting and pasting into your R
session, which will install all dependencies also.

```
## Install a package if not already installed
install_if_needed <- function(packages, ...) {
    toInstall <- setdiff(packages, installed.packages()[, 1])
    if (length(toInstall) > 0) install.packages(toInstall, ...)
}
install_if_needed(c("gmp", "sodium", "devtools"), repos = "https://cloud.r-project.org")
devtools::install_github("bnaras/homomorpheR")
```

Four vignettes are provided:

- A quick and easy introduction to homomorphic computations,
- An example of Homomorphic Maximum Likelihood Estimation for a Poisson
  parameter,
- Fitting a Stratified Cox Regression model where the data is
  distributed across sites,
- An example of Query Count using Noncooperating parties,
- Fitting a Stratified Cox Regression model where the data is
  distributed across sites using Non-cooperating parties.

A related project is [distcomp](https://cran.r-project.org/package=distcomp).

## Website

You can view everything, including documentation and vignettes on the
[homomorpheR website](https://bnaras.github.io/homomorpheR). 
