homomorpheR
===========

Install this package the usual way in R or via:

```
library(devtools)
install_github("bnaras/homomorpheR")
```

Several vignettes are provided and can be listed as below.

```
> vignette(package = "homomorpheR")
Vignettes in package ‘homomorpheR’:

DHCox                   Distributed Stratified Cox Regression using
                        Homomorphic Computation (source, html)
DHCoxNCP                Distributed Stratified Cox Regression using
                        Non-Cooperating Parties (source, html)
introduction            Introduction to Homomorphic Computation
                        (source, html)
homomorphing            MLE using Homomorphic Computation (source,
                        html)
```

Then view, for example, as follows:

```
vignette("introduction", package="homomorpheR")
```

Four examples are provided:

- A quick and easy introduction to homomorphic computations
- An example of Homomorphic Maximum Likelihood Estimation for a Poisson
  parameter.
- A larger example of Homomorphic Computations for fitting a
  Stratified Cox Regression model where the data is distributed across
  sites.
- A more advanced example of Homomorphic Stratified Cox Regression
  using Non-cooperating parties

A related project is [distcomp](https://cran.r-project.org/package=distcomp).

