homomorpheR
===========

Install this package the usual way in R or via:

```{r}
library(devtools)
install_github("bnaras/homomorpheR")
```

Two vignettes are provided:

```{r}
vignette(package = "homomorpheR")
```

will list them after installation.

```
> vignette(package = "homomorpheR")
Vignettes in package 'homomorpheR':

introduction            Introduction to Homomorphic Computation
                        (source, html)
homomorphing            MLE using Homomorphic Computation (source,
                        html)
```

Then view, for example, as follows:

```{r}
vignette("introduction", package="homomorpheR")
```

Three examples are provided:

- A quick and easy introduction to homomorphic computations
- An example of Homomorphic Maximum Likelihood Estimation for a Poisson
  parameter.
- A larger example of Homomorphic Computations for fitting a
  Stratified Cox Regression model where the data is distributed across
  sites.

Eventually, as this library becomes industrial strength, it will be
incorporated into the computations defined in
[distcomp](https://cran.r-project.org/package=distcomp).

