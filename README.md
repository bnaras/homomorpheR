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

The first introduces simple homomorphic computations and the other
shows a simple example of Maximum Likelihood Estimation for a Poisson
parameter.

Eventually, as this library becomes industrial strength, it will be
incorporated into the computations defined in
[distcomp](http://cran.r-project.org/package=distcomp).

