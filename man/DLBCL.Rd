\name{DLBCL}
\alias{DLBCL}
\docType{data}
\title{Diffuse Large B-cell Lymphoma Cohort (Rosenwald et al. 2002)}
\description{
  Patient-level survival data and gene-expression signature scores
  from the diffuse large-B-cell lymphoma (DLBCL) cohort of
  Rosenwald et al. (2002). Used in the Cox-regression vignettes
  to demonstrate distributed Cox estimation under threshold FHE
  with sites partitioned by molecular subgroup.
}
\usage{data(DLBCL)}
\format{
  A data frame with 235 observations on the following 12 variables:
  \describe{
    \item{\code{ID}}{LYM patient identifier (integer).}
    \item{\code{Set}}{Original analysis set assignment, either
      \code{"Training"} or \code{"Validation"}.}
    \item{\code{Subgroup}}{Molecular subgroup, a factor with
      levels \code{"GCB"} (germinal-center B-cell-like),
      \code{"ABC"} (activated B-cell-like), and
      \code{"Type III"} (unclassified).}
    \item{\code{IPI}}{International Prognostic Index group
      (\code{"Low"}, \code{"Medium"}, \code{"High"}, or
      \code{NA}).}
    \item{\code{time}}{Follow-up time in years.}
    \item{\code{status}}{Vital status at last follow-up coded as
      \code{1} for death and \code{0} for alive at follow-up.}
    \item{\code{GCB_sig}}{Germinal-center B-cell signature score.}
    \item{\code{LN_sig}}{Lymph-node signature score.}
    \item{\code{Prolif_sig}}{Proliferation signature score.}
    \item{\code{BMP6}}{BMP6 expression score.}
    \item{\code{MHC2_sig}}{MHC class II signature score.}
    \item{\code{Score}}{Outcome predictor score combining the five
      signatures, as published.}
  }
}
\details{
  Each row represents one patient. The five signature columns
  (\code{GCB_sig}, \code{LN_sig}, \code{Prolif_sig}, \code{BMP6},
  \code{MHC2_sig}) are standardized expression-signature scores
  carried over from the published supplement. Following Bayle, Fan
  and Lou (2025), the five patients with zero follow-up time are
  excluded, so the cohort spans 235 patients with 133 deaths (event
  rate 56.6\%) over a median follow-up of 2.8 years. The
  molecular-subgroup partition gives three sites of unequal size:
  GCB (n=115, 54 deaths), ABC (n=71, 49 deaths), and Type III
  (n=49, 30 deaths).

  Splitting by \code{Subgroup} for distributed Cox estimation is
  biologically meaningful: GCB, ABC, and Type III tumors arise
  from different cells of origin and are routinely diagnosed at
  different referral centers. Stratified Cox regression with
  \code{strata(Subgroup)} factors the partial log-likelihood
  additively across the three subgroups, which is exactly the
  decomposition the master/worker protocol exploits.
}
\source{
  Web supplement to Rosenwald et al. (2002), available from the
  New England Journal of Medicine.
}
\references{
  Rosenwald, A., Wright, G., Chan, W. C., et al. (2002). The use
  of molecular profiling to predict survival after chemotherapy
  for diffuse large-B-cell lymphoma. \emph{New England Journal of
  Medicine} \bold{346}(25), 1937--1947. \doi{10.1056/NEJMoa012914}

  Bayle, P., Fan, J., and Lou, Z. (2025). Communication-Efficient
  Distributed Estimation and Inference for Cox's Model.
  \emph{Journal of the American Statistical Association}.
  \doi{10.1080/01621459.2025.2516820}
}
\seealso{
  \code{\link{DLBCL_gex}} for the full Lymphochip gene-expression
  matrix (235 x 6416) on the same cohort.
}
\examples{
data(DLBCL)
table(DLBCL$Subgroup, DLBCL$status)

## Stratified Cox fit on the five signatures
if (requireNamespace("survival", quietly = TRUE)) {
  fit <- survival::coxph(
    survival::Surv(time, status) ~ GCB_sig + LN_sig + Prolif_sig +
      BMP6 + MHC2_sig + survival::strata(Subgroup),
    data = DLBCL)
  print(fit)
}
}
\keyword{datasets}
