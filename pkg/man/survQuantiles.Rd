\name{survQuantiles}
\alias{survQuantiles}
\alias{survQuantiles.survfit}

\title{Compute Quantiles for a Survival Function}
\description{
 This function computes quantiles for a survival function (or survival 
 functions by strata) as returned by \code{\link[survival]{survfit}}.
 Quantiles are computed by monotone spline interpolation.
}

\usage{
survQuantiles(object, quantiles, ...)
\method{survQuantiles}{survfit}(object, quantiles = c(0.25, 0.5, 0.75), ...)
}

\arguments{
  \item{object}{a \code{survfit} object.}
  \item{quantiles}{quantiles to compute, default is 0.25, 0.5, and 0.75.}
  \item{\dots}{not used.}
}

\value{
A vector of quantiles (or a matrix, if there are strata).
}

\author{John Fox <jfox@mcmaster.ca>}

\seealso{\code{\link[survival]{survfit}}.}

\examples{
require(survival)
survQuantiles(survfit(Surv(time, status), data=cancer))
survQuantiles(survfit(Surv(time, status) ~ sex, data=cancer))
}

\keyword{survival}
