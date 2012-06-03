\name{quantile.survfit}
\alias{quantile.survfit}

\title{Compute Quantiles for a Survival Function}
\description{
 This methods computes quantiles for a survival function (or survival 
 functions by strata) as returned by \code{\link[survival]{survfit}}.
 Quantiles are computed conventionally as the last observed event time
 before each requested quantile. Confidence intervals for the estimated
 quantiles are obtained from the confidence limits of the etimated survival 
 function.
}

\usage{
\method{quantile}{survfit}(x, quantiles = c(0.25, 0.5, 0.75), ...)
}

\arguments{
  \item{x}{a \code{survfit} object.}
  \item{quantiles}{quantiles to compute, default is 0.25, 0.5, and 0.75.}
  \item{\dots}{not used.}
}

\value{
A vector of quantiles (or a matrix, if there are strata).
}

\author{John Fox <jfox@mcmaster.ca>}

\references{
	John Fox, Marilia Sa Carvalho (2012).
    The RcmdrPlugin.survival Package: Extending the R Commander Interface to Survival Analysis.
    \emph{Journal of Statistical Software}, 49(7), 1-32.
} 

\seealso{\code{\link[survival]{survfit}}.}

\examples{
require(survival)
quantile(survfit(Surv(time, status) ~ 1, data=cancer))
quantile(survfit(Surv(time, status) ~ sex, data=cancer))
}

\keyword{survival}
