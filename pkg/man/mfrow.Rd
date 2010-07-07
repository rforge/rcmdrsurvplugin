\name{mfrow}
\alias{mfrow}

\title{Function to Compute Layout for Plot Array}
\description{
Given a number of plots \code{n}, find a arrangement for showing the plots in an array, 
set by \code{par(mfrow=mfrow(n))}.
}
\usage{
mfrow(n, max.plots = 0)
}

\arguments{
  \item{n}{number of plots}
  \item{max.plots}{maximum number of plots; \code{0}, the default, means no maximum.}
}

\author{John Fox <jfox@mcmaster.ca>}

\seealso{\code{par}}

\examples{
mfrow(4)
mfrow(5)
mfrow(6)
}

\keyword{dplot}