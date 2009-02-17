\name{anova.coxph}
\alias{anova.coxph}

\title{anova method for coxph objects}
\description{
  This function masks \code{\link[survival]{anova.coxph}} in the
  \pkg{survival} package; its sole purpose is to supply the argument
  \code{test = "Chisq"} as the default.
}
\usage{
\method{anova}{coxph}(object, ..., test = "Chisq")
}

\arguments{
  \item{object}{see \code{\link[survival]{anova.coxph}}.}
  \item{\dots}{\code{\link[survival]{anova.coxph}}.}
  \item{test}{\code{\link[survival]{anova.coxph}}.}
}

\author{John Fox <jfox@mcmaster.ca>}

\seealso{\code{\link[survival]{anova.coxph}}}

\keyword{htest}
\keyword{survival}
