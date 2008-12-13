\name{plot.coxph}
\alias{plot.coxph}

\title{Plot Method for coxph Objects}
\description{
  Plots the predicted survival function from a \code{coxph} object, setting covariates to particular values.
}
\usage{
\method{plot}{coxph}(x, newdata, typical = mean, col = palette(), 
  lty = 1:nrow(newdata), ...)
}

\arguments{
  \item{x}{a \code{coxph} object.}
  \item{newdata}{a data frame containing (combinations of) values to which predictors are set; optional.}
  \item{typical}{function to use to compute "typical" values of numeric predictors.}
  \item{col}{colors for lines.}
  \item{lty}{line-types for lines.}
  \item{\ldots}{arguments passed to \code{plot}.}
}
\details{
  If \code{newdata} is missing then all combinations of levels of factor-predictors (or strata), 
  if present, are combined with "typical" values of numeric predictors.
}
\value{
 Invisibly returns the \code{summary} resulting from applying \code{\link[survival]{survfit.coxph}} 
 to the \code{coxph} object.
}

\author{John Fox \email{jfox@mcmaster.ca}.}

\seealso{\code{\link[survival]{coxph}}, \code{\link[survival]{survfit.coxph}}.}

\examples{
require(survival)
cancer$sex <- factor(ifelse(cancer$sex == 1, "male", "female"))

mod.1 <- coxph(Surv(time, status) ~ age + wt.loss, data=cancer)
plot(mod.1)
plot(mod.1, typical=function(x) quantile(x, c(.25, .75)))

mod.2 <- coxph(Surv(time, status) ~ age + wt.loss + sex, data=cancer)
plot(mod.2)

mod.3 <- coxph(Surv(time, status) ~ (age + wt.loss)*sex, data=cancer)
plot(mod.3)

mod.4 <- coxph(Surv(time, status) ~ age + wt.loss + strata(sex), data=cancer)
plot(mod.4)

mods.1 <- survreg(Surv(time, status) ~ age + wt.loss, data=cancer)
}

\keyword{hplot}
\keyword{survival}
