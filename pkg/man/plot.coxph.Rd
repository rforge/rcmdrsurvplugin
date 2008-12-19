\name{plot.coxph}
\alias{plot.coxph}

\title{Plot Method for coxph Objects}
\description{
  Plots the predicted survival function from a \code{coxph} object, setting covariates to particular values.
}
\usage{
\method{plot}{coxph}(x, newdata, typical = mean,  byfactors=FALSE, 
  col = palette(), lty,  conf.level = 0.95, ...)
}

\arguments{
  \item{x}{a \code{coxph} object.}
  \item{newdata}{a data frame containing (combinations of) values to which predictors are set; optional.}
  \item{typical}{function to use to compute "typical" values of numeric predictors.}
  \item{byfactors}{if \code{TRUE}, different lines are drawn for each unique combination of factor values, 
    including strata; if \code{FALSE} (the default) distinct lines are drawn only for different
    strata, with all columns of the model matrix (including for factors) set to their means.}
  \item{col}{colors for lines.}
  \item{lty}{line-types for lines; if missing, defaults to 1 to number required.}
  \item{conf.level}{level for confidence intervals; note: whether or not confidence intervals are
    plotted is determined by \code{\link[survival]{plot.survfit}}, which \code{plot.coxph} calls; if a 
    \code{conf.int} argument is supplied it is passed through.}
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

\seealso{\code{\link[survival]{coxph}}, \code{\link[survival]{survfit.coxph}},
\code{\link[survival]{plot.survfit}}.}

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
