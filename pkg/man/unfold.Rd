\name{unfold}
\alias{unfold}
\alias{unfold.data.frame}

\title{Convert a Survival Data Set from "Wide" to "Long" Format}
\description{
  Converts a survival-analysis data frame from "wide" format, in which time-varying covariates
  are separate variables, one per occasion, to "long" or counting-process format in which each
  occasion is a separate row in the data frame.
}
\usage{
unfold(data, ...)

\method{unfold}{data.frame}(data, time, event, cov, 
  cov.names = paste("covariate", ".", 1:ncovs, sep = ""), 
  suffix = ".time", cov.times = 0:ncov, common.times = TRUE, lag = 0, 
  show.progress=TRUE, ...)
}

\arguments{
  \item{data}{a data frame to be "unfolded" from wide to long.}
  \item{time}{the column number or quoted name of the event/censoring-time variable in data.}
  \item{event}{the column number or quoted name of the event/censoring-indicator variable in data.}
  \item{cov}{a vector giving the column numbers of the time-dependent covariate
	in \code{data}, or a list of vectors if there is more than one time-varying
	covariate.}
  \item{cov.names}{a character string or character vector giving the name or names
	to be assigned to the time-dependent covariate(s) in the output data set.}
  \item{suffix}{the suffix to be attached to the name of the time-to-event variable
	in the output data set; defaults to '.time'.}
  \item{cov.times}{the observation times for the covariate values, including the start
	time. This argument can take several forms: (1) The default is integers from 0 to 
	the number of covariate values (i.e., one more than the length of each vector in \code{cov}).
	(2) An arbitrary numerical vector with one more entry than the length of each
	vector in \code{cov}. (3) The columns in the input data set that give the observations times for each
	individual. There should be one more column than the length of each vector in \code{cov}.}
  \item{common.times}{a logical value indicating whether the times of observation
	are the same for all individuals; defaults to \code{TRUE}.}
  \item{lag}{number of observation periods to lag each value of the time-varying
	covariate(s); defaults to \code{0}.}
  \item{show.progress}{if \code{TRUE}, the default, show a progress bar as the observations are processed.}
  \item{\dots}{arguments to be passed down.}
}

\value{
A data frame containing the "long" version of the data set.
}


\author{John Fox <jfox@mcmaster.ca>}


\examples{
if (interactive()){
	head(Rossi, 2)
	Rossi.long <- unfold(Rossi, time="week", event="arrest", cov=11:62, 
	  cov.names="emp")
	head(Rossi.long, 50)
}
}

\keyword{manip}
