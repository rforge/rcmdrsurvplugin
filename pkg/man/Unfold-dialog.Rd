\name{Unfold-dialog}
\alias{Unfold-dialog}
\alias{Unfold}

\title{Dialog to Convert a Survival Data Set from "Wide" to "Long" Format}
\description{
  Converts a survival-analysis data frame from "wide" format, in which time-varying covariates
  are separate variables, one per occasion, to "long" or counting-process format in which each
  occasion is a separate row in the data frame.
}

\usage{
Unfold() # called via the R Commander menus
}

\details{
Most of the dialog box is self-explanatory. A time-varying covariate is identified
by selecting the variables constituting the covariate in the "wide" version of the data set 
using the variable-list box at the lower-left; specifying a name to be used
for the covariate in the "long" version of the data set; and pressing the \emph{Select} button. 
This process is repeated for each time-varying covariate. All time-varying covariates have to
be measured on the same occasions, which are assigned times 0, 1, ... in the output data set. If the
covariates are to be lagged, this is indicated via the \emph{Lag covariates} slider near the
lower right. The default lag is 0 --- i.e., no lag. The output data set will include variables named 
\code{start} and \code{stop}, which give the counting-process start and stop times for each
row, and an event indicator composed of the name of the event indicator in the "wide" form of the
data set and the suffix \code{.time}.

The \emph{Unfold} dialog calls the \code{\link{unfold}} function, which is somewhat more flexible.
}

\author{John Fox <jfox@mcmaster.ca>}

\seealso{\code{\link{unfold}}}
