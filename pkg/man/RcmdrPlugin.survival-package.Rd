\name{RcmdrPlugin.survival-package}
\alias{RcmdrPlugin.survival-package}
\alias{RcmdrPlugin.survival}
\docType{package}
\title{
Rcmdr Plug-In Package for the \pkg{survival} Package
}
\description{
An R Commander plug-in for the survival package,
with dialogs for managing survival data (this to a limited extent),
Cox models, parametric survival regression models,
estimation of survival curves, testing for differences in survival curves,
and a variety of diagnostics, tests, and displays.
}

\details{
\tabular{ll}{
Package: \tab RcmdrPlugin.survival\cr
Type: \tab Package\cr
Version: \tab 1.1-0\cr
Date: \tab 2016-04-06\cr
Depends:\tab survival, date, stats\cr
Imports:\tab Rcmdr (>= 2.2-1)\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
}

The plug-in is tightly integrated with the \emph{R Commander} interface; see the following menus: 
\strong{Data -> Survival data"}, \strong{Statistics -> Survival analysis}, 
\strong{Statistics -> Fit Models}, \strong{Models -> Hypothesis tests},
\strong{Models -> Numerical diagnostics}, \strong{Models -> Graphs}.
}

\author{
John Fox

Maintainer: John Fox \email{jfox@mcmaster.ca}
}

\references{
	John Fox, Marilia Sa Carvalho (2012).
    The RcmdrPlugin.survival Package: Extending the R Commander Interface to Survival Analysis.
    \emph{Journal of Statistical Software}, 49(7), 1-32.
} 

\section{Acknowledgments}{
I am grateful to Marilia Sa Carvalho, FIOCRUZ, Rio de Janeiro, Brazil, for many comments and suggestions, 
and to the following individuals for translations of messages into other languages: Philippe Grojean
(French), Matjaz Jeran (Slovenian), Anton Korobeinikov (Russian), Manuel Munoz Marquez (Spanish), and Marilia Sa Carvalho (Portuguese).
}

\keyword{ package }
