\name{Dialysis}
\alias{Dialysis}
\docType{data}
\title{
Hemodialysis Data from Brazil
}
\description{
This data set is analyzed in Sa Carvalho et al. (2003), and consists of data on 6805 hemodialysis patients
in all federally funded clinics in Rio de Janeiro State, Brazil.
}
\usage{data(Dialysis)}
\format{
  A data frame with 6805 observations on the following 7 variables.
  \describe{
    \item{\code{center}}{a numeric code indicating in which of 67 centers the patient was treated.}
    \item{\code{age}}{of the patient.}
    \item{\code{begin}}{The month in which treatment began, with \code{1} representing January 1998.}
    \item{\code{end}}{The month in which observation terminated, either because of death or censoring.
    	The study ended in month \code{44} (August, 2000).}
    \item{\code{event}}{\code{1}, death, or \code{0}, censoring.}
    \item{\code{time}}{the difference between \code{end} and \code{begin}.}
    \item{\code{disease}}{a factor with levels \code{congen}, (congenital); \code{diabetes}; \code{hypert} (hypertension); \code{other}; and \code{renal}.}
  }
}

\source{
	M. Sa Carvalho, R. Henderson, S. Shimakura, and I. P. S. C. Sousa (2003). 
	Survival of hemodialysis patients: Modeling differences in risk of dialysis centers. 
	\emph{International Journal for Quality in Health Care}, 15: 189--196.
}

\references{
	John Fox, Marilia Sa Carvalho (2012).
    The RcmdrPlugin.survival Package: Extending the R Commander Interface to Survival Analysis.
    \emph{Journal of Statistical Software}, 49(7), 1-32.
} 

\examples{
summary(Dialysis)
table(Dialysis$center)
}
\keyword{datasets}
