\name{alligator}
\alias{alligator}
\docType{data}
\encoding{UTF-8}
\title{
Alligator Food
}
\description{
The data describe the food choice of alligators,
they originate from a study of the Florida Game and Fresh Water Commission.
}
\usage{data(alligator)}
\format{
  A data frame with 219 observations on the following 4 variables.
  \describe{
    \item{\code{Food}}{Food type with levels \code{bird}, \code{fish}, \code{invert}, \code{other} and \code{rep}}
    \item{\code{Size}}{Size of the alligator with levels \code{<2.3} and \code{>2.3}}
    \item{\code{Gender}}{Gender with levels \code{female} and \code{male}}
    \item{\code{Lake}}{Name of the lake with levels \code{George}, \code{Hancock}, \code{Oklawaha} and \code{Trafford}}
  }
}

\source{
\url{http://www.stat.ufl.edu/~aa/cda/sas/sas.html}
}
\references{
Agresti (2002): \emph{Categorical Data Analysis}, Wiley.
}
\examples{
\dontrun{
data(alligator)
vignette("alligator")
}
}
\keyword{datasets}
\keyword{multinomial response}
