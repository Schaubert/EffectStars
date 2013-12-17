\name{womenlabour}
\alias{womenlabour}
\docType{data}
\encoding{UTF-8}
\title{
Canadian Women's Labour-Force Participation
}
\description{
The data are from a 1977 survey of the Canadian population.
}
\usage{data(womenlabour)}
\format{
  A data frame with 263 observations on the following 4 variables.
  \describe{
    \item{\code{Participation}}{Labour force participation with levels \code{fulltime}, \code{not.work} and \code{parttime}}
    \item{\code{IncomeHusband}}{Husband's income in 1000 $}
    \item{\code{Children}}{Presence od children in household with levels \code{absent} and \code{present}}
    \item{\code{Region}}{Region with levels \code{Atlantic}, \code{BC}, \code{Ontario}, \code{Prairie} and \code{Quebec}}
  }
}
\source{
R package car: \code{\link[car]{Womenlf}}
}
\references{
\emph{Social Change in Canada Project}. York Institute for Social Research.
\cr
Fox, J. (2008): \emph{Applied Regression Analysis and Generalized Linear Models}, Second Edition.
}
\examples{
\dontrun{
data(womenlabour)
vignette("womenlabour")
}
}
\keyword{datasets}
\keyword{multinomial response}
