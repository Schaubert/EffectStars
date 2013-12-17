\name{plebiscite}
\alias{plebiscite}
\docType{data}
\encoding{UTF-8}
\title{
Chilean Plebiscite
}
\description{
The data origin from a survey refering to the plebiscite in Chile 1988. The
chilean people had to decide, wether Augusto Pinochet would remain president for another
ten years (voting yes) or if there would be presidential elections in 1989 (voting no).
}
\usage{data(plebiscite)}
\format{
  A data frame with 2431 observations on the following 7 variables.
  \describe{
    \item{\code{Gender}}{Gender with levels \code{female} and \code{male}}
    \item{\code{Education}}{Educational level with levels \code{low} and \code{high}}
    \item{\code{SantiagoCity}}{Respondent from Santiago City with levels \code{no} and \code{yes}}
    \item{\code{Income}}{Monthly Income in Pesos}
    \item{\code{Population}}{Population size of respondent's community}
    \item{\code{Age}}{Age in years}
    \item{\code{Vote}}{Response with levels \code{Abstention}, \code{No}, \code{Undecided} and \code{Yes}}
  }
}
\source{
R package car: \code{\link[car]{Chile}}
}
\references{
Personal communication from FLACSO/Chile.
\cr
Fox, J. (2008): \emph{Applied Regression Analysis and Generalized Linear Models}, Second Edition.
}
\examples{
\dontrun{
data(plebiscite)
vignette("plebiscite")
}
}
\keyword{datasets}
\keyword{multinomial response}
