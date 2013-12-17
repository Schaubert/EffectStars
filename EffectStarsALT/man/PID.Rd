\name{PID}
\alias{PID}
\docType{data}
\encoding{UTF-8}
\title{
Party Identification
}
\description{
Subset of the 1996 American National Election Study.
}
\usage{data(election)}
\format{
  A data frame with 944 observations on the following 6 variables.
  \describe{
    \item{\code{TVnews}}{Days in the past week spent watching news on TV}
    \item{\code{PID}}{Party identification with levels \code{Democrat}, \code{Independent} and \code{Republican}}
    \item{\code{Income}}{Income}
    \item{\code{Education}}{Educational level with levels \code{low} (no college) and \code{high} (at least college)}
    \item{\code{Age}}{Age in years}
    \item{\code{Population}}{Population of respondent's location in 1000s of people}
  }
}
\source{
R package faraway: \code{\link[faraway]{nes96}}
}
\references{
\url{http://www.stat.washington.edu/quinn}
}
\examples{
\dontrun{
data(PID)
vignette("PID")
}
}
\keyword{datasets}
\keyword{multinomial response}
