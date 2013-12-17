\name{coffee}
\alias{coffee}
\docType{data}
\encoding{UTF-8}
\title{
Coffee Brands
}
\description{
The data frame is part of a long-term panel about the choice of coffee brands in 2111 households.
The explanatory variables either refer to the household as a whole or to the head of the household.
}
\usage{data(coffee)}
\format{
  A data frame with 2111 observations on the following 8 variables.
  \describe{
    \item{\code{Education}}{Educational level with levels \code{no Highschool} and \code{Highschool}}
    \item{\code{PriceSensitivity}}{Price sensitivity with levels \code{not sensitive} and \code{sensitive}}
    \item{\code{Income}}{Income with levels \code{< 2499} and \code{>= 2500}}
    \item{\code{SocialLevel}}{Social level with levels \code{high} and \code{low}}
    \item{\code{Age}}{Age with levels \code{< 49} and \code{>= 50}}
    \item{\code{Brand}}{Coffee Brand with levels \code{Jacobs}, \code{JacobsSpecial}, \code{Aldi}, \code{AldiSpecial}, \code{Eduscho}, \code{EduschoSpecial}, \code{Tchibo}, \code{TchiboSpecial} and \code{Others}}
    \item{\code{Amount}}{Amount of packs with levels \code{1} and \code{>= 2}}
    \item{\code{Persons}}{Number of persons in household}
  }
}
\source{
\url{http://www.stat.uni-muenchen.de/service/datenarchiv/}
}
\references{
Gesellschaft \enc{für}{fuer} Konsumforschung (GfK)
}
\examples{
\dontrun{
data(coffee)
vignette("coffee")
}
}
\keyword{datasets}
\keyword{multinomial response}
