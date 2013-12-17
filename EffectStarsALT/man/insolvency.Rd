\name{insolvency}
\alias{insolvency}
\docType{data}
\encoding{UTF-8}
\title{
Insolvency data
}
\description{
The data set originates from the Munich founder study. The data were collected on business
founders who registered their new companies at the local chambers of commerce in Munich and surrounding
administrative districts. The focus was on survival of firms measured in 7 categories, the first six represent
failure in intervals of six months, the last category represents survival time beyond 36 months.
}
\usage{data(insolvency)}
\format{
  A data frame with 1224 observations on the following 16 variables.
  \describe{
    \item{\code{Insolvency}}{Survival of firms in ordered categories with levels \code{1} < \code{2} < \code{3} < \code{4} < \code{5} < \code{6} < \code{7}}
    \item{\code{Sector}}{Economic Sector with levels \code{industry}, \code{commerce} and \code{service industry}}
    \item{\code{Legal}}{Legal form with levels \code{small trade}, \code{one man business}, \code{GmBH} and \code{GbR, KG, OHG}}
    \item{\code{Location}}{Location with levels \code{residential area} and \code{business area}}
    \item{\code{New_Foundation}}{New Foundation or take-over with levels \code{new foundation} and \code{take-over}}
    \item{\code{Pecuniary_Reward}}{Pecuniary reward with levels \code{main} and \code{additional}}
    \item{\code{Seed_Capital}}{Seed capital with levels \code{< 25000} and \code{> 25000}}
    \item{\code{Equity_Capital}}{Equity capital with levels \code{no} and \code{yes}}
    \item{\code{Debt_Capital}}{Debt capital with levels \code{no} and \code{yes}}
    \item{\code{Market}}{Market with levels \code{local} and \code{national}}
    \item{\code{Clientele}}{Clientele with levels \code{wide spread} and \code{small}}
    \item{\code{Degree}}{Educational level with levels \code{no A-levels} and \code{A-Levels}}
    \item{\code{Gender}}{Gender with levels \code{female} and \code{male}}
    \item{\code{Experience}}{Professional experience with levels \code{< 10 years} and \code{> 10 years}}
    \item{\code{Employees}}{Number of employees with levels \code{0 or 1} and \code{> 2}}
    \item{\code{Age}}{Age of the founder at formation of the company}
  }
}

\source{
\enc{Münchner}{Muenchner} \enc{Gründer}{Gruender} Studie
}
\references{
\enc{Brüderl}{Bruederl}, J. and  \enc{Preisendörfer}{Preisendoerfer}, P. and Ziegler, R. (1996): \emph{Der Erfolg \enc{neugegründeter}{neugegruendeter} Betriebe: eine empirische Studie zu den Chancen und Risiken von \enc{Unternehmensgründungen}{Unternehmensgruendungen}},
Duncker & Humblot.
}
\examples{
\dontrun{
data(insolvency)
vignette("insolvency")
}
}
\keyword{datasets}
\keyword{ordinal response}
