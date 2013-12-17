\name{election}
\alias{election}
\docType{data}
\encoding{UTF-8}
\title{
Election Data
}
\description{
The data set contains data from the German Longitudinal Election Study. The Response Categories refer to
the five dominant parties in Germany. The explanatory variables refer to the declarations of single voters.
}
\usage{data(election)}
\format{
  A data frame with 816 observations on the following 30 variables.
  \describe{
    \item{\code{Age}}{Standardized age of the voter}
    \item{\code{AgeOrig}}{Unstandardized age of the voter}
    \item{\code{Partychoice}}{Party Choice with levels \code{CDU}, \code{SPD}, \code{FDP}, \code{Greens} and \code{Left Party}}
    \item{\code{Gender}}{Gender with levels \code{female} and \code{male}}
    \item{\code{West}}{Regional provenance (West-Germany or East-Germany) with levels \code{east} and \code{west}}
    \item{\code{Union}}{Member of a Union with levels \code{no member} and \code{member}}
    \item{\code{Highschool}}{Educational level with levels \code{no highschool} and \code{highschool}}
    \item{\code{Unemployment}}{Unemployment with levels \code{not unemployed} and \code{unemployed}}
    \item{\code{Pol.Interest}}{Political Interest with levels \code{very interested} and \code{less interested}}
    \item{\code{Democracy}}{Satisfaction with the functioning of democracy with levels \code{satisfied} and \code{not satisfied}}
    \item{\code{Religion}}{Religion with levels \code{evangelical}, \code{catholic} and \code{other religion}}
    \item{\code{Social_CDU}}{Difference in attitude towards the socioeconomic dimension of politics between respondent and CDU}
    \item{\code{Social_SPD}}{Difference in attitude towards the socioeconomic dimension of politics between respondent and SPD}
    \item{\code{Social_FDP}}{Difference in attitude towards the socioeconomic dimension of politics between respondent and FDP}
    \item{\code{Social_Greens}}{Difference in attitude towards the socioeconomic dimension of politics between respondent and the Greens}
    \item{\code{Social_Left}}{Difference in attitude towards the socioeconomic dimension of politics between respondent and the Left party}
    \item{\code{Immigration_CDU}}{Difference in attitude towards immigration of foreigners between respondent and CDU}
    \item{\code{Immigration_SPD}}{Difference in attitude towards immigration of foreigners between respondent and SPD}
    \item{\code{Immigration_FDP}}{Difference in attitude towards immigration of foreigners between respondent and FDP}
    \item{\code{Immigration_Greens}}{Difference in attitude towards immigration of foreigners between respondent and the Greens}
    \item{\code{Immigration_Left}}{Difference in attitude towards immigration of foreigners between respondent and the Left party}
    \item{\code{Nuclear_CDU}}{Difference in attitude towards nuclear energy between respondent and CDU}
    \item{\code{Nuclear_SPD}}{Difference in attitude towards nuclear energy between respondent and SPD}
    \item{\code{Nuclear_FDP}}{Difference in attitude towards nuclear energy between respondent and FDP}
    \item{\code{Nuclear_Greens}}{Difference in attitude towards nuclear energy between respondent and the Greens}
    \item{\code{Nuclear_Left}}{Difference in attitude towards nuclear energy between respondent and the Left party}
    \item{\code{Left_Right_CDU}}{Difference in attitude towards the positioning on a political left-right scale between respondent and CDU}
    \item{\code{Left_Right_SPD}}{Difference in attitude towards the positioning on a political left-right scale between respondent and SPD}
    \item{\code{Left_Right_FDP}}{Difference in attitude towards the positioning on a political left-right scale between respondent and FDP}
    \item{\code{Left_Right_Greens}}{Difference in attitude towards the positioning on a political left-right scale between respondent and the Greens}
    \item{\code{Left_Right_Left}}{Difference in attitude towards the positioning on a political left-right scale between respondent and the Left party}
  }
}

\references{
German Longitudinal Election Study (GLES)
}
\examples{
data(election)
vignette("election")
}
\keyword{datasets}
\keyword{multinomial response}
