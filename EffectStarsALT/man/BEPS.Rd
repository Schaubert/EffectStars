\name{BEPS}
\alias{BEPS}
\docType{data}
\encoding{UTF-8}
\title{
British Election Panel Study
}
\description{
These data are drawn from the 1997-2001 British Election Panel Study (BEPS).
}
\usage{data(BEPS)}
\format{
  A data frame with 1525 observations on the following 10 variables.
  \describe{
    \item{\code{Europe}}{An 11-point scale that measures respondents' attitudes toward European integration. High scores represent eurosceptic sentiment}
    \item{\code{Leader_Cons}}{Assessment of the Conservative leader Hague, 1 to 5}
    \item{\code{Leader_Labour}}{Assessment of the Labour leader Blair, 1 to 5}
    \item{\code{Leader_Liberals}}{Assessment of the Liberals leader Kennedy, 1 to 5}
    \item{\code{Vote}}{Party Choice with levels \code{Conservative}, \code{Labour} and \code{Liberal Democrat}}
    \item{\code{Age}}{Age in years}
    \item{\code{Gender}}{Gender with levels \code{female} and \code{male}}
    \item{\code{Political_Knowledge}}{Knowledge of parties' positions on European integration, 0 to 3}
    \item{\code{National_Economy}}{Assessment of current national economic conditions, 1 to 5}
    \item{\code{Household}}{Assessment of current household economic conditions, 1 to 5}
  }
}
\source{
R package effects: \code{\link[effects]{BEPS}}
}
\references{
British Election Panel Study (BEPS)
\cr
J. Fox and R. Andersen (2006): \emph{Effect displays for multinomial and proportional-odds logit models}. Sociological Methodology 36, 225–255
}
\examples{
\dontrun{
data(BEPS)
vignette("BEPS")
}
}
\keyword{datasets}
\keyword{multinomial response}
