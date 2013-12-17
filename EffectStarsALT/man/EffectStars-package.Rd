\name{EffectStars}
\alias{EffectStars-package}
\alias{EffectStars}
\docType{package}
\encoding{UTF-8}
\title{
Visualization of Categorical Response Models
}
\description{
The package provides functions that visualize categorical regression models.\cr
Included models are the multinomial logit model, the sequential logit model and the
cumulative logit model.\cr
The exponentials of the effects of the predictors are plotted as star plots showing the strengths of the effects.\cr
In addition p-values for the effect of predictors are given.\cr
Various data sets and examples are provided.\cr
The plots should in general be exported to file formats like pdf, ps or png to recieve the optimal display. Plotting in R devices may not provide the optimal results.
\cr \cr
For further details see \code{\link{star.nominal}}, \code{\link{star.sequential}}  and \code{\link{star.cumulative}}.
}

\author{
Gunther Schauberger\cr
\email{gunther.schauberger@stat.uni-muenchen.de}\cr
\url{http://www.statistik.lmu.de/~schauberger/}
}
\references{
Tutz, G. and Schauberger, G. (2012): \emph{Visualization of Categorical Response Models -
from Data Glyphs to Parameter Glyphs}, Department of Statistics, LMU Munich, Technical Report 117.
\cr \cr
Gerhard Tutz (2012): \emph{Regression for Categorical Data}, Cambridge University Press
}
\seealso{
\code{\link{star.nominal}}, \code{\link{star.sequential}}, \code{\link{star.cumulative}}
}
\keyword{ package }
\keyword{multinomial response}
\keyword{ordinal response}
\keyword{multinomial logit model}
\keyword{sequential logit model}
\keyword{cumulative logit model}
\keyword{categorical data}
\examples{
\dontrun{
vignette("alligator")
vignette("BEPS")
vignette("coffee")
vignette("election")
vignette("insolvency")
vignette("PID")  
vignette("plebiscite")
vignette("womenlabour")
}
}
