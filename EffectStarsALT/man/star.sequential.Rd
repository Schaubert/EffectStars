\name{star.sequential}
\alias{star.sequential}
\encoding{UTF-8}
\title{
Effect stars for sequential logit models
}
\description{
The function computes and visualizes sequential logit models. The computation is done with help of
the package \pkg{VGAM}. The visualization is based on the function \code{\link{stars}} from the package \pkg{graphics}.
}
\usage{
star.sequential(formula, data, global = NULL, test.rel = TRUE, test.glob = FALSE,
    globcircle = FALSE, maxit = 100, scale = TRUE, nlines = NULL, select = NULL, 
    dist.x = 1, dist.y = 1, dist.cov = 1, dist.cat = 1, xpd = TRUE, main = "", 
    col.fill = "gray90", col.circle = "black", lwd.circle = 1, 
    lty.circle = "longdash", col.global = "black", lwd.global = 1, 
    lty.global = "dotdash", cex.labels = 1, cex.cat = 0.8, xlim = NULL, 
    ylim = NULL)
}
\arguments{
  \item{formula}{
An object of class \dQuote{formula}. Formula for the sequential logit model to be fitted an visualized.
}
  \item{data}{
An object of class \dQuote{data.frame} containing the covariates used in \code{formula}.
}
\item{global}{
Numeric vector to choose a subset of predictors to be included with global coefficients. Default is to include all coefficients category-specific. Numbers refer to total amount of predictors, including intercept and dummy variables.
}
  \item{test.rel}{
Provides a Likelihood-Ratio-Test to test the relevance of the explanatory covariates.
The corresponding p-values will be printed as \code{p-rel}. \code{test.rel=FALSE} might
save a lot of time. 
}
  \item{test.glob}{
Provides a Likelihood-Ratio-Test to test if a covariate has to be included as a category-specific covariate (in contrast to being global). The corresponding p-values will be printed as \code{p-global}. \code{test.glob=FALSE} and \code{globcircle=FALSE} might save a lot of time.
}
  \item{globcircle}{
If \code{TRUE}, additional circles that represent the global effects of the covariates are plotted. \code{test.glob=FALSE} and \code{globcircle=FALSE} might save a lot of time.
}
  \item{maxit}{
Maximal number of iterations to fit the sequential logit model. See also
\code{\link[VGAM]{vglm.control}}.
}
  \item{scale}{
If \code{TRUE}, the stars are scaled to equal maximal ray length.
}
  \item{nlines}{
If specified, \code{nlines} gives the number of lines in which the effect stars are plotted.
}
  \item{select}{
Numeric vector to choose only a subset of the stars to be plotted. Default is to plot all stars. Numbers refer to total amount of predictors, including intercept and dummy variables. 
}
  \item{dist.x}{
Optional factor to increase/decrease distances between the centers of the stars on the x-axis. Values greater than
1 increase, values smaller than 1 decrease the distances.
}
  \item{dist.y}{
Optional factor to increase/decrease distances between the centers of the stars on the y-axis. Values greater than
1 increase, values smaller than 1 decrease the distances.
}
  \item{dist.cov}{
Optional factor to increase/decrease distances between the stars and the covariates labels above the stars. Values greater than
1 increase, values smaller than 1 decrease the distances.
}
  \item{dist.cat}{
Optional factor to increase/decrease distances between the stars and the category labels around the stars. Values greater than
1 increase, values smaller than 1 decrease the distances.
}
  \item{xpd}{
If \code{FALSE}, all plotting is clipped to the plot region, if \code{TRUE}, all plotting is clipped to the figure region,
and if \code{NA}, all plotting is clipped to the device region. See also \code{\link[graphics]{par}}.
}
  \item{main}{
An overall title for the plot. See also \code{\link[graphics]{plot}}.
}
  \item{col.fill}{
Color of background of the circle. See also \code{col} in \code{\link[graphics]{par}}.
}
  \item{col.circle}{
Color of margin of the circle. See also \code{col} in \code{\link[graphics]{par}}.
}
  \item{lwd.circle}{
Line width of the circle. See also \code{lwd} in \code{\link[graphics]{par}}.
}
  \item{lty.circle}{
Line type of the circle. See also \code{lty} in \code{\link[graphics]{par}}.
}
  \item{col.global}{
Color of margin of the global effects circle. See also \code{col} in \code{\link[graphics]{par}}. Ignored, if \code{globcircle = FALSE}.
}
  \item{lwd.global}{
Line width of the global effects circle. See also \code{lwd} in \code{\link[graphics]{par}}. Ignored, if \code{globcircle = FALSE}.
}
  \item{lty.global}{
Line type of the global effects circle. See also \code{lty} in \code{\link[graphics]{par}}. Ignored, if \code{globcircle = FALSE}.
}
  \item{cex.labels}{
Size of labels for covariates placed above the corresponding star. See also \code{cex}
in \code{\link[graphics]{par}}.
}
  \item{cex.cat}{
Size of labels for categories placed around the corresponding star. See also \code{cex}
in \code{\link[graphics]{par}}.
}
  \item{xlim}{
Optional specification of the x coordinates ranges. See also \code{xlim} in \code{\link[graphics]{plot.window}}
}
  \item{ylim}{
Optional specification of the y coordinates ranges. See also \code{ylim} in \code{\link[graphics]{plot.window}}
}
}
\details{
The underlying models are fitted with the function \code{\link[VGAM]{vglm}} from the package \pkg{VGAM}. The family argument 
for \code{vglm} is \code{sratio(parallel=FALSE)}. \cr \cr
The stars show the exponentials of the estimated coefficients. In sequential logit models the exponential coefficients can
be interpreted as odds. More precisely, the exponential \eqn{e^{\gamma_{rj}}, r=1,\ldots,k-1} represents the multiplicative effect of the covariate j on the continuation ratio odds \eqn{\frac{P(Y=r|x)}{P(Y>r|x)}} if \eqn{x_j} increases by one unit.\cr \cr
In addition to the stars, we plot a cirlce that refers to the case where the coefficients of the corresponding star are zero. Therefore, the radii of these circles are always \eqn{exp(0)=1}. If \code{scale=TRUE}, the stars are scaled so that they all have the same maximal ray length. In this case, the actual appearances of the circles differ, but they still refer to the no-effects case where all the coefficients are zero. Now the circles can be used to compare different stars based on their respective circles radii. The p-values beneath the covariate labels, which are given out if \code{test.rel=TRUE}, correspond to the distance between the circle and the star as a whole. They refer to a likelihood ratio test if all the coefficients from one covariate are zero (i.e. the variable is left out completely) and thus would lie exactly upon the cirlce.
\cr
The appearance of the circles can be modified by \code{col.circle}, \code{lwd.circle} and \code{lty.circle}. \cr \cr
By setting \code{globcircle=TRUE}, an addictional circle can be drawn. The radii now correspond to a model, where the respective covariate is not included category-specific but globally. Therefore, the distance between this circle and the star as a whole corresponds to the p-value p-global that is given if \code{test.glob=TRUE}.\cr \cr
It is strongly recommended to standardize metric covariates, display of effect stars can benefit greatly as in general differences between the coefficients are increased.
}

\value{
P-values are only available if the corresponding option is set \code{TRUE}. \cr
\item{odds}{Odds or exponential coefficients of the sequential logit model}
\item{coefficients}{Coefficients of the sequential logit model}
\item{se}{Standard errors of the coefficients}
\item{p_rel}{P-values of Likelihood-Ratio-Tests for the relevance of the explanatory covariates}
\item{p_global}{P-values of Likelihood-Ratio-Tests wether the covariates need to be included category-specific}
\item{xlim}{\code{xlim} values that were automatically produced. May be helpfull if you want to specify
your own \code{xlim}}
\item{ylim}{\code{ylim} values that were automatically produced. May be helpfull if you want to specify
your own \code{ylim}}
}

\references{
Tutz, G. and Schauberger, G. (2012): \emph{Visualization of Categorical Response Models -
from Data Glyphs to Parameter Glyphs}, Department of Statistics, LMU Munich, Technical Report 117.
\cr \cr
Gerhard Tutz (2012): \emph{Regression for Categorical Data}, Cambridge University Press
}
\author{
Gunther Schauberger\cr
\email{gunther.schauberger@stat.uni-muenchen.de}\cr
\url{http://www.statistik.lmu.de/~schauberger/}
}

\seealso{
\code{\link{star.nominal}}, \code{\link{star.cumulative}}
}
\examples{
\dontrun{
vignette("insolvency")
}
}

\keyword{ sequential logit model }
\keyword{ star plot }
\keyword{ordinal response}