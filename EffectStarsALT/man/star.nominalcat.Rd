\name{star.nominalcat}
\alias{star.nominalcat}
\encoding{UTF-8}
\title{
Effect stars for multinomial logit models with category-specific covariates
}
\description{
Internal function called by \code{star.nominal} for multinomial logit models with category-specific covariates. 
}
\usage{
star.nominalcat(formula, data, xij, conf.int = FALSE, symmetric = TRUE, 
    pred.coding = "reference", printpvalues = TRUE, test.rel = TRUE, refLevel = 1, 
    maxit = 100, scale = TRUE, nlines = NULL, select = NULL, catstar = TRUE, 
    dist.x = 1, dist.y = 1, dist.cov = 1, dist.cat = 1, xpd = TRUE, main = "", 
    lwd.stars = 1.2, col.fill = "gray90", col.circle = "black", lwd.circle = 1, 
    lty.circle = "longdash", lty.conf = "dotted", cex.labels = 1, cex.cat = 0.8, 
    xlim = NULL, ylim = NULL)
}
\arguments{
  \item{formula}{
An object of class \dQuote{formula}. Formula for the multinomial logit model to be fitted and visualized.
}
  \item{data}{
An object of class \dQuote{data.frame} containing the covariates used in \code{formula}.
}
\item{xij}{
An object of class list, used if category-specific covariates are to be inlcuded. Every element is a formula referring to one of the category-specific covariates. For details see help for \code{xij} in \code{\link[VGAM]{vglm.control}} and the details below.
}
  \item{conf.int}{
If \code{TRUE}, confidence intervals are drawn.
}
  \item{symmetric}{
Which side constraint for the coefficients in the multinomial logit model shall be used for the plot?
Default \code{TRUE} uses symmetric side constraints, \code{FALSE} uses the reference category specified by \code{refLevel}.
}
  \item{pred.coding}{
Which coding for categorical predictors with more than two categories is to be used? 
Default \code{pred.coding="reference"} uses the first category as reference category, the alternative \code{pred.coding="effect"} uses effect coding equivalent to symmetric side constraints. For \code{pred.coding="effect"} a star for every category is plotted, for \code{pred.coding="reference"} no star for the reference category is plotted. 
}
  \item{printpvalues}{
If \code{TRUE}, p-values for the respective coefficients are printed besides the category labels.
P-values are recieved by a Wald test.
}
  \item{test.rel}{
Provides a Likelihood-Ratio-Test to test the relevance of the explanatory covariates.
The corresponding p-values will be printed behind the covariates labels. \code{test.rel=FALSE} might
save a lot of time.
}
  \item{refLevel}{
Reference category for multinomial logit model. Ignored if \code{symmetric=TRUE}. See also
\code{\link[VGAM]{multinomial}}.
}
  \item{maxit}{
Maximal number of iterations to fit the multinomial logit model. See also
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
\item{catstar}{
A logical argument to specify if all category-specific effects in the model should be visualized with an additional star. Ignored if \code{xij=NULL}. 
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
  \item{lwd.stars}{
Line width of the stars. See also \code{lwd} in \code{\link[graphics]{par}}.
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
\item{lty.conf}{
Line type of confidence intervals. Ignored, if \code{conf.int=FALSE}. See also \code{lty}
in \code{\link[graphics]{par}}.
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
For details see \code{\link[EffectStars]{star.nominal}}
}
\value{
P-values are only available if the corresponding option is set \code{TRUE}. \cr
\item{odds}{Odds or exponential coefficients of the multinomial logit model}
\item{coefficients}{Coefficients of the multinomial logit model}
\item{se}{Standard errors of the coefficients}
\item{pvalues}{P-values of Wald tests for the respective coefficients}
\item{catspec}{Coefficients for the category-specific covariates}
\item{catspecse}{Standard errors for the coefficients for the category-specific covariates}
\item{p_rel}{P-values of Likelihood-Ratio-Tests for the relevance of the explanatory covariates}
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
\code{\link{star.nominal}}, \code{\link{star.sequential}}, \code{\link{star.cumulative}}
}