\name{star.nominal}
\alias{star.nominal}
\encoding{UTF-8}
\title{
Effect stars for multinomial logit models
}
\description{
The function computes and visualizes multinomial logit models. The computation is done with help of
the package \pkg{VGAM}. The visualization is based on the function \code{\link{stars}} from the package \pkg{graphics}.
}
\usage{
star.nominal(formula, data, xij = NULL, conf.int = FALSE, symmetric = TRUE, 
    pred.coding = "reference", printpvalues = TRUE, test.rel = TRUE, refLevel = 1, 
    maxit = 100, scale = TRUE, nlines = NULL, select = NULL, catstar = TRUE, 
    dist.x = 1, dist.y = 1, dist.cov = 1, dist.cat = 1, xpd = TRUE, main = "", 
    lwd.stars = 1, col.fill = "gray90", col.circle = "black", lwd.circle = 1, 
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
The underlying models are fitted with the function \code{\link[VGAM]{vglm}} from the package \pkg{VGAM}. The family argument 
for \code{vglm} is \code{multinomial(parallel=FALSE)}. \cr \cr
The stars show the exponentials of the estimated coefficients. In multinomial logit models the exponential coefficients can
be interpreted as odds. More precisely, for the model with symmetric side constraints, the exponential \eqn{e^{\gamma_{rj}}, r=1,\ldots,k} represents the multiplicative effect of the covariate j on the odds \eqn{\frac{P(Y=r|x)}{GM(x)}} if \eqn{x_j} increases by one unit and \eqn{GM(x)} is the median response. For the model with reference category k, the exponential \eqn{e^{\gamma_{rj}}, r=1,\ldots,k-1} represents the multiplicative effect of the covariate j on the odds \eqn{\frac{P(Y=r|x)}{P(Y=k|x)}} if \eqn{x_j} increases by one unit.\cr \cr
In addition to the stars, we plot a cirlce that refers to the case where the coefficients of the corresponding star are zero. Therefore, the radii of these circles are always \eqn{exp(0)=1}. If \code{scale=TRUE}, the stars are scaled so that they all have the same maximal ray length. In this case, the actual appearances of the circles differ, but they still refer to the no-effects case where all the coefficients are zero. Now the circles can be used to compare different stars based on their respective circles radii. The distances between the rays of a star and the cirlce correspond to the p-values that are printed beneath the category levels if \code{printpvalues=TRUE}. The closer a star ray lies to the no--effects circle, the more the p-value is increased. 
\cr
The p-values beneath the covariate labels, which are given  if \code{test.rel=TRUE}, correspond to the distance between the circle and the star as a whole. They refer to a likelihood ratio test if all the coefficients from one covariate are zero (i.e. the variable is left out completely) and thus would lie exactly upon the cirlce. 
\cr
The appearance of the circles can be modified by \code{col.circle}, \code{lwd.circle} and \code{lty.circle}. 
\cr \cr
The argument \code{xij} is  important  because it has to be used to include category-specific covariates. If its default \code{xij=NULL} is kept, an ordinary multinomial logit model without category-specific covariates is fitted. If category-specific covariates are to be included, attention has to be paid to the exact usage of \code{xij}. Our \code{xij} argument is identical to the \code{xij} argument used in the embedded \code{vglm} function. For details see also \code{\link[VGAM]{vglm.control}}. The data are thought to be present in a wide format, i.e. a category-specific covariate consists of k columns. Before calling \code{star.nominal}, the values for the reference category (defined by \code{refLevel}) have to be subtracted from the values of the further categories. Additionally, the resulting variable for the first response category (but not the reference category) has to be duplicated. This duplicate should be denoted by an appropriate name for the category-specific variable, independent from the different response categories. It will be used as an assignment variable for the corresponding coefficient of the covariate and has to be included in to the \code{formula}. For every category-specific covariate, a formula has to be specified in the \code{xij} argument. On the left hand side of that formula, the assignment variable has to be placed. On the right hand side, the variables containing the differences from the values for the reference category are written. So the left hand side of the formula contains k-1 terms. The order of these terms has to be chosen according to the order of the response categories, ignoring the reference category. Examples for effect stars for models with category-specific covariates are recieved by typing \code{vignette("election")} or \code{vignette("plebiscite")}.\cr \cr
It is strongly recommended to standardize metric covariates, display of effect stars can benefit greatly as in general differences between the coefficients are increased.
}
\value{
P-values are only available if the corresponding option is set \code{TRUE}. \cr
\code{catspec} and \code{catspecse} are only available if \code{xij} is specified.\cr
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
\code{\link{star.sequential}}, \code{\link{star.cumulative}}
}
\examples{
\dontrun{
vignette("election")
vignette("alligator")
vignette("coffee")
vignette("plebiscite")
vignette("PID")
vignette("BEPS")
vignette("womenlabour")
}
}
\keyword{ multinomial logit model }
\keyword{ star plot }
\keyword{multinomial response}