\name{plot-methods}
\docType{methods}
\alias{plot.performance}
\alias{plot-methods}
\alias{plot,performance-method}
\alias{plot,performance,missing-method}

\title{Plot method for performance objects}

\description{This is the method to plot all objects of class
  performance.}

\usage{
\S4method{plot}{performance,missing}(x, y, ..., avg="none", spread.estimate="none",
  spread.scale=1, show.spread.at=c(), colorize=FALSE,
  colorize.palette=rev(rainbow(256,start=0, end=4/6)),
  colorkey=colorize, colorkey.relwidth=0.25, colorkey.pos="right",
  print.cutoffs.at=c(), cutoff.label.function=function(x) { round(x,2) },
  downsampling=0, add=FALSE )
}

\arguments{
  \item{x}{an object of class \code{performance}}
  \item{y}{not used}
  \item{...}{Optional graphical parameters to adjust different components of
    the performance plot. Parameters are directed to their target component by
    prefixing them with the name of the component
    (\code{component.parameter}, e.g. \code{text.cex}). The following
    components are available: \code{xaxis}, \code{yaxis},
    \code{coloraxis}, \code{box} (around the plotting region),
    \code{points}, \code{text}, \code{plotCI} (error bars),
    \code{boxplot}. The names of these components are influenced by the R
    functions that are used to create them. Thus, \code{par(component)}
    can be used to see which parameters are available for a
    given component (with the expection of the three axes; use
    \code{par(axis)} here). To adjust the canvas or the performance
    curve(s), the standard \code{plot} parameters can be used without any prefix.}
  \item{avg}{If the performance object describes several curves
    (from cross-validation runs or bootstrap evaluations of one
    particular method), the curves from each of the runs can be
    averaged. Allowed values are \code{none} (plot all curves separately), \code{horizontal}
    (horizontal averaging), \code{vertical} (vertical averaging), and
    \code{threshold} (threshold (=cutoff) averaging). Note that while
    threshold averaging is always feasible, vertical and horizontal
    averaging are not well-defined if the graph cannot be represented as
    a function x->y and y->x, respectively.}
  \item{spread.estimate}{When curve averaging is enabled, the variation
    around the average curve can be visualized as standard error bars
    (\code{stderror}), standard deviation bars (\code{stddev}), or by using
    box plots (\code{boxplot}). Note that the function \code{plotCI},
    which is used internally by ROCR to draw error bars, might raise a
    warning if the spread of the curves at certain positions is 0.}
  \item{spread.scale}{For \code{stderror} or \code{stddev}, this is a
    scalar factor to be multiplied with the length of the standard
    error/deviation bar. For example, under normal assumptions,
    \code{spread.scale=2} can be used to get approximate 95\% confidence
    intervals.}
  \item{show.spread.at}{For vertical averaging, this vector determines the x
    positions for which the spread estimates should be
    visualized. In contrast, for horizontal and threshold averaging, the
    y positions and cutoffs are determined, respectively. By default,
    spread estimates are shown at 11 equally spaced positions.}
  \item{colorize}{This logical determines whether the curve(s) should be
    colorized according to cutoff.}
  \item{colorize.palette}{If curve colorizing is enabled, this determines the
    color palette onto which the cutoff range is mapped.}
  \item{colorkey}{If true, a color key is drawn into the 4\% border
    region (default of \code{par(xaxs)} and \code{par(yaxs)}) of the
    plot. The color key visualizes the mapping from cutoffs to colors.}
  \item{colorkey.relwidth}{Scalar between 0 and 1 that determines the
    fraction of the 4\% border region that is occupied by the colorkey.}
  \item{colorkey.pos}{Determines if the colorkey is drawn vertically at
    the \code{right} side, or horizontally at the \code{top} of the
    plot.}
  \item{print.cutoffs.at}{This vector specifies the cutoffs which should
    be printed as text along the curve at the corresponding curve positions.} 
  \item{cutoff.label.function}{By default, cutoff annotations along the
    curve or at the color key are rounded to two decimal places
    before printing. Using a custom \code{cutoff.label.function}, any other 
    transformation can be performed on the cutoffs instead (e.g. rounding with
    different precision or taking the logarithm).}
  \item{downsampling}{ROCR can efficiently compute most performance measures
    even for data sets with millions of elements. However, plotting of
    large data sets can be slow and lead to PS/PDF documents of
    considerable size. In that case, performance curves that are
    indistinguishable from the original can be obtained by using only a
    fraction of the computed performance values. Values for downsampling
    between 0 and 1 indicate the fraction of the original data set size to which
    the performance object should be downsampled, integers above
    1 are interpreted as the actual number of performance values to which the
    curve(s) should be downsampled.}
  \item{add}{If \code{TRUE}, the curve(s) is/are added to an already
    existing plot; otherwise a new plot is drawn.}
  
}

% \details{}

\references{A detailed list of references can be found on the ROCn'COST
  homepage at \url{http://rocr.bioinf.mpi-sb.mpg.de}.}

\author{Tobias Sing \email{tobias.sing@mpi-sb.mpg.de},
  Oliver Sander \email{osander@mpi-sb.mpg.de}}

% \note{}

\seealso{\code{\link{prediction}}, \code{\link{performance}},
  \code{\link{prediction-class}}, \code{\link{performance-class}}} 

\examples{
# plotting a ROC curve:
library(ROCR)
data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
perf <- performance( pred, "tpr", "fpr" )
plot( perf )

# To entertain your children, make your plots nicer
# using ROCR's flexible parameter passing mechanisms
# (much cheaper than a finger painting set)
par(bg="lightblue", mai=c(1.2,1.5,1,1))
plot(perf, main="ROCR fingerpainting toolkit", colorize=TRUE,
  xlab="Mary's axis", ylab="", box.lty=7, box.lwd=5,
  box.col="gold", lwd=17, colorkey.relwidth=0.5, xaxis.cex.axis=2,
  xaxis.col='blue', xaxis.col.axis="blue", yaxis.col='green', yaxis.cex.axis=2,
  yaxis.at=c(0,0.5,0.8,0.85,0.9,1), yaxis.las=1, xaxis.lwd=2, yaxis.lwd=3,
  yaxis.col.axis="orange", cex.lab=2, cex.main=2)
}
\keyword{hplot}
