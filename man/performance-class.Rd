\name{performance-class}
\docType{class}
\alias{performance-class}

\title{Class "performance"}
\description{Object to capture the result of a performance evaluation,
  optionally collecting evaluations from several cross-validation or
  bootstrapping runs.}

\section{Objects from the Class}{Objects can be created by using the
  \code{performance} function.}

\section{Slots}{
  \describe{
    \item{\code{x.name}:}{Performance measure used for the x axis.}
    \item{\code{y.name}:}{Performance measure used for the y axis.}
    \item{\code{alpha.name}:}{Name of the unit that is used to create the parametrized
    curve. Currently, curves can only be parametrized by cutoff, so
    \code{alpha.name} is either \code{none} or \code{cutoff}.}
    \item{\code{x.values}:}{A list in which each entry contains the x values
    of the curve of this particular cross-validation run. x.values[[i]],
    y.values[[i]], and alpha.values[[i]] correspond to each other.}
    \item{\code{y.values}:}{A list in which each entry contains the y values
    of the curve of this particular cross-validation run.}
    \item{\code{alpha.values}:}{A list in which each entry contains the cutoff values
    of the curve of this particular cross-validation run.}
  }
}

\details{A \code{performance} object can capture information from four
  different evaluation scenarios:
  \itemize{
    \item The behaviour of a cutoff-dependent performance measure across
    the range of all cutoffs (e.g. \code{performance( predObj, 'acc' )} ). Here,
    \code{x.values} contains the cutoffs, \code{y.values} the
    corresponding values of the performance measure, and
    \code{alpha.values} is empty.\cr
    \item The trade-off between two performance measures across the
    range of all cutoffs (e.g. \code{performance( predObj,
      'tpr', 'fpr' )} ). In this case, the cutoffs are stored in
    \code{alpha.values}, while \code{x.values} and \code{y.values}
    contain the corresponding values of the two performance measures.\cr
    \item A performance measure that comes along with an obligatory
    second axis (e.g. \code{performance( predObj, 'ecost' )} ). Here, the measure values are
    stored in \code{y.values}, while the corresponding values of the
    obligatory axis are stored in \code{x.values}, and \code{alpha.values}
    is empty.\cr
    \item A performance measure whose value is just a scalar
    (e.g. \code{performance( predObj, 'auc' )} ). The value is then stored in
    \code{y.values}, while \code{x.values} and \code{alpha.values} are
    empty.
  }
}

  
\references{A detailed list of references can be found on the ROCR
  homepage at \url{http://rocr.bioinf.mpi-sb.mpg.de}.}

\author{Tobias Sing \email{tobias.sing@mpi-sb.mpg.de},
  Oliver Sander \email{osander@mpi-sb.mpg.de}}

\seealso{\code{\link{prediction}}, \code{\link{performance}},
  \code{\link{prediction-class}}, \code{\link{plot.performance}} } 

\keyword{classes}
