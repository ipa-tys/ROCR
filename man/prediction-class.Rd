\name{prediction-class}
\docType{class}
\alias{prediction-class}

\title{Class "prediction"}
\description{Object to encapsulate numerical predictions together with the
  corresponding true class labels, optionally collecting predictions and
  labels for several cross-validation or bootstrapping runs.}

\section{Objects from the Class}{Objects can be created by using the
  \code{prediction} function.}

\section{Slots}{
  \describe{
    \item{\code{predictions}:}{A list, in which each element is a vector of
    predictions (the list has length > 1 for x-validation data.)}
    \item{\code{labels}:}{Analogously, a list in which each element is a
    vector of true class labels.}
    \item{\code{cutoffs}:}{A list in which each element is a vector of
      all necessary cutoffs. Each cutoff vector consists of the
      predicted scores (duplicates removed), in descending order.}
    \item{\code{fp}:}{A list in which each element is a vector of the number (not
    the rate!) of false positives induced by the cutoffs given in the
    corresponding 'cutoffs' list entry.}
    \item{\code{tp}:}{As fp, but for true positives.}
    \item{\code{tn}:}{As fp, but for true negatives.}
    \item{\code{fn}:}{As fp, but for false negatives.}
    \item{\code{n.pos}:}{A list in which each element contains the number of
    positive samples in the given x-validation run.}
    \item{\code{n.neg}:}{As n.pos, but for negative samples.}
    \item{\code{n.pos.pred}:}{A list in which each element is a vector
      of the number of samples predicted as positive at the cutoffs
      given in the corresponding 'cutoffs' entry.}
    \item{\code{n.neg.pred}:}{As n.pos.pred, but for negatively
      predicted samples.}
  }
}

\note{Every \code{prediction} object contains information about the 2x2
  contingency table consisting of tp,tn,fp, and fn, along with the
  marginal sums n.pos,n.neg,n.pos.pred,n.neg.pred, because these form
  the basis for many derived performance measures.}

\references{A detailed list of references can be found on the ROCR
  homepage at \url{http://rocr.bioinf.mpi-sb.mpg.de}.}

\author{Tobias Sing \email{tobias.sing@mpi-sb.mpg.de},
  Oliver Sander \email{osander@mpi-sb.mpg.de}}

\seealso{\code{\link{prediction}}, \code{\link{performance}},
  \code{\link{performance-class}}, \code{\link{plot.performance}} } 

\keyword{classes}
