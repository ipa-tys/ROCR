\name{ROCR.simple}
\alias{ROCR.simple}
\docType{data}
\title{Data set: Simple artificial prediction data for use with ROCR}
  
\description{
  A mock data set containing a simple set of predictions and
  corresponding class labels.
}

\usage{data(ROCR.simple)}

\format{A two element list. The first element,
  \code{ROCR.simple$predictions}, is a vector of numerical predictions. The
  second element, \code{ROCR.simple$labels}, is a vector of
  corresponding class labels.}

\examples{
# plot a ROC curve for a single prediction run
# and color the curve according to cutoff.
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
}
\keyword{datasets}
