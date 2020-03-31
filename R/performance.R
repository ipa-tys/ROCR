
#' @name performance
#'
#' @title Function to create performance objects
#'
#' @description
#' All kinds of predictor evaluations are performed using this function.
#'
#' @details
#' Here is the list of available performance measures. Let Y and
#' \eqn{\hat{Y}}{Yhat} be random variables representing the class and the prediction for
#' a randomly drawn sample, respectively. We denote by
#' \eqn{\oplus}{+} and \eqn{\ominus}{-} the positive and
#' negative class, respectively. Further, we use the following
#' abbreviations for empirical quantities: P (\# positive
#' samples), N (\# negative samples), TP (\# true positives), TN (\# true
#' negatives), FP (\# false positives), FN (\# false negatives).
#' \describe{
#'  \item{\code{acc}:}{Accuracy. \eqn{P(\hat{Y}=Y)}{P(Yhat = Y)}. Estimated
#'    as: \eqn{\frac{TP+TN}{P+N}}{(TP+TN)/(P+N)}.}
#'  \item{\code{err}:}{Error rate. \eqn{P(\hat{Y}\ne Y)}{P(Yhat !=
#'                                                           Y)}. Estimated as: \eqn{\frac{FP+FN}{P+N}}{(FP+FN)/(P+N)}.}
#'  \item{\code{fpr}:}{False positive rate. \eqn{P(\hat{Y}=\oplus | Y =
#'                                                    \ominus)}{P(Yhat = + | Y = -)}. Estimated as:
#'      \eqn{\frac{FP}{N}}{FP/N}.}
#'  \item{\code{fall}:}{Fallout. Same as \code{fpr}.}
#'  \item{\code{tpr}:}{True positive
#'    rate. \eqn{P(\hat{Y}=\oplus|Y=\oplus)}{P(Yhat = + | Y = +)}. Estimated
#'    as: \eqn{\frac{TP}{P}}{TP/P}.}
#'  \item{\code{rec}:}{Recall. Same as \code{tpr}.}
#'  \item{\code{sens}:}{Sensitivity. Same as \code{tpr}.}
#'  \item{\code{fnr}:}{False negative
#'    rate. \eqn{P(\hat{Y}=\ominus|Y=\oplus)}{P(Yhat = - | Y =
#'                                                +)}. Estimated as: \eqn{\frac{FN}{P}}{FN/P}.}
#'  \item{\code{miss}:}{Miss. Same as \code{fnr}.}
#'  \item{\code{tnr}:}{True negative rate. \eqn{P(\hat{Y} =
#'                                                   \ominus|Y=\ominus)}{P(Yhat = - | Y = -)}.}
#'  \item{\code{spec}:}{Specificity. Same as \code{tnr}.}
#'  \item{\code{ppv}:}{Positive predictive
#'    value. \eqn{P(Y=\oplus|\hat{Y}=\oplus)}{P(Y = + | Yhat =
#'                                                +)}. Estimated as: \eqn{\frac{TP}{TP+FP}}{TP/(TP+FP)}.}
#'  \item{\code{prec}:}{Precision. Same as \code{ppv}.}
#'  \item{\code{npv}:}{Negative predictive
#'    value. \eqn{P(Y=\ominus|\hat{Y}=\ominus)}{P(Y = - | Yhat =
#'                                                  -)}. Estimated as: \eqn{\frac{TN}{TN+FN}}{TN/(TN+FN)}.}
#'  \item{\code{pcfall}:}{Prediction-conditioned
#'    fallout. \eqn{P(Y=\ominus|\hat{Y}=\oplus)}{P(Y = - | Yhat =
#'                                                   +)}. Estimated as: \eqn{\frac{FP}{TP+FP}}{FP/(TP+FP)}.}
#'  \item{\code{pcmiss}:}{Prediction-conditioned
#'    miss. \eqn{P(Y=\oplus|\hat{Y}=\ominus)}{P(Y = + | Yhat =
#'                                                -)}. Estimated as: \eqn{\frac{FN}{TN+FN}}{FN/(TN+FN)}.}
#'  \item{\code{rpp}:}{Rate of positive predictions. \eqn{P( \hat{Y} =
#'                                                            \oplus)}{P(Yhat = +)}. Estimated as: (TP+FP)/(TP+FP+TN+FN).}
#'  \item{\code{rnp}:}{Rate of negative predictions. \eqn{P( \hat{Y} =
#'                                                            \ominus)}{P(Yhat = -)}. Estimated as: (TN+FN)/(TP+FP+TN+FN).}
#'  \item{\code{phi}:}{Phi correlation coefficient. \eqn{\frac{TP \cdot
#'    TN - FP \cdot FN}{\sqrt{ (TP+FN) \cdot (TN+FP) \cdot (TP+FP)
#'      \cdot (TN+FN)}}}{(TP*TN -
#'                          FP*FN)/(sqrt((TP+FN)*(TN+FP)*(TP+FP)*(TN+FN)))}. Yields a
#'    number between -1 and 1, with 1 indicating a perfect
#'    prediction, 0 indicating a random prediction. Values below 0
#'    indicate a worse than random prediction.}
#'  \item{\code{mat}:}{Matthews correlation coefficient. Same as \code{phi}.}
#'  \item{\code{mi}:}{Mutual information. \eqn{I(\hat{Y},Y) := H(Y) -
#'      H(Y|\hat{Y})}{I(Yhat, Y) := H(Y) - H(Y | Yhat)}, where H is the
#'    (conditional) entropy. Entropies are estimated naively (no bias
#'                                                            correction).}
#'  \item{\code{chisq}:}{Chi square test statistic. \code{?chisq.test}
#'    for details. Note that R might raise a warning if the sample size
#'    is too small.}
#'  \item{\code{odds}:}{Odds ratio. \eqn{\frac{TP \cdot TN}{FN \cdot
#'    FP}}{(TP*TN)/(FN*FP)}. Note that odds ratio produces
#'    Inf or NA values for all cutoffs corresponding to FN=0 or
#'    FP=0. This can substantially decrease the plotted cutoff region.}
#'  \item{\code{lift}:}{Lift
#'    value. \eqn{\frac{P(\hat{Y}=\oplus|Y=\oplus)}{P(\hat{Y}=\oplus)}}{P(Yhat = + |
#'                                                                          Y = +)/P(Yhat = +)}.}
#'  \item{\code{f}:}{Precision-recall F measure (van Rijsbergen, 1979). Weighted
#'    harmonic mean of precision (P) and recall (R). \eqn{F =
#'      \frac{1}{\alpha \frac{1}{P} + (1-\alpha)\frac{1}{R}}}{F = 1/
#'        (alpha*1/P + (1-alpha)*1/R)}. If
#'    \eqn{\alpha=\frac{1}{2}}{alpha=1/2}, the mean is balanced. A
#'    frequent equivalent formulation is
#'    \eqn{F = \frac{(\beta^2+1) \cdot P \cdot R}{R + \beta^2 \cdot
#'      P}}{F = (beta^2+1) * P * R / (R + beta^2 * P)}. In this formulation, the
#'      mean is balanced if \eqn{\beta=1}{beta=1}. Currently, ROCR only accepts
#'      the alpha version as input (e.g. \eqn{\alpha=0.5}{alpha=0.5}). If no 
#'      value for alpha is given, the mean will be balanced by default.}
#'  \item{\code{rch}:}{ROC convex hull. A ROC (=\code{tpr} vs \code{fpr}) curve 
#'    with concavities (which represent suboptimal choices of cutoff) removed 
#'    (Fawcett 2001). Since the result is already a parametric performance 
#'    curve, it cannot be used in combination with other measures.}
#'  \item{\code{auc}:}{Area under the ROC curve. This is equal to the value of the
#'    Wilcoxon-Mann-Whitney test statistic and also the probability that the
#'    classifier will score are randomly drawn positive sample higher than a
#'    randomly drawn negative sample. Since the output of
#'    \code{auc} is cutoff-independent, this
#'    measure cannot be combined with other measures into a parametric
#'    curve. The partial area under the ROC curve up to a given false
#'    positive rate can be calculated by passing the optional parameter
#'    \code{fpr.stop=0.5} (or any other value between 0 and 1) to 
#'    \code{performance}.}
#'  \item{\code{aucpr}:}{Area under the Precision/Recall curve. Since the output
#'    of \code{aucpr} is cutoff-independent, this measure cannot be combined 
#'    with other measures into a parametric curve.}
#'  \item{\code{prbe}:}{Precision-recall break-even point. The cutoff(s) where
#'    precision and recall are equal. At this point, positive and negative
#'    predictions are made at the same rate as their prevalence in the
#'    data. Since the output of
#'    \code{prbe} is just a cutoff-independent scalar, this
#'    measure cannot be combined with other measures into a parametric curve.}
#'  \item{\code{cal}:}{Calibration error. The calibration error is the
#'    absolute difference between predicted confidence and actual reliability. This
#'    error is estimated at all cutoffs by sliding a window across the
#'    range of possible cutoffs. The default window size of 100 can be
#'    adjusted by passing the optional parameter \code{window.size=200}
#'    to \code{performance}. E.g., if for several
#'    positive samples the output of the classifier is around 0.75, you might
#'    expect from a well-calibrated classifier that the fraction of them
#'    which is correctly predicted as positive is also around 0.75. In a
#'    well-calibrated classifier, the probabilistic confidence estimates
#'    are realistic. Only for use with
#'    probabilistic output (i.e. scores between 0 and 1).}
#'  \item{\code{mxe}:}{Mean cross-entropy. Only for use with
#'    probabilistic output. \eqn{MXE :=-\frac{1}{P+N}( \sum_{y_i=\oplus}
#'                                                    ln(\hat{y}_i) + \sum_{y_i=\ominus} ln(1-\hat{y}_i))}{MXE := - 1/(P+N) \sum_{y_i=+}
#'                                                      ln(yhat_i) + \sum_{y_i=-} ln(1-yhat_i)}. Since the output of
#'    \code{mxe} is just a cutoff-independent scalar, this
#'    measure cannot be combined with other measures into a parametric curve.}
#'  \item{\code{rmse}:}{Root-mean-squared error. Only for use with
#'    numerical class labels. \eqn{RMSE:=\sqrt{\frac{1}{P+N}\sum_i (y_i
#'                                                                  - \hat{y}_i)^2}}{RMSE := sqrt(1/(P+N) \sum_i (y_i -
#'                                                                                                                  yhat_i)^2)}. Since the output of
#'    \code{rmse} is just a cutoff-independent scalar, this
#'    measure cannot be combined with other measures into a parametric curve.}
#'  \item{\code{sar}:}{Score combinining performance measures of different
#'    characteristics, in the attempt of creating a more "robust"
#'    measure (cf. Caruana R., ROCAI2004):
#'      SAR = 1/3 * ( Accuracy + Area under the ROC curve + Root
#'                    mean-squared error ).}
#'  \item{\code{ecost}:}{Expected cost. For details on cost curves,
#'    cf. Drummond&Holte 2000,2004. \code{ecost} has an obligatory x
#'    axis, the so-called 'probability-cost function'; thus it cannot be
#'    combined with other measures. While using \code{ecost} one is
#'    interested in the lower envelope of a set of lines, it might be
#'    instructive to plot the whole set of lines in addition to the lower
#'    envelope. An example is given in \code{demo(ROCR)}.}
#'  \item{\code{cost}:}{Cost of a classifier when
#'    class-conditional misclassification costs are explicitly given.
#'    Accepts the optional parameters \code{cost.fp} and
#'    \code{cost.fn}, by which the costs for false positives and
#'    negatives can be adjusted, respectively. By default, both are set
#'    to 1.}
#' }
#'
#' @note
#' Here is how to call \code{performance()} to create some standard
#' evaluation plots:
#' \describe{
#'   \item{ROC curves:}{measure="tpr", x.measure="fpr".}
#'   \item{Precision/recall graphs:}{measure="prec", x.measure="rec".}
#'   \item{Sensitivity/specificity plots:}{measure="sens", x.measure="spec".}
#'   \item{Lift charts:}{measure="lift", x.measure="rpp".}
#' }
#'
#' @param prediction.obj An object of class \code{prediction}.
#' @param measure Performance measure to use for the evaluation. A complete list
#'   of the performance measures that are available for \code{measure} and
#'   \code{x.measure} is given in the 'Details' section.
#' @param x.measure A second performance measure. If different from the default,
#'   a two-dimensional curve, with \code{x.measure} taken to be the unit in
#'   direction of the x axis, and \code{measure} to be the unit in direction of
#'   the y axis, is created. This curve is parametrized with the cutoff.
#' @param ... Optional arguments (specific to individual performance measures).
#'
#' @return An S4 object of class \code{performance}.
#'
#' @references
#' A detailed list of references can be found on the ROCR homepage at
#' \url{http://rocr.bioinf.mpi-sb.mpg.de}.
#'
#' @author
#' Tobias Sing \email{tobias.sing@gmail.com}, Oliver Sander
#' \email{osander@gmail.com}
#'
#' @seealso
#' \code{\link{prediction}},
#' \code{\link{prediction-class}},
#' \code{\link{performance-class}},
#' \code{\link{plot.performance}}
#'
#' @export
#'
#' @examples
#' # computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
#' library(ROCR)
#' data(ROCR.simple)
#' pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
#' pred
#' perf <- performance(pred,"tpr","fpr")
#' perf
#' plot(perf)
#'
#' # precision/recall curve (x-axis: recall, y-axis: precision)
#' perf <- performance(pred, "prec", "rec")
#' perf
#' plot(perf)
#'
#' # sensitivity/specificity curve (x-axis: specificity,
#' # y-axis: sensitivity)
#' perf <- performance(pred, "sens", "spec")
#' perf
#' plot(perf)
performance <- function(prediction.obj,
                        measure,
                        x.measure="cutoff",
                        ...) {

  ## define the needed environments
  envir.list <- .define.environments()
  long.unit.names <- envir.list$long.unit.names
  function.names <- envir.list$function.names
  obligatory.x.axis <- envir.list$obligatory.x.axis
  optional.arguments <- envir.list$optional.arguments
  default.values <- envir.list$default.values

  ## abort in case of misuse
  if (class(prediction.obj) != 'prediction' ||
      !exists(measure, where=long.unit.names, inherits=FALSE) ||
      !exists(x.measure, where=long.unit.names, inherits=FALSE)) {
    stop(paste("Wrong argument types: First argument must be of type",
               "'prediction'; second and optional third argument must",
               "be available performance measures!"))
  }

  ## abort, if attempt is made to use a measure that has an obligatory
  ## x.axis as the x.measure (cannot be combined)
  if (exists( x.measure, where=obligatory.x.axis, inherits=FALSE )) {
    message <- paste("The performance measure",
                     x.measure,
                     "can only be used as 'measure', because it has",
                     "the following obligatory 'x.measure':\n",
                     get( x.measure, envir=obligatory.x.axis))
    stop(message)
  }

  ## if measure is a performance measure with obligatory x.axis, then
  ## enforce this axis:
  if (exists( measure, where=obligatory.x.axis, inherits=FALSE )) {
    x.measure <- get( measure, envir=obligatory.x.axis )
  }

  if (x.measure == "cutoff" ||
      exists( measure, where=obligatory.x.axis, inherits=FALSE )) {

    ## fetch from '...' any optional arguments for the performance
    ## measure at hand that are given, otherwise fill up the default values
    optional.args <- list(...)
    argnames <- c()
    if ( exists( measure, where=optional.arguments, inherits=FALSE )) {
      argnames <- get( measure, envir=optional.arguments )
      default.arglist <- list()
      for (i in 1:length(argnames)) {
        default.arglist <- c(default.arglist,
                             get(paste(measure,":",argnames[i],sep=""),
                                 envir=default.values, inherits=FALSE))
      }
      names(default.arglist) <- argnames

      for (i in 1:length(argnames)) {
        templist <- list(optional.args,
                         default.arglist[[i]])
        names(templist) <- c('arglist', argnames[i])

        optional.args <- do.call('.farg', templist)
      }
    }
    optional.args <- .select.args( optional.args, argnames )

    ## determine function name
    function.name <- get( measure, envir=function.names )

    ## for each x-validation run, compute the requested performance measure
    x.values <- list()
    y.values <- list()
    for (i in 1:length( prediction.obj@predictions )) {
      argumentlist <- .sarg(optional.args,
                            predictions= prediction.obj@predictions[[i]],
                            labels= prediction.obj@labels[[i]],
                            cutoffs= prediction.obj@cutoffs[[i]],
                            fp= prediction.obj@fp[[i]],
                            tp= prediction.obj@tp[[i]],
                            fn= prediction.obj@fn[[i]],
                            tn= prediction.obj@tn[[i]],
                            n.pos= prediction.obj@n.pos[[i]],
                            n.neg= prediction.obj@n.neg[[i]],
                            n.pos.pred= prediction.obj@n.pos.pred[[i]],
                            n.neg.pred= prediction.obj@n.neg.pred[[i]])

      ans <- do.call( function.name, argumentlist )

      if (!is.null(ans[[1]])) x.values <- c( x.values, list( ans[[1]] ))
      y.values <- c( y.values, list( ans[[2]] ))
    }

    if (! (length(x.values)==0 || length(x.values)==length(y.values)) ) {
      stop("Consistency error.")
    }

    ## create a new performance object
    return( new("performance",
                x.name       = get( x.measure, envir=long.unit.names ),
                y.name       = get( measure, envir=long.unit.names ),
                alpha.name   = "none",
                x.values     = x.values,
                y.values     = y.values,
                alpha.values = list() ))
  } else {
    perf.obj.1 <- performance( prediction.obj, measure=x.measure, ... )
    perf.obj.2 <- performance( prediction.obj, measure=measure, ... )
    return( .combine.performance.objects( perf.obj.1, perf.obj.2 ) )
  }
}

#' @importFrom stats approxfun
.combine.performance.objects <- function( p.obj.1, p.obj.2 ) {
  ## some checks for misusage (in any way, this function is
  ## only for internal use)
  if ( p.obj.1@x.name != p.obj.2@x.name ) {
    stop("Error: Objects need to have identical x axis.")
  }
  if ( p.obj.1@alpha.name != "none" || p.obj.2@alpha.name != "none") {
    stop("Error: At least one of the two objects has already been merged.")
  }
  if (length(p.obj.1@x.values) != length(p.obj.2@x.values)) {
    stop(paste("Only performance objects with identical number of",
               "cross-validation runs can be combined."))
  }

  x.values <- list()
  x.name <- p.obj.1@y.name
  y.values <- list()
  y.name <- p.obj.2@y.name
  alpha.values <- list()
  alpha.name <- p.obj.1@x.name

  for (i in 1:length( p.obj.1@x.values )) {
    x.values.1 <- p.obj.1@x.values[[i]]
    y.values.1 <- p.obj.1@y.values[[i]]
    x.values.2 <- p.obj.2@x.values[[i]]
    y.values.2 <- p.obj.2@y.values[[i]]

    ## cutoffs of combined object = merged cutoffs of simple objects
    cutoffs <- sort( unique( c(x.values.1, x.values.2)), decreasing=TRUE )

    ## calculate y.values at cutoffs using step function
    y.values.int.1 <- stats::approxfun(x.values.1, y.values.1,
                                       method="constant",f=1,rule=2)(cutoffs)
    y.values.int.2 <- stats::approxfun(x.values.2, y.values.2,
                                       method="constant",f=1,rule=2)(cutoffs)

    ## 'approxfun' ignores NA and NaN
    objs <- list( y.values.int.1, y.values.int.2)
    objs.x <- list( x.values.1, x.values.2 )
    na.cutoffs.1.bool <- is.na( y.values.1) & !is.nan( y.values.1 )
    nan.cutoffs.1.bool <- is.nan( y.values.1)
    na.cutoffs.2.bool <- is.na( y.values.2) & !is.nan( y.values.2 )
    nan.cutoffs.2.bool <- is.nan( y.values.2)
    bools <- list(na.cutoffs.1.bool, nan.cutoffs.1.bool,
                  na.cutoffs.2.bool, nan.cutoffs.2.bool)
    values <- c(NA,NaN,NA,NaN)

    for (j in 1:4) {
      for (k in which(bools[[j]])) {
        interval.max <- objs.x[[ ceiling(j/2) ]][k]
        interval.min <- -Inf
        if (k < length(objs.x[[ ceiling(j/2) ]])) {
          interval.min <- objs.x[[ ceiling(j/2) ]][k+1]
        }
        objs[[ ceiling(j/2) ]][cutoffs <= interval.max &
                                 cutoffs > interval.min ] <- values[j]
      }
    }

    alpha.values <- c(alpha.values, list(cutoffs))
    x.values <- c(x.values, list(objs[[1]]))
    y.values <- c(y.values, list(objs[[2]]))
  }

  return( new("performance",
              x.name=x.name, y.name=y.name,
              alpha.name=alpha.name, x.values=x.values,
              y.values=y.values, alpha.values=alpha.values))
}

.define.environments <- function() {
  ## There are five environments: long.unit.names, function.names,
  ## obligatory.x.axis, optional.arguments, default.values

  ## Define long names corresponding to the measure abbreviations.
  long.unit.names <- new.env()
  assign("none","None", envir=long.unit.names)
  assign("cutoff", "Cutoff", envir=long.unit.names)
  assign("acc", "Accuracy", envir=long.unit.names)
  assign("err", "Error Rate", envir=long.unit.names)
  assign("fpr", "False positive rate", envir=long.unit.names)
  assign("tpr", "True positive rate", envir=long.unit.names)
  assign("rec", "Recall", envir=long.unit.names)
  assign("sens", "Sensitivity", envir=long.unit.names)
  assign("fnr", "False negative rate", envir=long.unit.names)
  assign("tnr", "True negative rate", envir=long.unit.names)
  assign("spec", "Specificity", envir=long.unit.names)
  assign("ppv", "Positive predictive value", envir=long.unit.names)
  assign("prec", "Precision", envir=long.unit.names)
  assign("npv", "Negative predictive value", envir=long.unit.names)
  assign("fall", "Fallout", envir=long.unit.names)
  assign("miss", "Miss", envir=long.unit.names)
  assign("pcfall", "Prediction-conditioned fallout", envir=long.unit.names)
  assign("pcmiss", "Prediction-conditioned miss", envir=long.unit.names)
  assign("rpp", "Rate of positive predictions", envir=long.unit.names)
  assign("rnp", "Rate of negative predictions", envir=long.unit.names)
  assign("auc","Area under the ROC curve", envir=long.unit.names)
  assign("aucpr","Area under the Precision/Recall curve", envir=long.unit.names)
  assign("cal", "Calibration error", envir=long.unit.names)
  assign("mwp", "Median window position", envir=long.unit.names)
  assign("prbe","Precision/recall break-even point", envir=long.unit.names)
  assign("rch", "ROC convex hull", envir=long.unit.names)
  assign("mxe", "Mean cross-entropy", envir=long.unit.names)
  assign("rmse","Root-mean-square error", envir=long.unit.names)
  assign("phi", "Phi correlation coefficient", envir=long.unit.names)
  assign("mat","Matthews correlation coefficient", envir=long.unit.names)
  assign("mi", "Mutual information", envir=long.unit.names)
  assign("chisq", "Chi-square test statistic", envir=long.unit.names)
  assign("odds","Odds ratio", envir=long.unit.names)
  assign("lift", "Lift value", envir=long.unit.names)
  assign("f","Precision-Recall F measure", envir=long.unit.names)
  assign("sar", "SAR", envir=long.unit.names)
  assign("ecost", "Expected cost", envir=long.unit.names)
  assign("cost", "Explicit cost", envir=long.unit.names)

  ## Define function names corresponding to the measure abbreviations.
  function.names <- new.env()
  assign("acc", ".performance.accuracy", envir=function.names)
  assign("err", ".performance.error.rate", envir=function.names)
  assign("fpr", ".performance.false.positive.rate", envir=function.names)
  assign("tpr", ".performance.true.positive.rate", envir=function.names)
  assign("rec", ".performance.true.positive.rate", envir=function.names)
  assign("sens", ".performance.true.positive.rate", envir=function.names)
  assign("fnr", ".performance.false.negative.rate", envir=function.names)
  assign("tnr", ".performance.true.negative.rate", envir=function.names)
  assign("spec", ".performance.true.negative.rate", envir=function.names)
  assign("ppv", ".performance.positive.predictive.value",
         envir=function.names)
  assign("prec", ".performance.positive.predictive.value",
         envir=function.names)
  assign("npv", ".performance.negative.predictive.value",
         envir=function.names)
  assign("fall", ".performance.false.positive.rate", envir=function.names)
  assign("miss", ".performance.false.negative.rate", envir=function.names)
  assign("pcfall", ".performance.prediction.conditioned.fallout",
         envir=function.names)
  assign("pcmiss", ".performance.prediction.conditioned.miss",
         envir=function.names)
  assign("rpp", ".performance.rate.of.positive.predictions",
         envir=function.names)
  assign("rnp", ".performance.rate.of.negative.predictions",
         envir=function.names)
  assign("auc", ".performance.auc", envir=function.names)
  assign("aucpr", ".performance.aucpr", envir=function.names)
  assign("cal", ".performance.calibration.error", envir=function.names)
  assign("prbe", ".performance.precision.recall.break.even.point",
         envir=function.names)
  assign("rch", ".performance.rocconvexhull", envir=function.names)
  assign("mxe", ".performance.mean.cross.entropy", envir=function.names)
  assign("rmse", ".performance.root.mean.squared.error",
         envir=function.names)
  assign("phi", ".performance.phi", envir=function.names)
  assign("mat", ".performance.phi", envir=function.names)
  assign("mi", ".performance.mutual.information", envir=function.names)
  assign("chisq", ".performance.chisq", envir=function.names)
  assign("odds", ".performance.odds.ratio", envir=function.names)
  assign("lift", ".performance.lift", envir=function.names)
  assign("f", ".performance.f", envir=function.names)
  assign("sar", ".performance.sar", envir=function.names)
  assign("ecost", ".performance.expected.cost", envir=function.names)
  assign("cost", ".performance.cost", envir=function.names)

  ## If a measure comes along with an obligatory x axis (including "none"),
  ## list it here.
  obligatory.x.axis <- new.env()
  assign("mxe", "none", envir=obligatory.x.axis)
  assign("rmse", "none", envir=obligatory.x.axis)
  assign("prbe", "none", envir=obligatory.x.axis)
  assign("auc", "none", envir=obligatory.x.axis)
  assign("aucpr", "none", envir=obligatory.x.axis)
  assign("rch","none", envir=obligatory.x.axis)
  ## ecost requires probability cost function as x axis, which is handled
  ## implicitly, not as an explicit performance measure.
  assign("ecost","none", envir=obligatory.x.axis)

  ## If a measure has optional arguments, list the names of the
  ## arguments here.
  optional.arguments <- new.env()
  assign("cal", "window.size", envir=optional.arguments)
  assign("f", "alpha", envir=optional.arguments)
  assign("cost", c("cost.fp", "cost.fn"), envir=optional.arguments)
  assign("auc", "fpr.stop", envir=optional.arguments)

  ## If a measure has additional arguments, list the default values
  ## for them here. Naming convention: e.g. "cal" has an optional
  ## argument "window.size" the key to use here is "cal:window.size"
  ## (colon as separator)
  default.values <- new.env()
  assign("cal:window.size", 100, envir=default.values)
  assign("f:alpha", 0.5, envir=default.values)
  assign("cost:cost.fp", 1, envir=default.values)
  assign("cost:cost.fn", 1, envir=default.values)
  assign("auc:fpr.stop", 1, envir=default.values)

  list(long.unit.names=long.unit.names, function.names=function.names,
       obligatory.x.axis=obligatory.x.axis,
       optional.arguments=optional.arguments,
       default.values=default.values)
}
