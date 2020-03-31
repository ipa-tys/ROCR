#' @import methods
NULL

#' @name prediction-class
#' @aliases prediction-class
#'
#' @title Class \code{prediction}
#'
#' @description
#' Object to encapsulate numerical predictions together with the
#' corresponding true class labels, optionally collecting predictions and
#' labels for several cross-validation or bootstrapping runs.
#'
#' @section Objects from the Class:
#' Objects can be created by using the \code{prediction} function.
#'
#' @note
#' Every \code{prediction} object contains information about the 2x2
#' contingency table consisting of tp,tn,fp, and fn, along with the
#' marginal sums n.pos,n.neg,n.pos.pred,n.neg.pred, because these form
#' the basis for many derived performance measures.
#'
#' @slot predictions A list, in which each element is a vector of predictions
#'   (the list has length > 1 for x-validation data.
#' @slot labels Analogously, a list in which each element is a vector of true
#'   class labels.
#' @slot cutoffs A list in which each element is a vector of all necessary
#'   cutoffs. Each cutoff vector consists of the predicted scores (duplicates
#'   removed), in descending order.
#' @slot fp A list in which each element is a vector of the number (not the
#'   rate!) of false positives induced by the cutoffs given in the corresponding
#'   'cutoffs' list entry.
#' @slot tp As fp, but for true positives.
#' @slot tn As fp, but for true negatives.
#' @slot fn As fp, but for false negatives.
#' @slot n.pos A list in which each element contains the number of positive
#'   samples in the given x-validation run.
#' @slot n.neg As n.pos, but for negative samples.
#' @slot n.pos.pred A list in which each element is a vector of the number of
#'   samples predicted as positive at the cutoffs given in the corresponding
#'   'cutoffs' entry.
#' @slot n.neg.pred As n.pos.pred, but for negatively predicted samples.
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
#' \code{\link{performance}},
#' \code{\link{performance-class}},
#' \code{\link{plot.performance}}
#'
#' @export
setClass("prediction",
         representation(predictions = "list",
                        labels      = "list",
                        cutoffs     = "list",
                        fp          = "list",
                        tp          = "list",
                        tn          = "list",
                        fn          = "list",
                        n.pos       = "list",
                        n.neg       = "list",
                        n.pos.pred  = "list",
                        n.neg.pred  = "list"))

setMethod("show","prediction",
          function(object){
              cat("A ", class(object), " instance\n", sep = "")
              if(length(object@predictions) > 1L){
                  cat("  with ", length(object@predictions)," cross ",
                      "validation runs ", sep = "")
                  if(length(unique(vapply(object@predictions,length,integer(1))))){
                      cat("(equal lengths)", sep = "")
                  } else {
                      cat("(different lengths)", sep = "")
                  }
              } else {
                  cat("  with ", length(object@predictions[[1L]]),
                      " data points", sep = "")
              }
          })

#' @name performance-class
#' @aliases performance-class
#'
#' @title Class \code{performance}
#'
#' @description
#' Object to capture the result of a performance evaluation, optionally
#' collecting evaluations from several cross-validation or bootstrapping runs.
#'
#' @section Objects from the Class:
#' Objects can be created by using the \code{performance} function.
#'
#' @details
#' A \code{performance} object can capture information from four
#' different evaluation scenarios:
#'   \itemize{
#'     \item The behaviour of a cutoff-dependent performance measure across
#'     the range of all cutoffs (e.g. \code{performance( predObj, 'acc' )} ). Here,
#'     \code{x.values} contains the cutoffs, \code{y.values} the
#'     corresponding values of the performance measure, and
#'     \code{alpha.values} is empty.\cr
#'     \item The trade-off between two performance measures across the
#'     range of all cutoffs (e.g. \code{performance( predObj,
#'                                                   'tpr', 'fpr' )} ). In this case, the cutoffs are stored in
#'     \code{alpha.values}, while \code{x.values} and \code{y.values}
#'     contain the corresponding values of the two performance measures.\cr
#'     \item A performance measure that comes along with an obligatory
#'     second axis (e.g. \code{performance( predObj, 'ecost' )} ). Here, the measure values are
#'     stored in \code{y.values}, while the corresponding values of the
#'     obligatory axis are stored in \code{x.values}, and \code{alpha.values}
#'     is empty.\cr
#'     \item A performance measure whose value is just a scalar
#'     (e.g. \code{performance( predObj, 'auc' )} ). The value is then stored in
#'     \code{y.values}, while \code{x.values} and \code{alpha.values} are
#'     empty.
#'   }
#'
#' @slot x.name Performance measure used for the x axis.
#' @slot y.name Performance measure used for the y axis.
#' @slot alpha.name Name of the unit that is used to create the parametrized
#'   curve. Currently, curves can only be parametrized by cutoff, so
#'   \code{alpha.name} is either \code{none} or \code{cutoff}.
#' @slot x.values A list in which each entry contains the x values of the curve
#'   of this particular cross-validation run. \code{x.values[[i]]},
#'   \code{y.values[[i]]}, and \code{alpha.values[[i]]} correspond to each
#'   other.
#' @slot y.values A list in which each entry contains the y values of the curve
#'   of this particular cross-validation run.
#' @slot alpha.values A list in which each entry contains the cutoff values of
#'   the curve of this particular cross-validation run.
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
#' \code{\link{prediction}}
#' \code{\link{performance}},
#' \code{\link{prediction-class}},
#' \code{\link{plot.performance}}
#'
#' @export
setClass("performance",
         representation(x.name       = "character",
                        y.name       = "character",
                        alpha.name   = "character",
                        x.values     = "list",
                        y.values     = "list",
                        alpha.values = "list" ))

setMethod("show","performance",
          function(object){
              cat("A ", class(object), " instance\n", sep = "")
              if(length(object@y.values[[1L]]) > 1L){
                  cat("  '", object@x.name, "' vs. '", object@y.name,
                      "' (alpha: '",object@alpha.name,"')\n", sep = "")
              } else {
                  cat("  '", object@y.name, "'\n", sep = "")
              }
              if(length(object@y.values) > 1L){
                  cat("  for ", length(object@y.values)," cross ",
                      "validation runs ", sep = "")
              } else {
                  if(length(object@y.values[[1L]]) > 1L){
                      cat("  with ", length(object@y.values[[1L]])," data points",
                          sep = "")
                  }
              }
          })

#' @name plot-methods
#' @aliases plot,performance,missing-method plot.performance
#'
#' @title Plot method for performance objects
#'
#' @description
#' This is the method to plot all objects of class performance.
#'
#' @param x an object of class \code{performance}
#' @param y not used
#' @param ... Optional graphical parameters to adjust different components of
#'   the performance plot. Parameters are directed to their target component by
#'   prefixing them with the name of the component (\code{component.parameter},
#'   e.g. \code{text.cex}). The following components are available:
#'   \code{xaxis}, \code{yaxis}, \code{coloraxis}, \code{box} (around the
#'   plotting region), \code{points}, \code{text}, \code{plotCI} (error bars),
#'   \code{boxplot}. The names of these components are influenced by the R
#'   functions that are used to create them. Thus, \code{par(component)} can be
#'   used to see which parameters are available for a given component (with the
#'   expection of the three axes; use \code{par(axis)} here). To adjust the
#'   canvas or the performance curve(s), the standard \code{plot} parameters can
#'   be used without any prefix.
#' @param avg If the performance object describes several curves (from
#'   cross-validation runs or bootstrap evaluations of one particular method),
#'   the curves from each of the runs can be averaged. Allowed values are
#'   \code{none} (plot all curves separately), \code{horizontal} (horizontal
#'   averaging), \code{vertical} (vertical averaging), and \code{threshold}
#'   (threshold (=cutoff) averaging). Note that while threshold averaging is
#'   always feasible, vertical and horizontal averaging are not well-defined if
#'   the graph cannot be represented as a function x->y and y->x, respectively.
#' @param spread.estimate When curve averaging is enabled, the variation around
#'   the average curve can be visualized as standard error bars
#'   (\code{stderror}), standard deviation bars (\code{stddev}), or by using box
#'   plots (\code{boxplot}). Note that the function \code{plotCI}, which is used
#'   internally by ROCR to draw error bars, might raise a warning if the spread
#'   of the curves at certain positions is 0.
#' @param spread.scale For \code{stderror} or \code{stddev}, this is a scalar
#'   factor to be multiplied with the length of the standard error/deviation
#'   bar. For example, under normal assumptions, \code{spread.scale=2} can be
#'   used to get approximate 95\% confidence intervals.
#' @param show.spread.at For vertical averaging, this vector determines the x
#'   positions for which the spread estimates should be visualized. In contrast,
#'   for horizontal and threshold averaging, the y positions and cutoffs are
#'   determined, respectively. By default, spread estimates are shown at 11
#'   equally spaced positions.
#' @param colorize This logical determines whether the curve(s) should be
#'   colorized according to cutoff.
#' @param colorize.palette If curve colorizing is enabled, this determines the
#'   color palette onto which the cutoff range is mapped.
#' @param colorkey If true, a color key is drawn into the 4\% border
#'   region (default of \code{par(xaxs)} and \code{par(yaxs)}) of the
#'   plot. The color key visualizes the mapping from cutoffs to colors.
#' @param colorkey.relwidth Scalar between 0 and 1 that determines the
#'   fraction of the 4\% border region that is occupied by the colorkey.
#' @param colorkey.pos Determines if the colorkey is drawn vertically at
#'   the \code{right} side, or horizontally at the \code{top} of the
#'   plot.
#' @param print.cutoffs.at This vector specifies the cutoffs which should
#'   be printed as text along the curve at the corresponding curve positions.
#' @param cutoff.label.function By default, cutoff annotations along the curve
#'   or at the color key are rounded to two decimal places before printing.
#'   Using a custom \code{cutoff.label.function}, any other transformation can
#'   be performed on the cutoffs instead (e.g. rounding with different precision
#'   or taking the logarithm).
#' @param downsampling ROCR can efficiently compute most performance measures
#'   even for data sets with millions of elements. However, plotting of large
#'   data sets can be slow and lead to PS/PDF documents of considerable size. In
#'   that case, performance curves that are indistinguishable from the original
#'   can be obtained by using only a fraction of the computed performance
#'   values. Values for downsampling between 0 and 1 indicate the fraction of
#'   the original data set size to which the performance object should be
#'   downsampled, integers above 1 are interpreted as the actual number of
#'   performance values to which the curve(s) should be downsampled.
#' @param add If \code{TRUE}, the curve(s) is/are added to an already existing
#'   plot; otherwise a new plot is drawn.
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
#' \code{\link{performance}},
#' \code{\link{prediction-class}},
#' \code{\link{performance-class}}
#'
#' @export
#'
#' @examples
#' # plotting a ROC curve:
#' library(ROCR)
#' data(ROCR.simple)
#' pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
#' pred
#' perf <- performance( pred, "tpr", "fpr" )
#' perf
#' plot( perf )
#'
#' # To entertain your children, make your plots nicer
#' # using ROCR's flexible parameter passing mechanisms
#' # (much cheaper than a finger painting set)
#' par(bg="lightblue", mai=c(1.2,1.5,1,1))
#' plot(perf, main="ROCR fingerpainting toolkit", colorize=TRUE,
#'      xlab="Mary's axis", ylab="", box.lty=7, box.lwd=5,
#'      box.col="gold", lwd=17, colorkey.relwidth=0.5, xaxis.cex.axis=2,
#'      xaxis.col='blue', xaxis.col.axis="blue", yaxis.col='green', yaxis.cex.axis=2,
#'      yaxis.at=c(0,0.5,0.8,0.85,0.9,1), yaxis.las=1, xaxis.lwd=2, yaxis.lwd=3,
#'      yaxis.col.axis="orange", cex.lab=2, cex.main=2)
setMethod("plot",
          signature(x="performance",y="missing"),
          function(x,
                   y,
                   ...,
                   avg = "none",
                   spread.estimate = "none",
                   spread.scale = 1,
                   show.spread.at = c(),
                   colorize = FALSE,
                   colorize.palette = rev(rainbow(256, start = 0, end = 4 / 6)),
                   colorkey = colorize,
                   colorkey.relwidth = 0.25,
                   colorkey.pos = "right",
                   print.cutoffs.at = c(),
                   cutoff.label.function = function(x) {
                     round(x, 2)
                   },
                   downsampling = 0,
                   add = FALSE ) {

            .plot.performance(x,...,
                              avg = avg,
                              spread.estimate = spread.estimate,
                              spread.scale = spread.scale,
                              show.spread.at = show.spread.at,
                              colorize = colorize,
                              colorize.palette = colorize.palette,
                              colorkey = colorkey,
                              colorkey.relwidth = colorkey.relwidth,
                              colorkey.pos = colorkey.pos,
                              print.cutoffs.at = print.cutoffs.at,
                              cutoff.label.function = cutoff.label.function,
                              downsampling = downsampling,
                              add = add)
          }
)

#' @rdname plot-methods
#' @method plot performance
#' @export
"plot.performance" <- function(...) plot(...)

#' @name ROCR.hiv
#'
#' @docType data
#' @keywords datasets
#'
#' @title Data set: Support vector machines and neural networks applied to the
#'   prediction of HIV-1 coreceptor usage
#'
#' @description
#' Linear support vector machines (libsvm) and neural networks (R package
#' nnet) were applied to predict usage of the coreceptors CCR5 and CXCR4
#' based on sequence data of the third variable loop of the HIV envelope
#' protein.
#'
#' @format
#' A list consisting of the SVM (\code{ROCR.hiv$hiv.svm}) and NN
#' (\code{ROCR.hiv$hiv.nn}) classification data. Each of those is in turn a list
#' consisting of the two elements \code{$predictions} and \code{$labels} (10
#' element list representing cross-validation data).
#'
#' @references
#' Sing, T. & Beerenwinkel, N. & Lengauer, T.  "Learning mixtures
#' of localized rules by maximizing the area under the ROC curve".  1st
#'   International Workshop on ROC Analysis in AI, 89-96, 2004.
#'
#' @usage data(ROCR.hiv)
#'
#' @examples
#' library(ROCR)
#' data(ROCR.hiv)
#' attach(ROCR.hiv)
#' pred.svm <- prediction(hiv.svm$predictions, hiv.svm$labels)
#' pred.svm
#' perf.svm <- performance(pred.svm, 'tpr', 'fpr')
#' perf.svm
#' pred.nn <- prediction(hiv.nn$predictions, hiv.svm$labels)
#' pred.nn
#' perf.nn <- performance(pred.nn, 'tpr', 'fpr')
#' perf.nn
#' plot(perf.svm, lty=3, col="red",main="SVMs and NNs for prediction of
#' HIV-1 coreceptor usage")
#' plot(perf.nn, lty=3, col="blue",add=TRUE)
#' plot(perf.svm, avg="vertical", lwd=3, col="red",
#'      spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
#' plot(perf.nn, avg="vertical", lwd=3, col="blue",
#'      spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
#' legend(0.6,0.6,c('SVM','NN'),col=c('red','blue'),lwd=3)
"ROCR.hiv"


#' @name ROCR.simple
#'
#' @docType data
#' @keywords datasets
#'
#' @title Data set: Simple artificial prediction data for use with ROCR
#'
#' @description
#' A mock data set containing a simple set of predictions and corresponding
#' class labels.
#'
#' @format
#' A two element list. The first element, \code{ROCR.simple$predictions}, is a
#' vector of numerical predictions. The second element,
#' \code{ROCR.simple$labels}, is a vector of corresponding class labels.
#'
#' @usage data(ROCR.simple)
#'
#' @examples
#' # plot a ROC curve for a single prediction run
#' # and color the curve according to cutoff.
#' library(ROCR)
#' data(ROCR.simple)
#' pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
#' pred
#' perf <- performance(pred,"tpr","fpr")
#' perf
#' plot(perf,colorize=TRUE)
"ROCR.simple"


#' @name ROCR.xval
#'
#' @docType data
#' @keywords datasets
#'
#' @title Data set: Artificial cross-validation data for use with ROCR
#'
#' @description
#' A mock data set containing 10 sets of predictions and corresponding labels as
#' would be obtained from 10-fold cross-validation.
#'
#' @format
#' A two element list. The first element, \code{ROCR.xval$predictions}, is
#' itself a 10 element list. Each of these 10 elements is a vector of numerical
#' predictions for each cross-validation run. Likewise, the second list entry,
#' \code{ROCR.xval$labels} is a 10 element list in which each element is a
#' vector of true class labels corresponding to the predictions.
#'
#' @usage data(ROCR.xval)
#'
#' @examples
#' # plot ROC curves for several cross-validation runs (dotted
#' # in grey), overlaid by the vertical average curve and boxplots
#' # showing the vertical spread around the average.
#' library(ROCR)
#' data(ROCR.xval)
#' pred <- prediction(ROCR.xval$predictions, ROCR.xval$labels)
#' pred
#' perf <- performance(pred,"tpr","fpr")
#' perf
#' plot(perf,col="grey82",lty=3)
#' plot(perf,lwd=3,avg="vertical",spread.estimate="boxplot",add=TRUE)
"ROCR.xval"
