# ROCR <img src="https://raw.githubusercontent.com/ipa-tys/ROCR/rocr-images/ROCR_small.png" align="right">

*visualizing classifier performance in R, with only 3 commands*

<!-- badges: start -->
[![R-CMD-check](https://github.com/ipa-tys/ROCR/workflows/R-CMD-check/badge.svg)](https://github.com/ipa-tys/ROCR/actions?query=workflow:R-CMD-check)
[![CRAN Status](https://www.r-pkg.org/badges/version/ROCR)](https://CRAN.r-project.org/package=ROCR)
[![codecov](https://codecov.io/gh/ipa-tys/ROCR/branch/master/graph/badge.svg)](https://codecov.io/gh/ipa-tys/ROCR)
<!-- badges: end -->

![](https://raw.githubusercontent.com/ipa-tys/ROCR/rocr-images/ourplot_website.gif)

### Please support our work by citing the ROCR article in your publications:

***Sing T, Sander O, Beerenwinkel N, Lengauer T. [2005]
ROCR: visualizing classifier performance in R.
Bioinformatics 21(20):3940-1.***

Free full text:
http://bioinformatics.oxfordjournals.org/content/21/20/3940.full

[<img src="https://raw.githubusercontent.com/ipa-tys/ROCR/rocr-images/logo_mpi_430.png" align="right" width="300">](https://www.mpi-inf.mpg.de/home/)  
ROCR was originally developed at the [Max Planck Institute for Informatics](https://www.mpi-inf.mpg.de/home/)   


## Introduction

`ROCR` (with obvious pronounciation) is an R package for evaluating and visualizing classifier performance. It is...

 - ...easy to use: adds only three new commands to R.
 - ...flexible: integrates tightly with R's built-in graphics facilities.
 - ...powerful: Currently, 28 performance measures are implemented, which can be freely combined to form parametric curves such as ROC curves, precision/recall curves, or lift curves. Many options such as curve averaging (for cross-validation or bootstrap), augmenting the averaged curves by standard error bar or boxplots, labeling cutoffs to the curve, or coloring curves according to cutoff.

### Performance measures that `ROCR` knows:

Accuracy, error rate, true positive rate, false positive rate, true negative rate, false negative rate, sensitivity, specificity, recall, positive predictive value, negative predictive value, precision, fallout, miss, phi correlation coefficient, Matthews correlation coefficient, mutual information, chi square statistic, odds ratio, lift value, precision/recall F measure, ROC convex hull, area under the ROC curve, precision/recall break-even point, calibration error, mean cross-entropy, root mean squared error, SAR measure, expected cost, explicit cost.

### `ROCR` features:

ROC curves, precision/recall plots, lift charts, cost curves, custom curves by freely selecting one performance measure for the x axis and one for the y axis, handling of data from cross-validation or bootstrapping, curve averaging (vertically, horizontally, or by threshold), standard error bars, box plots, curves that are color-coded by cutoff, printing threshold values on the curve, tight integration with Rs plotting facilities (making it easy to adjust plots or to combine multiple plots), fully customizable, easy to use (only 3 commands).

## Installation of `ROCR`

The most straightforward way to install and use `ROCR` is to install it from 
`CRAN` by starting `R` and using the `install.packages` function:

```
install.packages("ROCR")
```

Alternatively you can install it from command line using the tar ball like this:

```
R CMD INSTALL ROCR_*.tar.gz
```

## Getting started

from withing R ...

```
library(ROCR)
demo(ROCR)
help(package=ROCR)
```

## Examples

Using ROCR's 3 commands to produce a simple ROC plot:

```
pred <- prediction(predictions, labels)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col=rainbow(10))
```

## Documentation

 - The Reference Manual found [here](https://CRAN.r-project.org/package=ROCR)
 - Slide deck for a tutorial talk (feel free to re-use for teaching, but please give appropriate credits and write us an email) [[PPT](https://raw.githubusercontent.com/ipa-tys/ROCR/rocr-images/ROCR_Talk_Tobias_Sing.ppt)]
 - A few pointers to the literature on classifier evaluation


## Contact
Questions, comments, and suggestions are very welcome. Open an issue on GitHub and we can discuss. We are also interested in seeing how ROCR is used in publications. Thus, if you have prepared a paper using ROCR we'd be happy to know.
