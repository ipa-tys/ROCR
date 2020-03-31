context("simple")
test_that("simple:",{
  some.predictions <- c(0.02495517, 0.92535646,
                        0.86251887, 0.80946685,
                        0.70922858, 0.69762824,
                        0.50604485, 0.25446810,
                        0.10837728, 0.07250349)
  some.labels <- c(0,1,1,0,1,1,0,1,0,0)
  
  tp.reference <- c(0, 1, 2, 2, 3, 4, 4, 5, 5, 5, 5)
  fp.reference <- c(0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5)
  
  pp.reference <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  np.reference <- c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
  p.reference <- rep(5, 11)
  n.reference <- rep(5, 11)
  
  tn.reference <- n.reference-fp.reference
  fn.reference <- p.reference-tp.reference
  
  # manually calculated reference measures
  rpp.reference <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
  rnp.reference <- c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0)
  
  tpr.reference <- c(0.0, 0.2, 0.4, 0.4, 0.6, 0.8, 0.8, 1.0, 1.0, 1.0, 1.0)
  fpr.reference <- c(0.0, 0.0, 0.0, 0.2, 0.2, 0.2, 0.4, 0.4, 0.6, 0.8, 1.0)
  acc.reference <- c(0.5, 0.6, 0.7, 0.6, 0.7, 0.8, 0.7, 0.8, 0.7, 0.6, 0.5)
  err.reference <- c(0.5, 0.4, 0.3, 0.4, 0.3, 0.2, 0.3, 0.2, 0.3, 0.4, 0.5)
  
  rec.reference <- tpr.reference
  sens.reference<- tpr.reference
  fnr.reference <- c(1.0, 0.8, 0.6, 0.6, 0.4, 0.2, 0.2, 0.0, 0.0, 0.0, 0.0) 
  tnr.reference <- c(1.0, 1.0, 1.0, 0.8, 0.8, 0.8, 0.6, 0.6, 0.4, 0.2, 0.0)
  spec.reference<- tnr.reference
  
  ppv.reference <- c(0/0, 1/1, 2/2, 2/3, 3/4, 4/5, 4/6, 5/7, 5/8, 5/9, 5/10)
  npv.reference <- c(5/10, 5/9, 5/8, 4/7, 4/6, 4/5, 3/4, 3/3, 2/2, 1/1, 0/0)
  prec.reference<- ppv.reference
  
  fall.reference <- fpr.reference
  miss.reference <- fnr.reference
  pcfall.reference <- c(0/0, 0/1, 0/2, 1/3, 1/4, 1/5, 2/6, 2/7, 3/8, 4/9, 5/10)
  pcmiss.reference <- c(5/10, 4/9, 3/8, 3/7, 2/6, 1/5, 1/4, 0/3, 0/2, 0/1, 0/0)
  
  auc.reference <- 0.84
  aucpr.reference <- 0.8814286
  
  cal.reference <- c()
  ind <- rev(order(some.predictions))
  sorted.predictions <- some.predictions[ind]
  sorted.labels <- some.labels[ind]
  for (i in 1:8) {
    mean.pred <- mean( sorted.predictions[i:(i+2)] )
    frac.pos <- sum( sorted.labels[i:(i+2)] ) / 3
    cal.reference <- c(cal.reference, abs( mean.pred - frac.pos ))
  }
  
  prbe.reference<- 0.8
  prbe.reference.x <- 0.69762824
  rch.reference.x <- fpr.reference[c(1,3,6,8,11)]
  rch.reference.y <- tpr.reference[c(1,3,6,8,11)]
  mxe.reference <- -(1/length(some.predictions)) * sum(some.labels*log(some.predictions) +
                                                         (1-some.labels)*log(1-some.predictions))
  rmse.reference <- sqrt((1/length(some.predictions)) * sum((some.predictions-some.labels)^2))
  
  phi.reference <- (tp.reference*tn.reference-fp.reference*fn.reference) /
    sqrt(p.reference*n.reference*pp.reference*np.reference)
  mat.reference <- phi.reference
  
  my.log2 <- function( x ) {
    ans <- log2(x)
    ans[ ans==-Inf ] <- 0
    ans
  }
  
  mi.reference <- (tn.reference * my.log2( tn.reference / (n.reference*np.reference)) +
                     fn.reference*my.log2(fn.reference/(np.reference*p.reference)) +
                     fp.reference*my.log2(fp.reference/(n.reference*pp.reference)) +
                     tp.reference*my.log2(tp.reference/(p.reference*pp.reference))) / length(some.labels) + log2(length(some.labels))
  
  chisq.reference <-
    (((pp.reference*p.reference/length(some.predictions)) -
        tp.reference)^2 / (pp.reference*p.reference/length(some.predictions))
     + ((pp.reference*n.reference/length(some.predictions)) -
          fp.reference)^2 / (pp.reference*n.reference/length(some.predictions))
     + ((np.reference*p.reference/length(some.predictions)) -
          fn.reference)^2 / (np.reference*p.reference/length(some.predictions))
     + ((np.reference*n.reference/length(some.predictions)) -
          tn.reference)^2 / (np.reference*n.reference/length(some.predictions)))
  
  odds.reference <- (tp.reference*tn.reference) / (fn.reference*fp.reference)
  
  lift.reference <- (tp.reference/p.reference) / (pp.reference/(p.reference+n.reference))
  
  f.reference <- 1 / (0.5 * ((1/prec.reference) + (1/rec.reference)))
  
  sar.reference <- 1/3 * (acc.reference + auc.reference + (1-rmse.reference))
  cost.reference <- (fpr.reference * n.reference/length(some.labels) * 1 +
                       fnr.reference * p.reference/length(some.labels) * 1)
  
  .get.performance.measures <- function(pred) {
    .get.performance.measure.result <- function(pred, measure){
      perf <- performance(pred, measure)
      show(perf)
      perf@y.values[[1]]
    }
    tpr <- .get.performance.measure.result(pred, "tpr")
    fpr <- .get.performance.measure.result(pred, "fpr")
    acc <- .get.performance.measure.result(pred, "acc")
    err <- .get.performance.measure.result(pred, "err")
    
    rec <- .get.performance.measure.result(pred, "rec")
    sens<- .get.performance.measure.result(pred, "sens")
    fnr <- .get.performance.measure.result(pred, "fnr")
    tnr <- .get.performance.measure.result(pred, "tnr")
    spec<- .get.performance.measure.result(pred, "spec")
    ppv <- .get.performance.measure.result(pred, "ppv")
    prec<- .get.performance.measure.result(pred, "prec")
    npv <- .get.performance.measure.result(pred, "npv")
    
    fall<- .get.performance.measure.result(pred, "fall")
    miss<- .get.performance.measure.result(pred, "miss")
    pcfall <- .get.performance.measure.result(pred, "pcfall")
    pcmiss <- .get.performance.measure.result(pred, "pcmiss")
    rpp <- .get.performance.measure.result(pred, "rpp")
    rnp <- .get.performance.measure.result(pred, "rnp")
    
    auc <- performance(pred, "auc")@y.values[[1]]
    aucpr <- performance(pred, "aucpr")@y.values[[1]]
    prbe<- performance(pred, "prbe")@y.values[[1]]
    rch <- performance(pred, "rch")@y.values[[1]]

    mxe <- .get.performance.measure.result(pred, "mxe")
    rmse<- .get.performance.measure.result(pred, "rmse")
    
    phi <- .get.performance.measure.result(pred, "phi")
    mat <- .get.performance.measure.result(pred, "mat")
    mi  <- .get.performance.measure.result(pred, "mi")
    chisq<- .get.performance.measure.result(pred, "chisq")
    odds<- .get.performance.measure.result(pred, "odds")
    lift<- .get.performance.measure.result(pred, "lift")
    f   <- .get.performance.measure.result(pred, "f")
    sar <- .get.performance.measure.result(pred,"sar")
    ecost  <- .get.performance.measure.result(pred, "ecost")
    cost  <- .get.performance.measure.result(pred, "cost")
    return(list(tpr=tpr, fpr=fpr, acc=acc, err=err,
                rec=rec, sens=sens, fnr=fnr, tnr=tnr,
                spec=spec, ppv=ppv, prec=prec, npv=npv, 
                fall=fall, miss=miss, pcfall=pcfall, pcmiss=pcmiss, rpp=rpp, rnp=rnp,
                auc=auc, aucpr=aucpr, prbe=prbe, rch=rch, mxe=mxe, 
                rmse=rmse, phi=phi, mat=mat, mi=mi, chisq=chisq, odds=odds,
                lift=lift, f=f, sar=sar, ecost=ecost, cost=cost))
    
  }
  
  ##############################################################################
  # test PerformanceMeasuresReference
  expect_error(prediction(some.predictions[-1], some.labels),
               "Number of predictions in each run must be equal")
  expect_error(prediction(c(NA,some.predictions[-1]), some.labels),
               "'predictions' contains NA.")
  expect_error(prediction(as.list(matrix(some.predictions)), some.labels),
               "Number of cross-validation runs must be equal")
  expect_error(prediction(some.predictions, factor(some.labels,ordered = TRUE),
                          label.ordering = c(1,0)),
               "'labels' is already ordered. No additional 'label.ordering'")
  expect_error()
  
  pred <- prediction(some.predictions, some.labels)
  expect_output(show(pred))
  actual <- prediction(some.predictions, factor(some.labels),
                       label.ordering = c(0,1))
  expect_equal(pred, actual)
  expect_error(performance("tpr",pred),
               "Wrong argument types")
  expect_error(performance(pred,"tpr","mxe"),
               "The performance measure mxe can only be used as 'measure'")
  actual1 <- performance(pred, "tpr")
  actual2 <- performance(pred, "fpr")
  actual <- ROCR:::.combine.performance.objects(actual1,actual2)
  expect_s4_class(actual,"performance")
  actual3 <- performance(pred, "mxe")
  expect_error(ROCR:::.combine.performance.objects(actual1,actual3),
               "Objects need to have identical x axis")
  expect_error(ROCR:::.combine.performance.objects(actual,actual),
               "At least one of the two objects has already been merged")
  
  measures <- expect_output(
    expect_warning(.get.performance.measures(pred),
                   "Chi-squared approximation may be incorrect"))
  attach(measures)
  
  expect_equal(tpr, tpr.reference)
  expect_equal(fpr, fpr.reference)
  expect_equal(acc, acc.reference)    
  expect_equal(err, err.reference)
  expect_equal(rec, rec.reference)
  expect_equal(sens, sens.reference)
  expect_equal(fnr, fnr.reference)
  expect_equal(tnr, tnr.reference)
  expect_equal(spec, spec.reference)
  
  expect_equal(ppv, ppv.reference)
  expect_equal(prec,prec.reference)
  expect_equal(npv, npv.reference)
  
  expect_equal(fall, fall.reference)
  expect_equal(miss,miss.reference)
  expect_equal(pcfall, pcfall.reference)
  expect_equal(pcmiss,pcmiss.reference)
  expect_equal(rpp, rpp.reference)
  expect_equal(rnp,rnp.reference)
  
  expect_equal(auc, auc.reference)
  expect_equal(aucpr, aucpr.reference, tolerance = .0000001)
  expect_equal(prbe, prbe.reference)
  
  expect_equal(mxe, mxe.reference)
  
  expect_equal(rmse, rmse.reference)
  expect_equal(phi, phi.reference)
  expect_equal(mat, mat.reference)
  
  expect_equal(mi, mi.reference)
  
  expect_equal(unname(chisq), chisq.reference)
  expect_equal(odds, odds.reference)
  expect_equal(lift, lift.reference)
  
  expect_equal(f, f.reference)
  expect_equal(sar,sar.reference)
  
  expect_equal(cost, cost.reference)
  
  ##############################################################################
  # ecost
  ecost.x.reference <- c(0,1/3,0.5,1)
  ecost.y.reference <- c(0,0.2,0.2,0)
  
  pred <- prediction(some.predictions, some.labels)
  perf <- performance(pred, "ecost")
  ecost.x <- perf@x.values[[1]]
  ecost.y <- perf@y.values[[1]]
  
  expect_equal( ecost.x, ecost.x.reference )
  
  expect_equal( ecost.y, ecost.y.reference )
  
  ##############################################################################
  # test cal
  pred <- prediction(some.predictions, some.labels)
  cal <- performance(pred, "cal", window.size=floor(length(pred@predictions[[1]])/3))@y.values[[1]]
  cal.x <- performance(pred, "cal", window.size=floor(length(pred@predictions[[1]])/3))@x.values[[1]]
  cal.x.reference <- rev(sort( some.predictions ))[2:(length(some.predictions)-1)]
  
  expect_equal( cal, cal.reference)
  expect_equal( cal.x, cal.x.reference)
  
  ##############################################################################
  # test cost
  pred <- prediction(some.predictions, some.labels)
  for (cost.fp in rnorm(50)) {
    cost.fn <- rnorm(1)
    
    perf <- performance(pred, "cost", cost.fp=cost.fp, cost.fn=cost.fn)
    cost <- perf@y.values[[1]]
    my.cost.reference <- (fpr.reference * n.reference/length(some.labels) * cost.fp +
                            fnr.reference * p.reference/length(some.labels) * cost.fn)
    
    expect_equal( cost, my.cost.reference)
  }
  
  ##############################################################################
  # test Rch
  pred <- prediction(some.predictions, some.labels)
  perf <- performance( pred, "rch")
  rch.x <- perf@x.values[[1]]
  rch.y <- perf@y.values[[1]]
  
  expect_equal( rch.x, rch.reference.x )
  expect_equal( rch.y, rch.reference.y )
  
  ##############################################################################
  # test RMSE
  pred <- prediction(c(0, 0, 1, 1),
                     ordered(c(0, 0, 1, 1)))
  rmse <- performance(pred, "rmse")@y.values[[1]]
  expect_equal(rmse, 0)
  
  pred <- prediction(c(0.0, 0.0, 1.0, 1.0),
                     ordered(c(1, 1, 0, 0), levels=c(1,0)))
  rmse <- performance(pred, "rmse")@y.values[[1]]
  
  expect_equal(rmse, 1)
  
  pred <- prediction(c(0.0, 0.0, 1.0, 1.0),
                     ordered(c(2, 2, 3, 3)))
  rmse <- performance(pred, "rmse")@y.values[[1]]
  
  expect_equal( rmse, 2)
  
  pred <- prediction(c(-0.5, 0.2, 2.5, 0.3),
                     ordered(c(-1, -1, 1, 1)))
  rmse <- performance(pred, "rmse")@y.values[[1]]
  
  expect_equal( rmse, sqrt(1/4*(0.5^2 + 1.2^2 + 1.5^2 + 0.7^2)))
  
  ##############################################################################
  # test PRBE
  pred <- prediction(some.predictions, some.labels)
  prbe.y <- performance(pred, "prbe")@y.values[[1]]
  prbe.x <- performance(pred, "prbe")@x.values[[1]]
  expect_equal(prbe.y, prbe.reference)
  expect_equal(prbe.x, prbe.reference.x)
  
  ##############################################################################
  # test prediction interface
  pred <- prediction(seq(0, 1, length=10),
                     c(rep(0,5), rep(1,5)))
  expect_equal(performance(pred, "auc")@y.values[[1]],
              1)    
  pred <- prediction(seq(1, 0, length=10),
                     c(rep(0,5), rep(1,5)))
  expect_equal(performance(pred, "auc")@y.values[[1]],
              0)    
  pred <- prediction(seq(0, 1, length=10),
                     factor(c(rep(0,5), rep(1,5))))
  expect_equal(performance(pred, "auc")@y.values[[1]],
              1)    
  pred <- prediction(seq(0, 1, length=10),
                     ordered(c(rep(0,5), rep(1,5))))
  expect_equal(performance(pred, "auc")@y.values[[1]],
              1)
  pred <- prediction(seq(0, 1, length=10),
                     ordered(c(rep(0,5), rep(1,5)), levels=c(1,0)))
  expect_equal(performance(pred, "auc")@y.values[[1]],
              0)    
  pred <- prediction(seq(0, 1, length=10),
                     ordered(c(rep("A",5), rep("B",5))))
  expect_equal(performance(pred, "auc")@y.values[[1]],
              1)
  
  expect_error(pred <- prediction(seq(0, 1, length=10),
                                    c(rep(0,5), rep(1,5)), label.ordering=c(1,2)))
  expect_error(pred <- prediction(list(c(0.1,0.3,0.7,1),
                                         c(0,0.2,0.8,1)),
                                    list(factor(c(0,0,1,1)),
                                         factor(c(1,1,2,2))))) 
  expect_error(pred <- prediction(list(c(0.2,0.3,0.7,1),
                                         c(0,0.2,0.8,1)),
                                    list(factor(c(0,0,1,1)),
                                         ordered(c(0,0,1,1)))))
  pred <- prediction(list(c(0,0.3,0.7,1),
                          c(0,0.2,0.8,1)),
                     list(factor(c(0,0,1,1)),
                          factor(c(0,0,1,1))))
  expect_equal(performance(pred, "auc")@y.values,
              list(1, 1))
  
  pred1 <- prediction(data.frame(c(0,0.3,0.7,1),
                                 c(0,0.2,0.8,1)),
                      data.frame(factor(c(0,0,1,1)),
                                 factor(c(0,0,1,1))))
  expect_equal( pred, pred1)
  
  pred2 <- prediction(cbind(c(0,0.3,0.7,1),
                            c(0,0.2,0.8,1)),
                      cbind(c(0,0,1,1),
                            c(0,0,1,1)))
  expect_equal(pred, pred2)
})
