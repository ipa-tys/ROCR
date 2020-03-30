
context("plot")
test_that("plot:",{
    some.predictions <- c(0.02495517, 0.92535646,
                          0.86251887, 0.80946685,
                          0.70922858, 0.69762824,
                          0.50604485, 0.25446810,
                          0.10837728, 0.07250349)
    some.labels <- c(0,1,1,0,1,1,0,1,0,0)
    
    .get.performance <- function(pred) {
        
        tpr <- performance(pred, "tpr")
        fpr <- performance(pred, "fpr")
        acc <- performance(pred, "acc")
        err <- performance(pred, "err")
        
        rec <- performance(pred, "rec")
        sens<- performance(pred, "sens")
        fnr <- performance(pred, "fnr")
        tnr <- performance(pred, "tnr")
        spec<- performance(pred, "spec")
        ppv <- performance(pred, "ppv")
        prec<- performance(pred, "prec")
        npv <- performance(pred, "npv")
        
        fall<- performance(pred, "fall")
        miss<- performance(pred, "miss")
        pcfall <- performance(pred, "pcfall")
        pcmiss <- performance(pred, "pcmiss")
        rpp <- performance(pred, "rpp")
        rnp <- performance(pred, "rnp")
        
        auc <- performance(pred, "auc")
        prbe<- performance(pred, "prbe")
        rch <- performance(pred, "rch")
        
        mxe <- performance(pred, "mxe")
        rmse<- performance(pred, "rmse")
        
        phi <- performance(pred, "phi")
        mat <- performance(pred, "mat")
        mi  <- performance(pred, "mi")
        chisq<- performance(pred, "chisq")
        odds<- performance(pred, "odds")
        lift<- performance(pred, "lift")
        f   <- performance(pred, "f")
        sar <- performance(pred,"sar")
        ecost  <- performance(pred, "ecost")
        cost  <- performance(pred, "cost")
        return(list(tpr=tpr, fpr=fpr, acc=acc, err=err,
                    rec=rec, sens=sens, fnr=fnr, tnr=tnr,
                    spec=spec, ppv=ppv, prec=prec, npv=npv, 
                    fall=fall, miss=miss, pcfall=pcfall, pcmiss=pcmiss, rpp=rpp, rnp=rnp,
                    auc=auc, prbe=prbe, rch=rch, mxe=mxe, 
                    rmse=rmse, phi=phi, mat=mat, mi=mi, chisq=chisq, odds=odds,
                    lift=lift, f=f, sar=sar, ecost=ecost, cost=cost))
        
    }
    pred <- prediction(some.predictions, some.labels)
    measures <- expect_warning(.get.performance(pred),
                               "Chi-squared approximation may be incorrect")
    actual1 <- measures[[1]]
    expect_error(plot(measures[[1]], colorize = TRUE),
                 "Threshold coloring or labeling cannot be performed")
    for(i in seq_along(measures)){
        if(names(measures[i]) %in% c("auc","mxe","rmse")){
            expect_error(plot(measures[[i]]))
        }
    }
    
    data(ROCR.hiv)
    pp <- ROCR.hiv$hiv.svm$predictions
    ll <- ROCR.hiv$hiv.svm$labels
    pred <- prediction(pp, ll)
    expect_error(ROCR:::.combine.performance.objects(actual1,performance(pred, "fpr")),
                 "Only performance objects with identical number of cross-validation")
    perf <- performance(pred, "tpr", "fpr")
    expect_null({
        plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
             main= "With ROCR you can produce standard plots like ROC curves ...")
        plot(perf, lty=3, col="grey78", add=TRUE)
    })
    perf <- performance(pred, "prec", "rec")
    expect_null({
        plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
             main= "... Precision/Recall graphs ...")
        plot(perf, lty=3, col="grey78", add=TRUE)
    })
    perf <- performance(pred, "sens", "spec")
    expect_null({
        plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
             main="... Sensitivity/Specificity plots ...")
        plot(perf, lty=3, col="grey78", add=TRUE)
    })
    perf <- performance(pred, "lift", "rpp")
    expect_null({
        plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
             main= "... and Lift charts.")
        plot(perf, lty=3, col="grey78", add=TRUE)
    })
    
    perf <- performance(pred, "tpr", "fpr")
    expect_null({
        plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
             main= "With ROCR you can produce standard plots like ROC curves ...",
             downsampling = 0.5)
    })
    expect_null({
        plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
             main= "With ROCR you can produce standard plots like ROC curves ...",
             downsampling = 0.9)
    })
    expect_null({
        plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
             main= "With ROCR you can produce standard plots like ROC curves ...",
             downsampling = 1)
    })
    expect_error(plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
                      main= "With ROCR you can produce standard plots like ROC curves ...",
                      downsampling = 1.1),
                 "'from' must be a finite number")
    
    data(ROCR.xval)
    pp <- ROCR.xval$predictions
    ll <- ROCR.xval$labels
    pred <- prediction(pp,ll)
    perf <- performance(pred,'tpr','fpr')
    
    expect_null({
        plot(perf, colorize=TRUE, lwd=2,
             main='ROC curves from 10-fold cross-validation')
    })
    expect_null({
        plot(perf, avg='vertical', spread.estimate='stderror',lwd=3,
             main='Vertical averaging + 1 standard error',col='blue')
    })
    expect_null({
        plot(perf, avg='horizontal', spread.estimate='boxplot',lwd=3,
             main='Horizontal averaging + boxplots',col='blue')
    })
    expect_null({
        plot(perf, avg='threshold', spread.estimate='stddev',lwd=2,
             main='Threshold averaging + 1 standard deviation',colorize=TRUE)
    })
    
    ############################################################################
    # removed because vdiffr is not available on mac
    ############################################################################
    
    # vdiffr
    # skip_on_ci()
    # skip_on_os("mac")
    # skip_if_not_installed("vdiffr")
    # for(i in seq_along(measures)){
    #     if(!(names(measures[i]) %in% c("auc","mxe","rmse"))){
    #         vdiffr::expect_doppelganger(names(measures[i]), plot(measures[[i]]))
    #     } else {
    #         expect_error(plot(measures[[i]]))
    #     }
    # }
    # 
    # data(ROCR.hiv)
    # pp <- ROCR.hiv$hiv.svm$predictions
    # ll <- ROCR.hiv$hiv.svm$labels
    # pred <- prediction(pp, ll)
    # expect_error(ROCR:::.combine.performance.objects(actual1,performance(pred, "fpr")),
    #              "Only performance objects with identical number of cross-validation")
    # perf <- performance(pred, "tpr", "fpr")
    # vdiffr::expect_doppelganger("ROC-curve",{
    #     plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
    #          main= "With ROCR you can produce standard plots like ROC curves ...")
    #     plot(perf, lty=3, col="grey78", add=TRUE)
    # })
    # perf <- performance(pred, "prec", "rec")
    # vdiffr::expect_doppelganger("Precision-Recall-graph",{
    #     plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
    #          main= "... Precision/Recall graphs ...")
    #     plot(perf, lty=3, col="grey78", add=TRUE)
    # })
    # perf <- performance(pred, "sens", "spec")
    # vdiffr::expect_doppelganger("Sensitivity-Specificity-plots",{
    #     plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
    #          main="... Sensitivity/Specificity plots ...")
    #     plot(perf, lty=3, col="grey78", add=TRUE)
    # })
    # perf <- performance(pred, "lift", "rpp")
    # vdiffr::expect_doppelganger("lift-chart",{
    #     plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
    #          main= "... and Lift charts.")
    #     plot(perf, lty=3, col="grey78", add=TRUE)
    # })
    # 
    # perf <- performance(pred, "tpr", "fpr")
    # vdiffr::expect_doppelganger("ROC-curve-downsampling1",{
    #     plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
    #          main= "With ROCR you can produce standard plots like ROC curves ...",
    #          downsampling = 0.5)
    # })
    # vdiffr::expect_doppelganger("ROC-curve-downsampling2",{
    #     plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
    #          main= "With ROCR you can produce standard plots like ROC curves ...",
    #          downsampling = 0.9)
    # })
    # vdiffr::expect_doppelganger("ROC-curve-downsampling3",{
    #     plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
    #          main= "With ROCR you can produce standard plots like ROC curves ...",
    #          downsampling = 1)
    # })
    # expect_error(plot(perf, avg= "threshold", colorize=TRUE, lwd= 3,
    #                   main= "With ROCR you can produce standard plots like ROC curves ...",
    #                   downsampling = 1.1),
    #              "'from' must be a finite number")
    # dev.off()
    # 
    # data(ROCR.xval)
    # pp <- ROCR.xval$predictions
    # ll <- ROCR.xval$labels
    # pred <- prediction(pp,ll)
    # perf <- performance(pred,'tpr','fpr')
    # 
    # vdiffr::expect_doppelganger("ROC-cross-valid",{
    #     plot(perf, colorize=TRUE, lwd=2,
    #          main='ROC curves from 10-fold cross-validation')
    # })
    # vdiffr::expect_doppelganger("ROC-vertical-avg",{
    #     plot(perf, avg='vertical', spread.estimate='stderror',lwd=3,
    #          main='Vertical averaging + 1 standard error',col='blue')
    # })
    # vdiffr::expect_doppelganger("ROC-horizontal-avg",{
    #     plot(perf, avg='horizontal', spread.estimate='boxplot',lwd=3,
    #          main='Horizontal averaging + boxplots',col='blue')
    # })
    # vdiffr::expect_doppelganger("ROC-threshold-avg",{
    #     plot(perf, avg='threshold', spread.estimate='stddev',
    #                         lwd=2,
    #          main='Threshold averaging + 1 standard deviation',colorize=TRUE)
    # })
    
})
