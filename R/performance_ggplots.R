
.get_ggplot2_data <- function(perf){
    if(!is.null(names(perf@x.values))) 
        group <- names(perf@x.values)
    else 
        group <- seq.int(1L,length(perf@x.values))
    data <- data.frame(x.values = unlist(perf@x.values),
                       y.values = unlist(perf@y.values),
                       group = unlist(mapply(rep,group,lengths(perf@x.values))),
                       stringsAsFactors = FALSE) 
    data
}

.ggplot.performance <- function(data,
                                ...,
                                avg = "none",
                                spread.estimate = "none",
                                spread.scale = 1,
                                show.spread.at = c(),
                                colorize = FALSE,
                                colorize.palette = rev(grDevices::rainbow(256, start = 0, end = 4 / 6)),
                                colorkey = colorize,
                                colorkey.relwidth = 0.25,
                                colorkey.pos = "right",
                                print.cutoffs.at = c(),
                                cutoff.label.function = function(x) {
                                    round(x, 2)
                                },
                                downsampling = 0,
                                add = FALSE,
                                environment = parent.frame()){
    # Input checks
    .check_performance_for_plotting(perf, colorize, print.cutoffs.at, avg)
    # Input norm
    if (downsampling > 0) perf <- .downsample(perf, downsampling)
    if (length(perf@alpha.values) != 0) perf <- .norm_alpha_values(perf)
    perf <- .norm_x_y_values(perf)
    # getting the arguments
    arglist <- c(lapply(as.list(environment()), eval ), list(...) )
    arglist <- .sarg( arglist, perf=data)
    
    # create input for ggplot2
    mapping <- ggplot2::aes(x = "x.values", y = "y.values")
    arglist <- .sarg( arglist, plot = ggplot2::ggplot(.get_ggplot2_data(perf),
                                                      mapping = mapping))
    if (avg=="none") 
        plot <- do.call(".performance.ggplot.no.avg", arglist)
    else if (avg=="vertical")
        plot <- do.call(".performance.ggplot.vertical.avg", arglist)
    else if (avg=="horizontal")
        plot <- do.call(".performance.ggplot.horizontal.avg", arglist)
    else if (avg=="threshold")
        plot <- do.call(".performance.ggplot.threshold.avg", arglist)
    plot
}

.performance.ggplot.no.avg <- function(plot, perf, ...){
    plot
}

.performance.ggplot.vertical.avg <- function(plot, perf, ...){
    plot
}

.performance.ggplot.horizontal.avg <- function(plot, perf, ...){
    plot
}

.performance.ggplot.threshold.avg <- function(plot, perf, ...){
    plot
}
