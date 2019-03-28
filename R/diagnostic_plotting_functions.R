## Script for plotting output from OM
## Brett Stacy
## 12_03_2019

## Objective: load output from OM generated from a scenario (start with Paul's) and plot for every year the age distribution of
## fish. Formulate a function that automates this. Eventually, use it for my 2 area model to visualize age structure convergence for tuning.


## Load OM output from some model:


#' SSB Time Series
#'
#' Plot SSB from output from Planetfish2 operating model or assessment model.
#'
#' This function plots data from the \code{output} matrix. This can be either OM or AM output as SSB or REC etc.
#' The intention is to be able to plot an item by passing the unique column name identifier to \code{plot()}.
#' @param output The output matrix from a model run.
#' @param item = OM_ssb_R1 as default but can be any known column name type.
#' @param mean = Logical. Should the mean be plotted?
#' @return Returns a time series plot of item.
#' @export
plot_OM_SSB = function(output, item = "OM_ssb_R1", mean = TRUE, ...){
  if(mean == T){
    get_item = output[, grepl(item, colnames(output))]
    if(NCOL(get_item) == 1){
      plot(get_item, xlab = "Year", ylab = item, type = "l", ...)
    }
    else {
      plot(colMeans(get_item), xlab = "Year", ylab = item, type = "l", ...)
    }
  }
  else {
    get_item = output[, grepl(item, colnames(output))]
    boxplot(get_item, xlab = "Year", ylab = item, type = "l", xaxt = "n", ...)
    axis(1, at = seq(10, round(dim(get_item)[2], -1), 10))
  }
}

# plot_OM_SSB(output)





#' Age structure of a population.
#'
#' Plot histogram of age frequencies for a particular region.
#'
#' This function plots age frequency data from the \code{output2} array. \code{output2} should be of dimention [n_iters, ages, years, regions]
#' @param output2 The output array from a model run.
#' @param region Which region should plotted?
#' @param skip Skip years if needed. = 1 means don't skip any years.
#' @return Returns a histogram of age structure for every year.
#' @export
plot_OM_Ages = function(output2, region, skip){
  par(mfrow = c(4,5), mar = c(.5, .5, .5, .5))
  for (i in seq(1, dim(output2)[3], skip)) {
    mean_iter = apply(output2, c(2,3,4), mean)
    barplot(mean_iter[,i,region], yaxt = "n", xaxt = "n")
    legend("topright", paste("year:", i), bty = "n")
  }
}

# plot_OM_Ages(output2, 1)





#' Relative Error
#'
#' Plot relative error statistic between SSBs
#'
#' This function uses data from the \code{output} matrix to plot the relative error between operating and assessment model output quantities.
#' This will need adaptation as more results are acquired. The main one will be to have the option to vectorize the boxplots like Pauls.
#' @param output The output matrix from a model run.
#' @param truth The true value(s).
#' @param est The estimated value(s).
#' @param type Switch. Type of SSB comparison. Options: "initial", "current", "status".
#' @return Returns a boxplot of relative error.
#' @export
plot_SSB_err = function(output, truth, est, type, ...){

  switch(type,
         initial = {om_output <- output[, grepl(truth, colnames(output))]/2;
         am_output <- output[, grepl(est, colnames(output))]},

         current = {om_output <- output[, grepl(truth, colnames(output))];
         am_output <- output[, grepl(est, colnames(output))]},

         status = {om_output <- output[, grepl(truth, colnames(output))]/(output[, grepl("OM_ssb0", colnames(output))]/2);
         am_output <- output[, grepl(est, colnames(output))]/output[, grepl("AM_ssb0", colnames(output))]})

  err = (am_output - om_output)/om_output

  boxplot(err, ...)


}

# plot_err(output, truth = "OM_ssb0", est = "AM_ssb0", half = "yes")
