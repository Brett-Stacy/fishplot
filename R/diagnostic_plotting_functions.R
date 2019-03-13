## Script for plotting output from OM
## Brett Stacy
## 12_03_2019

## Objective: load output from OM generated from a scenario (start with Paul's) and plot for every year the age distribution of
## fish. Formulate a function that automates this. Eventually, use it for my 2 area model to visualize age structure convergence for tuning.


## Load OM output from some model:

# line plot tracking SSB
plot_OM_SSB = function(output, item = "OM_ssb_R1", mean = TRUE, ...){
  if(mean == T){
    get_item = colMeans(output[, grepl(item, colnames(output))])
    plot(get_item, xlab = "Year", ylab = item, type = "l")
  }
  else {
    get_item = output[, grepl(item, colnames(output))]
    boxplot(get_item, xlab = "Year", ylab = item, type = "l")
  }
}

# plot_OM_SSB(output)





# tracking age structure
plot_OM_Ages = function(output2, region){
  par(mfrow = c(4,5), mar = c(.5, .5, .5, .5))
  for (i in seq(1, dim(output2)[3], 5)) {
    mean_iter = apply(output2, c(2,3,4), mean)
    barplot(mean_iter[,i,region], yaxt = "n", xaxt = "n")
  }
}

# plot_OM_Ages(output2, 1)

