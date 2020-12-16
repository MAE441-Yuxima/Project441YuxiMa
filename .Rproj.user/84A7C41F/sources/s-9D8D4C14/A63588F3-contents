#' Descriptive Plots
#'
#' @param your_variable This should be the name of the variable you want to plot
#' @param your_dataset This should be the dataset containing the target variable
#'
#' @return Returns the histogram, quantile plots and boxplots of your variable
#' @export
#'
draw_all_plot <- function(your_variable, your_dataset){
  #Histogram
  hist(your_variable, data = your_dataset, col="skyblue4", freq = FALSE)
  #fitted distributions
  lines(density(your_dataset$your_variable),lwd = 2, col ="red")
  #quantile plots
  qqnorm(your_dataset, data = your_dataset)
  #boxplots
  boxplot(your_variable, data = your_dataset)
}
