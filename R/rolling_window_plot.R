#' Rolling Window Plot
#'
#' @param your_x_variable this should be a data frame
#' @param your_y_variable this should be a data frame
#' @param your_dataset this should be a data frame containing all the variables you want to analyse
#'
#' @return Return the rolling window plot of the relationship between x and y
#' @export
#'

rolling_plot = function(your_x_variable, your_y_variable, your_dataset){
  your_y_variable = unclass(your_y_variable, data = your_dataset)
  your_x_variable = unclass(your_x_variable, data = your_dataset)
  obs = length(your_y_variable)
  reg = lm(your_y_variable ~ your_x_variable)
  hedge_ratio = reg$coefficient[2]

  Z_score = 1.96
  spread = data.frame(your_y_variable - hedge_ratio * your_x_variable)
  colnames(spread) = "Spread"
  avg = mean(spread$Spread)
  sd = sd(spread$Spread)
  spread$mean = rep(avg, obs)
  spread$lower_bound = rep(avg -( Z_score * sd ), length(spread$Spread))
  spread$higher_bound = rep(avg + ( Z_score * sd ), length(spread$Spread))

  plot(spread$Spread,
       ylab="Spread",
       type="l",
       col="black")
  lines(spread$mean, lty = "dashed", col = "red" )
  lines(spread$lower_bound, lty = "dotted", col = "green")
  lines(spread$higher_bound, lty = "dotted", col = "green")
  legend("topleft",
         c("Spread","Mean","Mean+-2SD"),
         fill=c("black","red", "green")
  )
}
