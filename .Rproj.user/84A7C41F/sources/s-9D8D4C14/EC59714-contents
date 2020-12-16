#' Geometric Mean
#'
#' @param your_return this should be a data frame consisting of return data
#'
#' @return Returns the geometric mean of the data input.
#' @export
#'
geom_return<-function(your_return){
  if(is.data.frame(your_return)){
    returns<-c()
    for(i in 1:ncol(your_return)){
      returns<-append(returns, prod(1+your_return[,i])-1)
    }
    return(returns)
  }
  return(prod(1+your_return)-1)
}
