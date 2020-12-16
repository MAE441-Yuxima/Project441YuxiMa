#' KNN Imputation
#'
#' @param target_variable This should be the name of the variable you want to impute
#' @param your_dataset This should be the dataset containing the target variable
#'
#' @return Returns the whole data frame with the missing value of the target variable being imputed
#' @export
#'
knn_impute <- function(target_variable, your_dataset){
  num_vars = ncol(your_dataset)
  data_knn = VIM::kNN(your_dataset, variable = colnames(your_dataset[,2:num_vars] ) , k = 5)
  data_knn = data_knn[1:num_vars]
  return(data_knn)
}
