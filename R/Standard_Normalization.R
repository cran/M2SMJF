#' Normalize the input matrix by column
#' 
#' Normalize each column of x to have mean 0 and standard deviation 1.
#' 
#' @param x A sample-feature matrix with rows as samples and columns as features
#' 
#' @return A sample-feature matrix with rows as samples and columns as features,each column of the matrix have mean 0 and standard deviation 1
#' @author Xiaoyao Yin
#' @examples 
#' 
#' data_list <- simu_data_gen()
#' x <- data_list[[1]]
#' data_matrix <- Standard_Normalization(x)
#' @rdname Standard_Normalization
#' @export
Standard_Normalization = function(x) {
  x = as.matrix(x);
  mean = apply(x, 2, mean)
  sd = apply(x, 2, sd)
  sd[sd==0] = 1
  xNorm = t((t(x) - mean) / sd)
  return(xNorm)
}