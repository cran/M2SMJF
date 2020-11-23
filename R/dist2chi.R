#' Calculate the chi-squared distance
#' 
#' Calculate the chi-squared distance of two any pair-wise samples x_i and x_j for discrete variables
#' 
#' @param X A sample-feature matrix with rows as samples and columns as features
#' 
#' @param C The same as X
#' 
#' @return A matrix whose elements at (i,j) is the chi-squared distance of two any pair-wise samples x_i and x_j
#' @author Xiaoyao Yin
#' @examples 
#' 
#' data_list <- simu_data_gen()
#' X <- data_list[[1]]
#' C <- X
#' Diff <- dist2chi(X,C)
#' @rdname dist2chi
#' @export
dist2chi <- function(X,C) {
  ndata = nrow(X)
  ncentres = nrow(C)
  meanX = apply(X,2,mean)
  meanC = apply(C,2,mean)

  res = matrix(0,ndata,ncentres)
  for (i in 1:ndata) 
  {
  	for (j in 1:ncentres) 
  	{
      res[i,j] = sum((X[i,]-C[j,])^2/(X[i,]+C[j,]))/2
  	}
  }
  return(res)
}