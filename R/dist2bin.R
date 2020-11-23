#' Calculate the agreement-based measurement
#' 
#' Calculate the agreement-based measurement of two any pair-wise samples x_i and x_j for binary variables
#' 
#' @param X A sample-feature matrix with rows as samples and columns as features
#' 
#' @param C The same as X
#' 
#' @return A matrix whose elements at (i,j) is the agreement-based measurement of two any pair-wise samples x_i and x_j
#' @author Xiaoyao Yin
#' @examples 
#' 
#' data_list <- simu_data_gen()
#' X <- data_list[[1]]
#' C <- X
#' Diff <- dist2bin(X,C)
#' @rdname dist2bin
#' @export
dist2bin <- function(X,C) {
  ndata = nrow(X)
  ncentres = nrow(C)

  res = matrix(0,ndata,ncentres)
  for (i in 1:ndata) 
  {
  	for (j in 1:ncentres) 
  	{
  		res[i,j] = sum(X[i,]!=C[j,])
  	}
  }
  return(res)
}