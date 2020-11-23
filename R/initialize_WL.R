#' Initialize from the similairty matrix list
#' 
#' Factorize the each of the similairty matrix Si into Ci*t(Ci) by SVD
#' 
#' @param WL A list of similarity matrices
#' 
#' @return A list as long as WL with elements satisfying res[[i]]%*%t(res[[i]])=WL[[i]]
#' @author Xiaoyao Yin
#' @examples 
#' 
#' WL <- simu_data_gen()
#' new_WL_list <- initialize_WL(WL)
#' @rdname initialize_WL
#' @export
initialize_WL <- function(WL)
{
  NN <- length(WL)
  res <- vector("list",NN)
  for (i in 1:NN)
  {
    SVD_res <- svd(WL[[i]])
    dd <- SVD_res$d
    dd1 <- sqrt(dd)
    res[[i]] <- SVD_res$u%*%diag(dd1)
  }
  return(res)
}