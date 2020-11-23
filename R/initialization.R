#' initialize the sub-matrix Ci into alpha*Li by SVD
#' 
#' Li takes the first k columns of matrix d in SVD, while alpha is the mean of all the u of SVD result in each modality
#' 
#' @param WL A list of similarity matrices
#' 
#' @param k A parameter to specify the cluster number
#' 
#' @return A list with N+2 elements, the former N as modality private sub-matrices, the Nth as the shared sub-matrix and the last one as 1
#' @author Xiaoyao Yin
#' @examples 
#' 
#' WL <- simu_data_gen()
#' new_WL_list <- initialize_WL(WL)
#' k <- 5
#' init_list <- initialization(new_WL_list,k)
#' @rdname initialization
#' @export
initialization <- function(WL,k)
{
  NN <- length(WL)
  res <- vector("list",(NN+2))
  alpha_list <- vector("list",NN)
  index_i <- vector("numeric",NN)
  for (i in 1:NN)
  {
    SVD_res <- svd(WL[[i]])
    alpha_list[[i]] <- SVD_res
    DD <- abs(SVD_res$d)
  }
  alpha0 <- matrix(0,nrow(WL[[i]]),k)
  for (i in 1:NN)
  {
    S_V_D <- alpha_list[[i]]
    DD <- abs(S_V_D$d)
    DD <- DD[1:k]
    res[[i]] <- diag(sqrt(DD))
    alpha0 <- alpha0+S_V_D$u[,1:k]
  }
  res[[(NN+1)]] <- alpha0/NN
  res[[(NN+2)]] <- 1
  return(res)
}