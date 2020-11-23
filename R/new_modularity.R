#' Calculate the modularity
#'
#' A function to calculate the modularity for weighted graph
#'
#' @param init_list A list with N+2 elements, the former N as modality private sub-matrices, the Nth as the shared sub-matrix and the last one as the current loss
#'
#' @param WL A list of similarity matrices
#'
#' @return A single value indicating the mudularity of current factorization and clustering
#' @author Xiaoyao Yin
#' @examples
#'
#' WL <- simu_data_gen()
#' WL[[1]] <- affinityMatrix(dist2eu(Standard_Normalization(WL[[1]]),Standard_Normalization(WL[[1]])))
#' WL[[2]] <- affinityMatrix(dist2eu(Standard_Normalization(WL[[2]]),Standard_Normalization(WL[[2]])))
#' new_WL_list <- initialize_WL(WL)
#' init_list <- initialization(new_WL_list,5)
#' res <- M2SMJF(WL,0.25,10^-4,5)
#' init_list <- res[[1]]
#' modularity <- new_modularity(init_list,WL)
#' @rdname new_modularity
#' @export
new_modularity <- function(init_list,WL)
{
  NN <- length(init_list)-2
  alpha <- init_list[[(NN+1)]]
  sample_num <- nrow(alpha)
  clu_num <- ncol(alpha)
  clusters <- kmeans(alpha,clu_num,iter.max = 1000,nstart = 20)
  clu_res <- clusters$cluster
  cluster_list <- vector("list",clu_num)
  for (i in 1:clu_num)
  {
    cluster_list[[i]] <- which(clu_res==i,T)
  }
  all_sum <- vector("numeric",NN)
  for (i in 1:NN)
  {
    all_sum[i] <- sum(WL[[i]])
  }
  s_matr_list <- vector("list",NN)
  for (i in 1:NN)
  {
    s_matr <- matrix(0,clu_num,clu_num)
    X_matr <- WL[[i]]
    for (j in 1:clu_num)
    {
      for (k in 1:clu_num)
      {
        j_index <- cluster_list[[j]]
        k_index <- cluster_list[[k]]
        s_matr[j,k] <- sum(X_matr[j_index,k_index])/all_sum[i]
      }
    }
    s_matr_list[[i]] <- s_matr
  }
  Q <- vector("numeric",NN)
  for (i in 1:NN)
  {
    qq <- 0
    s_matr <- s_matr_list[[i]]
    for (j in 1:clu_num)
    {
      incre <- s_matr[j,j]-(sum(s_matr[j,]))^2
      qq <- qq+incre
    }
    Q[i] <- qq
  }
  return(Q)
}
