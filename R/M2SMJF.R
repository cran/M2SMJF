#' the main part for M2SMJF and clustering result
#'
#' jointly factorize multiple matrices into a shared sub-matrix and multiple private sub-matrices
#'
#' @param WL A list of similarity matrices
#'
#' @param lambda A parameter to set the relative weight of the group sparsity constraints
#'
#' @param theta A parameter to determine the convergence
#'
#' @param k A parameter to specify the cluster number
#' @return A list containing the clustering result
#' \item{sub_matrices}{a list containing all the sub-matrices}
#' \item{cluster_res}{the clustering result which is as long as the number of samples}
#' @author Xiaoyao Yin
#' @examples
#'
#' WL <- simu_data_gen()
#' res <- M2SMJF(WL,0.25,10^-4,5)
#' @rdname M2SMJF
#' @export
#' @importFrom MASS ginv
#' @import dplyr
#' @importFrom stats kmeans rnorm
M2SMJF <- function(WL,lambda=0.25,theta=10^-4,k){
  if (!is.list(WL))
  {
    stop('Error:please provide a data list by WL')
  }
  if (is.na(k))
  {
    stop('Error:please provide a cluster number by k')
  }
  N <- length(WL)  ##length of the similarity matrices list
  new_WL_list <- initialize_WL(WL)  ## factorize the each of the similairty matrix Si into Ci*t(Ci)
  init_list <- initialization(new_WL_list,k)  ## initialize the sub-matrix Ci into alpha*Li
  ### minimize the objective function until convergence
  divergence <- 1 ##initialize the divergence
  iter_num <- 1  ##initialize the iteration number
  old_cost <- 0   ##initialize the cost
  while (divergence>theta)  ##while not convergent
  {
    update_L_list <- update_L(new_WL_list,init_list)  ##update all the L with update_L
    update_alpha_list <- update_alpha(new_WL_list,update_L_list,lambda)  ##update all the alpha with update_alpha
    init_list <- update_alpha_list
    new_cost <- cost(new_WL_list,init_list,lambda)  ## calculate the cost of objective function
    divergence <- abs((new_cost-old_cost)/new_cost)  ## calculate the relative variation of cost
    old_cost <- new_cost
    iter_num <- iter_num+1
  }
  alpha <- init_list[[(N+1)]]  ## sub_matrix alpha is the last one in init_list
  clu_res <- kmeans(alpha,k,1000,20)  ## clustering the samples by kmeans on the sub_matrix alpha, which is a conventional method in matrix factorization based clustering.
  cluster_res <- clu_res$cluster  ## get the clustering result, the ith element indicates the cluster of the ith sample
  result <- vector("list",2)
  names(result) <- c("sub_matrices","cluster_res")
  sub_result <- init_list
  names(sub_result) <- c(paste("sub_matrix_L",c(1:N),sep='_'),'alpha')
  result[[1]] <- sub_result
  result[[2]] <- cluster_res
  return(result)
}

