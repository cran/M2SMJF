#' Calculate the cost
#' 
#' A function to calculate the cost of the objective function
#' 
#' @param new_WL_list A list of matrices factorized from the similarity matrices list WL 
#' 
#' @param init_list A list containing the updated result in this iteration
#' 
#' @param lambda A parameter to set the relative weight of the group sparsity constraints
#' 
#' @return A number indicating the total cost of the objective function
#' @author Xiaoyao Yin
#' @examples 
#' 
#' WL <- simu_data_gen()
#' WL[[1]] <- affinityMatrix(dist2eu(Standard_Normalization(WL[[1]]),Standard_Normalization(WL[[1]])))
#' WL[[2]] <- affinityMatrix(dist2eu(Standard_Normalization(WL[[2]]),Standard_Normalization(WL[[2]])))
#' new_WL_list <- initialize_WL(WL)
#' k <- 5
#' lambda <- 0.25
#' init_list <- initialization(new_WL_list,k)
#' update_L_list <- update_L(new_WL_list,init_list)
#' update_alpha_list <- update_alpha(new_WL_list,update_L_list,lambda)
#' init_list <- update_alpha_list
#' new_loss <- cost(new_WL_list,init_list,lambda)
#' @rdname cost
#' @export
cost <- function(new_WL_list,init_list,lambda)
{
  res <- 0
  NN <- length(new_WL_list)
  alpha <- init_list[[(NN+1)]]
  for (i in 1:NN)
  {
    matr_loss <- new_WL_list[[i]]-alpha%*%init_list[[i]]
    res <- res+(norm(matr_loss,"F")^2)/2
  }
  res <- res+lambda*init_list[[(NN+2)]]
  return(res)
}
