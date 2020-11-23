#' the function to update Li, for i=1,2,...,N
#' 
#' update the sub-matrix Li, for i=1,2,...,N to convergence to its local minimum gradually
#' 
#' @param WL A list of similarity matrices
#' 
#' @param init_list A list with N+2 elements, the former N as modality private sub-matrices, the Nth as the shared sub-matrix and the last one as 1
#' 
#' @return A list containing the updated result in this iteration

#' @author Xiaoyao Yin
#' @examples 
#' WL <- simu_data_gen()
#' WL[[1]] <- affinityMatrix(dist2eu(Standard_Normalization(WL[[1]]),Standard_Normalization(WL[[1]])))
#' WL[[2]] <- affinityMatrix(dist2eu(Standard_Normalization(WL[[2]]),Standard_Normalization(WL[[2]])))
#' new_WL_list <- initialize_WL(WL)
#' k <- 5
#' init_list <- initialization(new_WL_list,k)
#' update_L_list <- update_L(WL,init_list)
#' @rdname update_L
#' @export
update_L <- function(WL,init_list)
{
  N1 <- length(init_list)-2
  alpha1 <- init_list[[(N1+1)]]
  dominators <- ginv(t(alpha1)%*%alpha1)
  ## update each Di separately
  for (b in 1:N1)
  {
    D_b <- init_list[[b]]
    inter_varia <- t(alpha1)%*%WL[[b]]
    init_list[[b]] <- dominators%*%inter_varia
  }
  return(init_list)
}