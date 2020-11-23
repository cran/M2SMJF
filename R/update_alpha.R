#' the function to update alpha
#' 
#' update the sub-matrix alpha to convergence to its local minimum gradually
#' 
#' @param WL A list of similarity matrices
#' 
#' @param update_L_list A list with N+2 elements, the former N as modality private sub-matrices, the Nth as the shared sub-matrix and the last one as the current loss
#' 
#' @param lambda A parameter to set the relative weight of the group sparsity constraints
#' 
#' @return A list containing the updated result in this iteration

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
#' update_alpha_list <- update_alpha(WL,update_L_list,lambda)
#' @rdname update_alpha
#' @export
update_alpha <- function(WL,update_L_list,lambda)
{
  N2 <- length(update_L_list)-2
  alpha1 <- update_L_list[[(N2+1)]]
  loss <- 0
  yyy <- kmeans(alpha1,ncol(alpha1),iter.max = 1000,nstart = 20)
  clu <- yyy$cluster
  size_clu <- yyy$size
  classify <- vector("list",ncol(alpha1))
  count11 <- 0
  for (i in 1:length(size_clu))
  {
    clu_size <- size_clu[i]
    classify[[i]] <- clu[(count11+1):(count11+clu_size)]
    count11 <- count11+clu_size
  }
  new_alpha1 <- alpha1
  for (i in 1:length(classify))
  {
    index <- classify[[i]]
    unit_col <- ncol(WL[[1]])
    X <- matrix(0,length(index),unit_col*N2)
    D <- matrix(0,1,unit_col*N2)
    nnn <- ncol(alpha1)
    for (j in 1:nnn)
    {
      for (k in 1:N2)
      {
        if (nnn==2)
        {
          X[1:length(index),(unit_col*(k-1)+1):(unit_col*k)] <- WL[[k]][index,]-as.matrix(alpha1[index,-j])%*%t(update_L_list[[k]][-j,])
          D[1,(unit_col*(k-1)+1):(unit_col*k)] <- update_L_list[[k]][j,]
        }
        else
        {
          X[1:length(index),(unit_col*(k-1)+1):(unit_col*k)] <- WL[[k]][index,]-alpha1[index,-j]%*%update_L_list[[k]][-j,]
          D[1,(unit_col*(k-1)+1):(unit_col*k)] <- update_L_list[[k]][j,]
        }
        
      }
      #       print(paste("i is ",i,"j is ",j,sep=" "))
      #       print(paste("dim x is ",dim(X),"dim D is ",dim(D),sep="  "))
      shrink <- D%*%t(D)
      yy <- X%*%t(D)/(shrink[1,1])
      lambda1 <- lambda/(shrink[1,1])
      abs_y <- sort(abs(yy),decreasing = T)
      count00 <- 0
      for (k in 1:length(abs_y))
      {
        aa<- (sum(abs_y[1:k])-lambda1)/k
        if (aa<abs_y[k])
        {
          count00 <- k
        }
      }
      if (count00==0)
      {
        yy <- 0
      }
      else
      {
        tao <- (sum(abs_y[1:count00])-lambda1)/count00
        for (k in 1:length(abs_y))
        {
          if (yy[k]>=tao)
          {
            yy[k] <- tao
          }
          else if (yy[k]<=(-tao))
          {
            yy[k] <- (-tao)
          }
          else
          {
            yy[k] <- 0
          }
        }
        
      }
      new_alpha1[index,j] <- yy
      loss <- loss+max(abs(yy))
    }
  }
  update_L_list[[(N2+1)]] <- new_alpha1
  update_L_list[[(N2+2)]] <- loss
  return(update_L_list)
}