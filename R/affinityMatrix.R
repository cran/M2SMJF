#' To calculate the similarity matrix
#' 
#' calculate the affinity matrix from the diff matrix with 20 neighbors
#' 
#' @param Diff A diff matrix
#' 
#' @param K The number of neighbors in consideration
#' 
#' @param sigma A parameter to determine the scale
#' 
#' @return W The similarity matrix
#' @author Xiaoyao Yin
#' @examples 
#' 
#' data_list <- simu_data_gen()
#' Diff <- dist2eu(Standard_Normalization(data_list[[1]]),Standard_Normalization(data_list[[1]]))
#' simi <- affinityMatrix(Diff,20,0.5)
#' @rdname affinityMatrix
#' @export
affinityMatrix <- function(Diff,K=20,sigma=0.5) {
  N = nrow(Diff)
  
  Diff = (Diff + t(Diff)) / 2
  diag(Diff) = 0;
  sortedColumns = as.matrix(t(apply(Diff,2,sort)))
  finiteMean <- function(x) { mean(x[is.finite(x)]) }
  means = apply(sortedColumns[,1:K+1],1,finiteMean)+.Machine$double.eps;
  
  avg <- function(x,y) (x+y)
  Sig = outer(means,means,avg)/3 + Diff/3 + .Machine$double.eps;
  Sig[Sig <= .Machine$double.eps] = .Machine$double.eps
  #densities = dnorm(Diff,0,sigma*Sig,log = FALSE)
  densities <- exp(-Diff/(sigma*Sig))

  normalize <- function(X)
  {
    D <- apply(X,1,sum)
    Y <- diag(1/sqrt(D))
    Z <- Y%*%X%*%Y
    return (Z)
  }
  W = normalize(densities)
  return(W)
  # 
   # return(densities)
}
