#' calculate the normalized mutual information.
#' 
#' calculate the normalized mutual information of two vectors x and y.
#' 
#' @param x A vector 
#' 
#' @param y A vector as long as x
#' 
#' @return A number between 0 and 1 indicating the normalized mutual information
#' @author Xiaoyao Yin
#' @examples 
#' 
#' x <- c(0.1,0.2,0.3,0.4)
#' y <- c(0.1,0.2,0.3,0.4)
#' NMI <- Cal_NMI(x,y)
#' @rdname Cal_NMI
#' @export
Cal_NMI <- function(x, y) {
	x = as.vector(x)
	y = as.vector(y)
    return(max(0, mutualInformation(x, y)/sqrt(entropy(x) * entropy(y)), na.rm=TRUE))
}

# Calculate the mutual information between vectors x and y.
mutualInformation <- function(x, y) {
  classx <- unique(x)
  classy <- unique(y)
  nx <- length(x)
  ncx <- length(classx)
  ncy <- length(classy)
  
  probxy <- matrix(NA, ncx, ncy)
  for (i in 1:ncx) {
    for (j in 1:ncy) {
      probxy[i, j] <- sum((x == classx[i]) & (y == classy[j])) / nx
    }
  }
  
  probx <- matrix(rowSums(probxy), ncx, ncy)
  proby <- matrix(colSums(probxy), ncx, ncy, byrow=TRUE)
  result <- sum(probxy * log(probxy / (probx * proby), 2), na.rm=TRUE)
  return(result)
}

# Calculate the entropy of vector x.
entropy <- function(x) {
  class <- unique(x)
  nx <- length(x)
  nc <- length(class)
  
  prob <- rep.int(NA, nc)
  for (i in 1:nc) {
    prob[i] <- sum(x == class[i])/nx
  }
  
  result <- -sum(prob * log(prob, 2))
  return(result)
}