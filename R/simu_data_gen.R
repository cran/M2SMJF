#' Generate simulated data
#' 
#' A function to generate simulated data with two modularities and five clusters
#' 
#' @return A list with two elements, which are the sample-feature matrices from different modality
#' @author Xiaoyao Yin
#' @examples 
#' 
#' data_list <- simu_data_gen()
#' @rdname simu_data_gen
#' @export
simu_data_gen <- function(){
  data1 <- matrix(0,100,100)
  data2 <- matrix(0,100,50)
  ### Data in the first cluster, with mean 10, sd 1 for the first modality and mean 5, sd 1 for the second.
  for (i in 1:20)
  {
    data1[i,] <- rnorm(100,10,1)
    data2[i,] <- rnorm(50,5,1)
  }
  ### Data in the second cluster, with mean 20, sd 1 for the first modality and mean 10, sd 1 for the second.
  for (i in 21:40)
  {
    data1[i,] <- rnorm(100,20,1)
    data2[i,] <- rnorm(50,10,1)
  }
  ### Data in the third cluster, with mean 30, sd 1 for the first modality and mean 15, sd 1 for the second.
  for (i in 41:60)
  {
    data1[i,] <- rnorm(100,30,1)
    data2[i,] <- rnorm(50,15,1)
  }
  ### Data in the fourth cluster, with mean 40, sd 1 for the first modality and mean 20, sd 1 for the second.
  for (i in 61:80)
  {
    data1[i,] <- rnorm(100,40,1)
    data2[i,] <- rnorm(50,20,1)
  }
  ### Data in the fifth cluster, with mean 50, sd 1 for the first modality and mean 30, sd 1 for the second.
  for (i in 81:100)
  {
    data1[i,] <- rnorm(100,50,1)
    data2[i,] <- rnorm(50,30,1)
  }

  data_list <- list(data1,data2)
  return(data_list)
}