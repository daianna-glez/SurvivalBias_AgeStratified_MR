
#' @title Simulate confounder
#' @description This function simulates a confounder (U) of X and Y across N individuals.
#' @param N Total number of individuals. 
#' @return A numeric vector containing the counfounder value for all N individuals.  
#' @rdname simulate_confounder
#' @export


simulate_confounder = function(N){
  
  U = runif(N, min = 0, max = 1)
  return(U)
  
}