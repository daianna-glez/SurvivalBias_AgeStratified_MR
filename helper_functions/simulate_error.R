
#' @title Simulate error
#' @description This function simulates the error term for X and Y across N individuals.
#' @param N Total number of individuals. 
#' @return A numeric vector containing the error for the N individuals.  
#' @rdname simulate_error
#' @export


simulate_error = function(N){
  
  e = rnorm(N, mean = 0, sd = 1)
  return(e)
  
}