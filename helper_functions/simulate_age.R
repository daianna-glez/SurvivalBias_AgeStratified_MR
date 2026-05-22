
#' @title Simulate age
#' @description This function simulates age across N individuals.
#' @param N Total number of individuals. 
#' @param min_age Min age among N individuals.
#' @param max_age Max age among N individuals.
#' @return A numeric vector containing the age of all N individuals.  
#' @rdname simulate_confounder
#' @export


simulate_age = function(min_age, max_age, N){
  
  U = runif(N, min = min_age, max = max_age)
  return(U)
  
}