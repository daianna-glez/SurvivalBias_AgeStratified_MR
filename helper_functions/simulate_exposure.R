
#' @title Simulate log(exposure)
#' @description This function simulates the exposure for all N individuals.
#' @param data data.frame with columns genotype, confounder and error for all N individuals.
#' @param cols vector with column names for genotype (G), confounder (U) and error (eX). 
#' @return A numeric vector containing the exposure for all N individuals.  
#' @rdname simulate_exposure
#' @export


simulate_exposure = function(data, cols, alpha_G, alpha_U){
  
  G = data[, cols[1]]
  U = data[, cols[2]]
  eX = data[, cols[3]]
  
  logX = (alpha_G*G) + (alpha_U*U) + eX
  X = exp(logX)
  
  return(X)
}