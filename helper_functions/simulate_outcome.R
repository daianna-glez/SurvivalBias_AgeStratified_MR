
#' @title Simulate outcome
#' @description This function simulates the outcome for all N individuals.
#' @param data data.frame with columns for exposure, age, confounder and error across all N individuals.
#' @param cols vector with column names for exposure (X), age (A), confounder (U) and error (eY). 
#' @param beta_X exposure effect on Y.
#' @param beta_A age effect on Y.
#' @param beta_XA interaction term for age and exposure.
#' @param beta_U confounder effect on Y.
#' @return A numeric vector containing the outcome for all N individuals.  
#' @rdname simulate_outcome
#' @export

simulate_outcome = function(data, cols, beta_X, beta_A, beta_XA, beta_U){
  
  X = data[, cols[1]]
  A = data[, cols[2]]
  U = data[, cols[3]]
  eY = data[, cols[4]]
 
  Y = ((beta_X + (beta_XA*A))*X) + (beta_A*A) +(beta_U*U) + eY
  return(Y)
}

