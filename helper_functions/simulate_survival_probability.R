
#' @title Simulate survival probability
#' @description This function simulates the outcome for all N individuals.
#' @param data data.frame with columns for exposure, age and confounder across all N individuals.
#' @param cols vector with column names for exposure (X), age (A) and confounder (U).
#' @param gamma_0 baseline survival log-odds. 
#' @param gamma_X exposure effect on survival log-odds.
#' @param gamma_A age effect on survival log-odds.
#' @param gamma_XA interaction term for age and exposure.
#' @param gamma_U confounder effect on survival log-odds.
#' @return A numeric vector containing the probability for all N individuals.  
#' @rdname simulate_survival_probability
#' @export

simulate_survival_probability <- function(data, cols, gamma_0, gamma_X, gamma_A, gamma_XA, gamma_U) {
  
  
  X = data[, cols[1]]
  A = data[, cols[2]]
  U = data[, cols[3]]
  
  # linear predictor
  eta <- gamma_0 + (gamma_X * X) + (gamma_A * A) + (gamma_XA * X * A) + (gamma_U * U)
  
  # inverse logit
  p <- 1 / (1 + exp(-eta))
  
  return(p)
}
