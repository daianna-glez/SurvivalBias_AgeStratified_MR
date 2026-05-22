
#' @title Simulate genotype data 
#' @description This function simulates genotypes in Hardy-Weinberg equilibrium for a bi-allelic SNP of MAF q across N individuals.
#' @param N Number of individuals. 
#' @param q MAF of the SNP.
#' @return A numeric vector containing the minor allele dosages for all N individuals.  
#' @rdname simulate_genotype
#' @export


simulate_genotype = function(N, q){
  
  G = rbinom(n = N, 
             size = 2, 
             prob = q)
  return(G)
  
}
