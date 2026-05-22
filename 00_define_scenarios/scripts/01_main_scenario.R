library(tidyr)
library(dplyr)
library(sessioninfo)

rm(list = ls())


################################################################################
#               Scenario 1: no exposure effect on the outcome 
################################################################################

## Define dirs
input_dir <- paste(getwd(), "00_define_scenarios", "inputs", sep = "/")
out_dir <- paste(getwd(), "00_define_scenarios", "outputs", sep = "/")
plot_dir <- paste(getwd(), "00_define_scenarios", "plots", sep = "/")

dir.create(input_dir, recursive = T, showWarnings = F)
dir.create(out_dir, recursive = T, showWarnings = F)
dir.create(plot_dir, recursive = T, showWarnings = F)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#  Base model: αɢ = 0.1, αu = 0.3, βu = 0.3, βA = 0.1, βx = 0.2, βxA = 0, 
#               γo = -0.85, γx = -0.2, γA = -0.1, γxA = 0, γu = -0.1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
alpha_G = 0.1
alpha_U = 0.3
beta_U = 0.3
beta_A = 0.1
beta_X = 0.2
beta_XA = 0
gamma_0 = -0.85
gamma_X = -0.2
gamma_A = -0.1
gamma_XA = 0
gamma_U = -0.1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

## Define ranges for varying parameters
beta_Xs = c(-0.4, -0.2, 0, 0.2, 0.4)
beta_XAs = c(-0.1, -0.05, 0, 0.05, 0.1)
gamma_Xs = c(-0.3, -0.2, -0.1)
gamma_As = c(-0.3, -0.2, -0.1)
gamma_XAs = c(-0.15, -0.01, -0.05)

# ==============================================================================
#  Scenario 1: No exposure effect on the outcome βx = 0, βxA = 0, 
# ==============================================================================
scenarios01 <- vector()
main_scenario =  "scenario.01"
main_scenario_val = ""

beta_X = 0
beta_XA = 0

## Sub-scenarios for one varying parameter at a time:
# ______________________________________________________________________________
#  1. Varying γx = {-0.3,-0.2,-0.1} 
# ______________________________________________________________________________
sub_sce_varying_par = "gammaX"

for (gx in gamma_Xs){
  scenarios01 <- rbind(scenarios01, c("main_scenario" = main_scenario, "main_scenario_val" = main_scenario_val, 
                                      "sub_sce_varying_par" = sub_sce_varying_par, "sub_sce_varying_par_value" = paste0(sub_sce_varying_par,"=", gx), 
                                      "alpha_G" = alpha_G, "alpha_U" = alpha_U, "beta_U" = beta_U, "beta_A" = beta_A, 
                                      "beta_X" = beta_X, "beta_XA" = beta_XA, "gamma_0" = gamma_0, "gamma_X" = gx, 
                                      "gamma_A" = gamma_A, "gamma_XA" = gamma_XA, "gamma_U" = gamma_U)) %>% as.data.frame()
}
# ______________________________________________________________________________
#  2. Varying γA = {-0.3,-0.2,-0.1}
# ______________________________________________________________________________
sub_sce_varying_par = "gammaA"

for (ga in gamma_As){
  scenarios01 <- rbind(scenarios01, c(main_scenario, main_scenario_val, sub_sce_varying_par, paste0(sub_sce_varying_par,"=", ga), 
                                      alpha_G, alpha_U, beta_U, beta_A, beta_X, beta_XA, 
                                      gamma_0, gamma_X, "gamma_A" = ga, gamma_XA, gamma_U)) %>% as.data.frame()
}
# __________________________________________________________________________________________________________
#  3. Varying γxA = {-0.15,-0.1,-0.05}
# __________________________________________________________________________________________________________
sub_sce_varying_par = "gammaXA"
for (gxa in gamma_XAs){
  scenarios01 <- rbind(scenarios01, c(main_scenario, main_scenario_val, sub_sce_varying_par, paste0(sub_sce_varying_par,"=", gxa), 
                                      alpha_G, alpha_U, beta_U, beta_A, beta_X, beta_XA, 
                                      gamma_0, gamma_X, gamma_A, "gamma_XA" = gxa, gamma_U)) %>% as.data.frame()
}



#  Sub-scenarios for two varying parameters at a time:  
# ______________________________________________________________________________________________
#  4. Varying γx = {-0.3,-0.2,-0.1},
#             γA = {-0.3,-0.2,-0.1}
# ______________________________________________________________________________________________
sub_sce_varying_par = "gammaX_gammaA"
for (ga in gamma_As){
  for (gx in gamma_Xs){
    scenarios01 <- rbind(scenarios01, c(main_scenario, main_scenario_val, sub_sce_varying_par, paste0("gammaX=", gx, ",gammaA=", ga), 
                                        alpha_G, alpha_U, beta_U, beta_A, beta_X, beta_XA, 
                                        gamma_0, "gamma_X" = gx, "gamma_A" = ga, gamma_XA, gamma_U)) %>% as.data.frame()
  }
}
# ______________________________________________________________________________________________
#  5. Varying γx = {-0.3,-0.2,-0.1},
#             γxA = {-0.15,-0.1,-0.05}
# ______________________________________________________________________________________________
sub_sce_varying_par = "gammaX_gammaXA"

for (gxa in gamma_XAs){
  for (gx in gamma_Xs){
    scenarios01 <- rbind(scenarios01, c(main_scenario, main_scenario_val, sub_sce_varying_par, paste0("gammaX=", gx, ",gammaXA=", gxa), 
                                        alpha_G, alpha_U, beta_U, beta_A, beta_X, beta_XA, 
                                        gamma_0, "gamma_X" = gx, gamma_A, "gamma_XA" = gxa, gamma_U)) %>% as.data.frame()
  }
}

# ______________________________________________________________________________________________
#  6. Varying γA = {-0.3,-0.2,-0.1},
#             γxA = {-0.15,-0.1,-0.05}
# ______________________________________________________________________________________________
sub_sce_varying_par = "gammaA_gammaXA"

for (gxa in gamma_XAs){
  for (ga in gamma_As){
    scenarios01 <- rbind(scenarios01, c(main_scenario, main_scenario_val, sub_sce_varying_par, paste0("gammaA=", ga, ",gammaXA=", gxa), 
                                        alpha_G, alpha_U, beta_U, beta_A, beta_X, beta_XA, 
                                        gamma_0, gamma_X, "gamma_A" = ga, "gamma_XA" = gxa, gamma_U)) %>% as.data.frame()
  }
}


#  Sub-scenarios for three varying parameters at a time:  
# ______________________________________________________________________________________________
#  7. Varying γx = {-0.3,-0.2,-0.1},
#             γA = {-0.3,-0.2,-0.1},
#             γxA = {-0.15,-0.1,-0.05}
# ______________________________________________________________________________________________
sub_sce_varying_par = "gammaX_gammaA_gammaXA"
for(gxa in gamma_XAs){
  for (ga in gamma_As){
    for (gx in gamma_Xs){
      scenarios01 <- rbind(scenarios01, c(main_scenario, main_scenario_val, sub_sce_varying_par, 
                                          paste0("gammaX=", gx, ",gammaA=", ga, ",gammaXA=", gxa), 
                                          alpha_G, alpha_U, beta_U, beta_A, beta_X, beta_XA, 
                                          gamma_0, "gamma_X" = gx, "gamma_A" = ga, "gamma_XA" = gxa, gamma_U)) %>% as.data.frame()
    }
  }
}

scenarios01[, 5:15] <- apply(scenarios01[, 5:15] , 2, as.numeric)
save(scenarios01, file = paste0(out_dir, "/scenario.01.Rdata"))




