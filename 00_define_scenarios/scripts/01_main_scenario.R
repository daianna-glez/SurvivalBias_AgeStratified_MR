library(tidyr)
library(sessioninfo)

rm(list = ls())


# ------------------------------------------------------------------------------
#                 0. Define subscenarios across main scenarios
# ------------------------------------------------------------------------------

## Define dirs
input_dir <- paste(getwd(), "00_define_scenarios", "inputs", sep = "/")
out_dir <- paste(getwd(), "00_define_scenarios", "outputs", sep = "/")
plot_dir <- paste(getwd(), "00_define_scenarios", "plots", sep = "/")

dir.create(input_dir, recursive = T, showWarnings = F)
dir.create(out_dir, recursive = T, showWarnings = F)
dir.create(plot_dir, recursive = T, showWarnings = F)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#  Base model: αɢ = 0.1, αu = 0.3, βu = 0.3, βA = 0.1, βx = 0.2, βxA = 0, 
#               γo = 2, γx = -0.2, γA = -0.1, γxA = 0, γu = -0.1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
alpha_G = 0.1
alpha_U = 0.3
beta_U = 0.3
beta_A = 0.1
beta_X = 0.2
beta_XA = 0
gamma_0 = 2
gamma_X = -0.2
gamma_A = -0.1
gamma_XA = 0
gamma_U = -0.1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

## Define ranges for varying parameters

beta_Xs = c(-0.2, 0, 0.2)
beta_XAs = c(-0.1, 0, 0.1)

alpha_Us = c(-0.4,-0.2, 0, 0.2, 0.4)
beta_Us = c(-0.4,-0.2, 0, 0.2, 0.4)

gamma_Us = c(-0.4,-0.2, 0, 0.2, 0.4)
gamma_Xs = c(-0.4, -0.2, 0, 0.2, 0.4)
gamma_XAs = c(-0.2,-0.1,0, 0.1, 0.2)

# ==============================================================================
#         Scenario 1: No exposure effect on the outcome βx = 0, βxA = 0 
# ==============================================================================
scenarios01 <- vector()
main_scenario =  "scenario.01"
main_scenario_val = ""

beta_X = 0
beta_XA = 0

## Sub-scenarios for confounder effects on X and Y (αu, βu)
# ______________________________________________________________________________
#  1. Varying αu = {-0.4,-0.3,-0.2,-0.1,0},  
#             βu = {-0.4,-0.3,-0.2,-0.1,0} 
# ______________________________________________________________________________
sub_sce_varying_par = "alphaU_betaU"

for(au in alpha_Us){
  for(bu in beta_Us){
  scenarios01 <- rbind(scenarios01, c("main_scenario" = main_scenario, "main_scenario_val" = main_scenario_val, 
                                      "sub_sce_varying_par" = sub_sce_varying_par, "sub_sce_varying_par_value" = paste0("alphaU=", au, ",betaU=", bu), 
                                      "alpha_G" = alpha_G, "alpha_U" = au, "beta_U" = bu, "beta_A" = beta_A, 
                                      "beta_X" = beta_X, "beta_XA" = beta_XA, "gamma_0" = gamma_0, "gamma_X" = gamma_X, 
                                      "gamma_A" = gamma_A, "gamma_XA" = gamma_XA, "gamma_U" = gamma_U)) %>% as.data.frame()
  }
}
## Sub-scenarios for effects on S (γx, γxA, γu)
# ______________________________________________________________________________
#  2. Varying γx =  {-0.4, -0.2, 0, 0.2, 0.4},
#             γxA = {-0.2, -0.1, 0, 0.1, 0.2}, 
#             γu =  {-0.4, -0.2, 0, 0.2, 0.4}
# ______________________________________________________________________________
sub_sce_varying_par = "gammaX_gammaXA_gammaU"

for (gx in gamma_Xs){
  for(gxa in gamma_XAs){
    for(gu in gamma_Us){
      scenarios01 <- rbind(scenarios01, c(main_scenario, main_scenario_val, sub_sce_varying_par, 
                                          paste0("gammaX=", gx, ",gammaXA=", gxa, "gammaU=", gu), 
                                          alpha_G, alpha_U, beta_U, beta_A, beta_X, beta_XA, 
                                          gamma_0, "gamma_X" = gx, gamma_A, "gamma_XA" = gxa, "gamma_U" = gu)) %>% as.data.frame()
    }
  }
}

## Sub-scenarios for confounder effects and effects on S (γx, γxA, γu) 
# __________________________________________________________________________________________________________
#  3. Varying αu = {-0.4,-0.3,-0.2,-0.1,0},  
#             βu = {-0.4,-0.3,-0.2,-0.1,0} 
#             γx =  {-0.4, -0.2, 0, 0.2, 0.4},
#             γxA = {-0.2, -0.1, 0, 0.1, 0.2}, 
#             γu =  {-0.4, -0.2, 0, 0.2, 0.4}
# __________________________________________________________________________________________________________

sub_sce_varying_par = "alphaU_betaU_gammaX_gammaXA_gammaU"

for (gx in gamma_Xs){
  for(gxa in gamma_XAs){
    for(gu in gamma_Us){
      for (au in alpha_Us){
        for(bu in beta_Us){
          scenarios01 <- rbind(scenarios01, c(main_scenario, main_scenario_val, sub_sce_varying_par, 
                                              paste0("alphaU=", au, ",betaU=", bu, "gammaX=", gx, ",gammaXA=", gxa, "gammaU=", gu), 
                                              alpha_G, au, bu, beta_A, beta_X, beta_XA, 
                                              gamma_0, gx, gamma_A, gxa, gu)) %>% as.data.frame()
        }
      }
    }
  }
}


scenarios01[, 5:15] <- apply(scenarios01[, 5:15] , 2, as.numeric)
save(scenarios01, file = paste0(out_dir, "/scenario.01.Rdata"))

scenarios01_table <- scenarios01
scenarios01_table$main_scenario_val <- NA
write.table(scenarios01_table, file = paste0(out_dir, "/scenarios.01.csv"), col.names = F, row.names = F, quote = F, na = "NA", sep = "\t")







## Reproducibility info
session_info()

# ─ Session info ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16)
# os       Rocky Linux 8.10 (Green Obsidian)
# system   x86_64, linux-gnu
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       GB
# date     2026-05-23
# rstudio  2023.09.1+494 Desert Sunflower (server)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# package     * version date (UTC) lib source
# cli           3.6.5   2025-04-23 [1] CRAN (R 4.3.1)
# colorspace    2.1-1   2024-07-26 [2] CRAN (R 4.3.1)
# cowplot     * 1.1.3   2024-01-22 [2] CRAN (R 4.3.1)
# dplyr       * 1.2.1   2026-04-03 [1] CRAN (R 4.3.1)
# generics      0.1.3   2022-07-05 [2] CRAN (R 4.3.1)
# ggplot2     * 3.5.1   2024-04-23 [2] CRAN (R 4.3.1)
# glue          1.8.0   2024-09-30 [2] CRAN (R 4.3.1)
# gtable        0.3.6   2024-10-25 [2] CRAN (R 4.3.1)
# lifecycle     1.0.5   2026-01-08 [1] CRAN (R 4.3.1)
# magrittr      2.0.3   2022-03-30 [2] CRAN (R 4.3.1)
# munsell       0.5.1   2024-04-01 [2] CRAN (R 4.3.1)
# pillar        1.10.1  2025-01-07 [2] CRAN (R 4.3.1)
# pkgconfig     2.0.3   2019-09-22 [2] CRAN (R 4.3.1)
# purrr         1.0.4   2025-02-05 [2] CRAN (R 4.3.1)
# R6            2.6.1   2025-02-15 [2] CRAN (R 4.3.1)
# rlang         1.2.0   2026-04-06 [1] CRAN (R 4.3.1)
# rstudioapi    0.17.1  2024-10-22 [2] CRAN (R 4.3.1)
# scales        1.3.0   2023-11-28 [2] CRAN (R 4.3.1)
# sessioninfo * 1.2.2   2021-12-06 [2] CRAN (R 4.3.1)
# tibble        3.2.1   2023-03-20 [2] CRAN (R 4.3.1)
# tidyr       * 1.3.1   2024-01-24 [2] CRAN (R 4.3.1)
# tidyselect    1.2.1   2024-03-11 [2] CRAN (R 4.3.1)
# vctrs         0.7.2   2026-03-21 [1] CRAN (R 4.3.1)
# withr         3.0.2   2024-10-28 [2] CRAN (R 4.3.1)
# 
# [1] /home/dg745/R/x86_64-pc-linux-gnu-library/4.3
# [2] /usr/local/Cluster-Apps/R/4.3.1-icelake/lib64/R/library
# 
# ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

