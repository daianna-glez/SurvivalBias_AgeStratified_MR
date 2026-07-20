
library(tidyr)
library(sessioninfo)

rm(list = ls())
options(scipen = 999)

# ------------------------------------------------------------------------------
#                  2. Summarize and plot results per subscenario 
# ------------------------------------------------------------------------------

# ______________________________________________________________________________
#    1.0  Compute and aggregate metrics across replicates per subscenario 
# ______________________________________________________________________________
#  This script summarizes results across replicates per subscenario and
#  aggregates results across subscenarios of a varying parameter(s) scenario. 
# ______________________________________________________________________________

input_dir <- paste(getwd(), "02_summarize_and_plot_results", "inputs", sep = "/")
out_dir <- paste(getwd(), "02_summarize_and_plot_results", "outputs", sep = "/")
plot_dir <- paste(getwd(), "02_summarize_and_plot_results", "plots", sep = "/")

dir.create(input_dir, showWarnings = F)
dir.create(out_dir, showWarnings = F)
dir.create(plot_dir, showWarnings = F)

## Input dirs
input_dir00 <- paste(getwd(), "00_define_scenarios", "outputs", sep = "/")
input_dir01 <- paste(getwd(), "01_run_replicates_x_subscenario", "outputs", sep = "/")


## Scenarios
all_scenarios <- get(load(paste0(input_dir00, "/scenario.01.Rdata")))[1:5, ]
# all_scenarios <- get(load(paste0(input_dir00, "/scenario.01.Rdata")))[1881:2632,]
scenarios <- all_scenarios[, c("main_scenario", "main_scenario_val", "sub_sce_varying_par")] %>% unique()
subscenarios <- all_scenarios[, c("main_scenario", "main_scenario_val", "sub_sce_varying_par","sub_sce_varying_par_value")]


## Confirm res file x subscenario first
for(j in 1:nrow(subscenarios)){

  sub_sce = subscenarios[j,]
  scenario_path <- paste(sub_sce, collapse = "/")
  res <- get(load(paste(c(input_dir01, scenario_path, "subscenario_res_across_100replicates.Rdata"), collapse = "/")))
  if(nrow(res)!= 100 | ncol(res) != 237){
    print(ncol(res))
  }
  if(!"subscenario_res_across_100replicates.Rdata" %in% list.files(paste(c(input_dir01, scenario_path), collapse = "/"))){
    print(j)
  }
}

## For agg results across subscenarios
for(j in 1:nrow(scenarios)){
  assign(paste(c("all_res", scenarios[j,]), collapse = "_"), vector())
  assign(paste(c("all_metrics", scenarios[j,]), collapse = "_"), vector())
}


for(j in 1:nrow(subscenarios)){
  
  sub_sce = subscenarios[j,]
  scenario_path <- paste(sub_sce, collapse = "/")
  
  ## Extract results across replicates 
  res <- get(load(paste(c(input_dir01, scenario_path, "subscenario_res_across_100replicates.Rdata"), collapse = "/")))
  
  ## Append with other sub scenarios results
  all_res <- get(paste(c("all_res", sub_sce[-4]), collapse = "_"))
  all_res <- rbind(all_res, res)
  assign(paste(c("all_res", sub_sce[-4]), collapse = "_"), all_res)
  
  ## Compute summary metrics x subscenario across replicates
  subscenario_metrics <- with(res, 
                                ## Mean num of survivors 
                              c("mean_N_survivors" = mean(N_survivors),
                                ## Mean and median min p of survivors
                                "mean_min_p" = mean(min_p),
                                "median_min_p" = median(min_p),
                                ## Mean num of survivors sampled 
                                "mean_N_survivors_sampled" = mean(N_survivors_sampled),
                                ## Mean MAF among sampled survivors
                                "mean_MAF_sampled" = mean(MAF_sampled),
                                ## Mean Q stat
                                "mean_Q" = mean(Q),
                                ## Positive rate for interaction
                                "PR_het" = mean(res$p_het < 0.05))
                              )
  
  ## Num of ALT/ALT per sampled age group
  ## MAF among sampled per age group
  ages <- c("sampled_30_40", "sampled_40_50", "sampled_50_60", "sampled_60_70", "sampled_70_80") 
  for(group in ages){
    subscenario_metrics[paste0("mean_N_hom_ALT_", group)] <- signif(mean(res[, paste0("N_survivors_", group, "_hom_ALT")]), digits = 2)
    subscenario_metrics[paste0("mean_MAF_", group)] <- signif(mean(res[, paste0("MAF_", group)]), digits = 2)
  }
  
  ## Add mean G-U, G-A, A-U, and A-X estimated effects among survivors and sampled survivors 
  for(group in c("survivors", "sampled", ages)){
    
    subscenario_metrics[paste0("mean_beta_GU_", group)] <- signif(mean(res[, paste0("beta_GU_", group)]), digits = 2)
    subscenario_metrics[paste0("median_beta_GU_", group)] <- signif(median(res[, paste0("beta_GU_", group)]), digits = 2)
    subscenario_metrics[paste0("sd_beta_GU_", group)] <- signif(sd(res[, paste0("beta_GU_", group)]), digits = 2)
    subscenario_metrics[paste0("PR_GU_", group)] = mean(res[, paste0("p_GU_", group)] < 0.05)
    
    subscenario_metrics[paste0("mean_beta_GA_", group)] <- signif(mean(res[, paste0("beta_GA_", group)]), digits = 2)
    subscenario_metrics[paste0("median_beta_GA_", group)] <- signif(median(res[, paste0("beta_GA_", group)]), digits = 2)
    subscenario_metrics[paste0("sd_beta_GA_", group)] <- signif(sd(res[, paste0("beta_GA_", group)]), digits = 2)
    subscenario_metrics[paste0("PR_GA_", group)] = mean(res[, paste0("p_GA_", group)] < 0.05)
    
    subscenario_metrics[paste0("mean_beta_AU_", group)] <- signif(mean(res[, paste0("beta_AU_", group)]), digits = 2)
    subscenario_metrics[paste0("median_beta_AU_", group)] <- signif(median(res[, paste0("beta_AU_", group)]), digits = 2)
    subscenario_metrics[paste0("sd_beta_AU_", group)] <- signif(sd(res[, paste0("beta_AU_", group)]), digits = 2)
    subscenario_metrics[paste0("PR_AU_", group)] = mean(res[, paste0("p_AU_", group)] < 0.05)
    
    subscenario_metrics[paste0("mean_beta_XA_", group)] <- signif(mean(res[, paste0("beta_XA_", group)]), digits = 2)
    subscenario_metrics[paste0("median_beta_XA_", group)] <- signif(median(res[, paste0("beta_XA_", group)]), digits = 2)
    subscenario_metrics[paste0("sd_beta_XA_", group)] <- signif(sd(res[, paste0("beta_XA_", group)]), digits = 2)
    subscenario_metrics[paste0("PR_XA_", group)] = mean(res[, paste0("p_XA_", group)] < 0.05)
  }
  
  ## Add mean G-X, G-Y and X-Y estimated effects overall and per age group
  strata <- c("all", "30_40", "40_50", "50_60", "60_70", "70_80") 
  for(group in strata){

    subscenario_metrics[paste0("mean_beta_GX_sampled_", group)] = mean(res[, paste0("beta_GX_sampled_", group)], na.rm = T)
    subscenario_metrics[paste0("median_beta_GX_sampled_", group)] = median(res[, paste0("beta_GX_sampled_", group)], na.rm = T)
    subscenario_metrics[paste0("sd_beta_GX_sampled_", group)] = sd(res[, paste0("beta_GX_sampled_", group)], na.rm = T)

    subscenario_metrics[paste0("mean_beta_GY_sampled_", group)] = mean(res[, paste0("beta_GY_sampled_", group)], na.rm = T)
    subscenario_metrics[paste0("median_beta_GY_sampled_", group)] = median(res[, paste0("beta_GY_sampled_", group)], na.rm = T)
    subscenario_metrics[paste0("sd_beta_GY_sampled_", group)] = sd(res[, paste0("beta_GY_sampled_", group)], na.rm = T)
    
    subscenario_metrics[paste0("mean_beta_XY_sampled_", group)] = mean(res[, paste0("beta_XY_sampled_", group)], na.rm = T)
    subscenario_metrics[paste0("median_beta_XY_sampled_", group)] = median(res[, paste0("beta_XY_sampled_", group)], na.rm = T)
    subscenario_metrics[paste0("sd_beta_XY_sampled_", group)] = sd(res[, paste0("beta_XY_sampled_", group)], na.rm = T)
    
    ## Positive rate for causal effect
    subscenario_metrics[paste0("PR_XY_", group)] = mean(res[, paste0("p_XY_sampled_", group)] < 0.05)
    ## Negative rate for causal effect
    subscenario_metrics[paste0("NR_XY_", group)]  = mean(res[, paste0("p_XY_sampled_", group)] >= 0.05)
  }
  
  save(subscenario_metrics, file = paste(c(input_dir01, scenario_path, "subscenario_summary_metrics.Rdata"), collapse = "/")) 
  
  all_metrics <- get(paste(c("all_metrics", sub_sce[-4]), collapse = "_"))
  to_bind <- data.frame("main_scenario" = sub_sce$main_scenario, 
                        "main_scenario_val" = sub_sce$main_scenario_val, 
                        "sub_sce_varying_par" = sub_sce$sub_sce_varying_par, 
                        "sub_sce_varying_par_value" = sub_sce$sub_sce_varying_par_value, 
                        t(subscenario_metrics))
  all_metrics <- rbind(all_metrics, to_bind)
  assign(paste(c("all_metrics", sub_sce[-4]), collapse = "_"), all_metrics)
}


## Confirm 100 rep x num sub scenarios for each var par scenario agg
for(j in 1:nrow(scenarios)){
  num_sub <- all_scenarios %>% 
    dplyr::filter(main_scenario == scenarios[j,"main_scenario"], 
                  main_scenario_val == scenarios[j,"main_scenario_val"], 
                  sub_sce_varying_par == scenarios[j, "sub_sce_varying_par"]) %>% nrow()
  
  if(nrow(get(paste(c("all_res", scenarios[j,]), collapse = "_"))) != num_sub*100){print(j)}
  if(nrow(get(paste(c("all_metrics", scenarios[j,]), collapse = "_"))) != num_sub){print(j)}
}

## Save agg
for(j in 1:nrow(scenarios)){
  
  sce = scenarios[j,]
  path <- paste(c(out_dir, sce), collapse = "/")
  dir.create(path, recursive = T, showWarnings = F)
  
  save(list = paste(c("all_res", sce), collapse = "_"), file = paste0(path, "/agg_results_subscenarios_replicates.Rdata"))
  save(list = paste(c("all_metrics", sce), collapse = "_"), file = paste0(path, "/agg_metrics_subscenarios.Rdata"))
}







## Reproducibility info
session_info()

# ─ Session info ───────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.1 (2025-06-13)
# os       macOS Monterey 12.7.6
# system   x86_64, darwin20
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       Europe/London
# date     2025-10-07
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# quarto   1.6.42 @ /private/var/folders/j1/klb6dgpn3dxfdrxfmbb4f0jc0000gp/T/AppTranslocation/7D11FDEE-A995-4F90-9AC5-8C647803C12A/d/RStudio.app/Contents/Resources/app/quarto/bin/quarto
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────
# package      * version date (UTC) lib source
# cli            3.6.5   2025-04-23 [1] CRAN (R 4.5.0)
# cowplot      * 1.2.0   2025-07-07 [1] CRAN (R 4.5.1)
# dichromat    * 2.0-0.1 2022-05-02 [1] CRAN (R 4.5.0)
# dplyr        * 1.1.4   2023-11-17 [1] CRAN (R 4.5.0)
# farver         2.1.2   2024-05-13 [1] CRAN (R 4.5.0)
# fs             1.6.6   2025-04-12 [1] CRAN (R 4.5.0)
# generics       0.1.4   2025-05-09 [1] CRAN (R 4.5.0)
# ggplot2      * 4.0.0   2025-09-11 [1] CRAN (R 4.5.1)
# gitcreds       0.1.2   2022-09-08 [1] CRAN (R 4.5.0)
# glue           1.8.0   2024-09-30 [1] CRAN (R 4.5.0)
# gtable         0.3.6   2024-10-25 [1] CRAN (R 4.5.0)
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.5.0)
# lifecycle      1.0.4   2023-11-07 [1] CRAN (R 4.5.0)
# magrittr       2.0.4   2025-09-12 [1] CRAN (R 4.5.1)
# pheatmap     * 1.0.13  2025-06-05 [1] CRAN (R 4.5.0)
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.5.1)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.5.0)
# purrr          1.1.0   2025-07-10 [1] CRAN (R 4.5.1)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.5.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.5.0)
# rlang          1.1.6   2025-04-11 [1] CRAN (R 4.5.0)
# rstudioapi     0.17.1  2024-10-22 [1] CRAN (R 4.5.0)
# S7             0.2.0   2024-11-07 [1] CRAN (R 4.5.0)
# scales         1.4.0   2025-04-24 [1] CRAN (R 4.5.0)
# sessioninfo  * 1.2.3   2025-02-05 [1] CRAN (R 4.5.0)
# stringi        1.8.7   2025-03-27 [1] CRAN (R 4.5.0)
# stringr        1.5.2   2025-09-08 [1] CRAN (R 4.5.1)
# tibble         3.3.0   2025-06-08 [1] CRAN (R 4.5.0)
# tidyr        * 1.3.1   2024-01-24 [1] CRAN (R 4.5.0)
# tidyselect     1.2.1   2024-03-11 [1] CRAN (R 4.5.0)
# usethis      * 3.2.1   2025-09-06 [1] CRAN (R 4.5.1)
# utf8           1.2.6   2025-06-08 [1] CRAN (R 4.5.0)
# vctrs          0.6.5   2023-12-01 [1] CRAN (R 4.5.0)
# withr          3.0.2   2024-10-28 [1] CRAN (R 4.5.0)
# 
# [1] /Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/library
# * ── Packages attached to the search path.
# 
# ──────────────────────────────────────────────────────────────────────────────────────────────────────