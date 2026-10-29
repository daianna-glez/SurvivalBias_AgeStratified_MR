
library(tidyr)
library(ggplot2)
library(cowplot)
library(sessioninfo)

rm(list = ls())
options(scipen = 999)

# ==============================================================================
#            2. Plot results for subscenarios 02 (βx ≠ 0, βxA = 0)
# ==============================================================================

input_dir <- paste(getwd(), "02_summarize_and_plot_results", "inputs", sep = "/")
out_dir <- paste(getwd(), "02_summarize_and_plot_results", "outputs", sep = "/")
plot_dir <- paste(getwd(), "02_summarize_and_plot_results", "plots", sep = "/")

## Input dirs
input_dir00 <- paste(getwd(), "00_define_scenarios", "outputs", sep = "/")
input_dir01 <- paste(getwd(), "01_run_replicates_x_subscenario", "outputs", sep = "/")


## Scenarios
all_scenarios <- get(load(paste0(input_dir00, "/scenario.02.Rdata")))
scenarios <- all_scenarios[, c("main_scenario", "sub_sce_varying_par")] %>% unique()
main_scenario_vals <- unique(all_scenarios$main_scenario_val)
  
for(j in 1:nrow(scenarios)){
  assign(paste(c("all_res", scenarios[j,]), collapse = "_"), vector())
  assign(paste(c("all_metrics", scenarios[j,]), collapse = "_"), vector())
}


## Aggregate replicates and summarized results across all main scenario values
for (i in 1:nrow(scenarios)) {
  
  main_val_scenarios <- subset(all_scenarios, sub_sce_varying_par == scenarios[i, "sub_sce_varying_par"])
  sub_sce = main_val_scenarios[, c("main_scenario", "main_scenario_val", "sub_sce_varying_par")] %>% unique()
  
  agg_sub_sce_res = vector()
  agg_sub_sce_metrics = vector()
  
  assign(paste(c("agg_res", unique(sub_sce$sub_sce_varying_par)), collapse = "_"), agg_sub_sce_res)
  assign(paste(c("agg_metrics", unique(sub_sce$sub_sce_varying_par)), collapse = "_"), agg_sub_sce_metrics)
  
  ## Results and metrics
  for(j in 1:nrow(sub_sce)){
    
    res <- get(load(paste(out_dir,  paste(sub_sce[j, ], collapse = "/"), "agg_results_subscenarios_replicates.Rdata", sep = "/")))
    metrics <- get(load(paste(out_dir, paste(sub_sce[j, ], collapse = "/"), "agg_metrics_subscenarios.Rdata", sep = "/")))
    
    agg_sub_sce_res <- rbind(agg_sub_sce_res, res)
    agg_sub_sce_metrics <- rbind(agg_sub_sce_metrics, metrics)
  }

  assign(paste(c("agg_res", unique(sub_sce$sub_sce_varying_par)), collapse = "_"), agg_sub_sce_res)
  assign(paste(c("agg_metrics", unique(sub_sce$sub_sce_varying_par)), collapse = "_"), agg_sub_sce_metrics)
}

var_labs = list("gammaX" = expression(gamma[X]),
                "gammaA" = expression(gamma[A]), 
                "gammaXA" = expression(gamma[XA]), 
                "gammaX_gammaA" = expression(gamma[X]*", "*gamma[A]),
                "gammaX_gammaXA" = expression(gamma[X]*", "*gamma[XA]),
                "gammaA_gammaXA" = expression(gamma[A]*", "*gamma[XA]),
                "gammaX_gammaA_gammaXA" = expression(gamma[X]*", "*gamma[A]*", "*gamma[XA]))

yvar_labels = list("N" = "Initial N", 
                   "N_survivors" = "N survivors", 
                   "MAF" = "MAF",
                   "beta_GU" = expression(hat(beta)[GU]), 
                   "se_GU" = expression(se(hat(beta)[GU])), 
                   "p_GU" = expression(p[GU]), 
                   "beta_GA" = expression(hat(beta)[GA]), 
                   "se_GA" = expression(se(hat(beta)[GA])), 
                   "p_GA" = expression(p[GA]), 
                   "beta_AU" = expression(hat(beta)[AU]), 
                   "se_AU" = expression(se(hat(beta)[AU])), 
                   "p_AU" = expression(p[AU]), 
                   "beta_XA" = expression(hat(beta)[XA]), 
                   "se_XA" = expression(se(hat(beta)[XA])), 
                   "p_XA" = expression(p[XA]), 
                   "beta_GX" = expression(hat(beta)[GX]), 
                   "se_GX" = expression(se(hat(beta)[GX])), 
                   "p_GX" = expression(p[GX]), 
                   "beta_GY" = expression(hat(beta)[GY]), 
                   "se_GY" = expression(se(hat(beta)[GY])), 
                   "p_GY" = expression(p[GY]), 
                   "beta_XY" = expression(hat(beta)[XY]), 
                   "se_XY" = expression(se(hat(beta)[XY])), 
                   "p_XY" = expression(p[XY]), 
                   "Q" = "Cochran's Q",
                   "p_het" = expression(p[Q]*" for heterogeneity across age strata"),
                   "p_XA_Y" = expression(p*" for interaction across age strata")
)

metric_to_show_lab = c("mean_N" = "Mean initial N", 
                       "mean_N_survivors" = "Mean survivors N", 
                       "mean_MAF" = "Mean MAF amongst sampled individuals",
                       "mean_Q" = "Mean Cochran's Q statistic",
                       "PR_het" = "Positive rate for age heterogeneity",
                       "PR_interaction" = "Positive rate for X-Age interaction",
                       
                       "mean_beta_GU" = expression("Mean "*hat(beta)[GU]),
                       "median_beta_GU" = expression("Median "*hat(beta)[GU]),
                       "mean_beta_GA" = expression("Mean "*hat(beta)[GA]),
                       "median_beta_GA" = expression("Median "*hat(beta)[GA]),
                       "mean_beta_AU" = expression("Mean "*hat(beta)[AU]),
                       "median_beta_AU" = expression("Median "*hat(beta)[AU]),
                       "mean_beta_XA" = expression("Mean "*hat(beta)[XA]),
                       "median_beta_XA" = expression("Median "*hat(beta)[XA]),
                       "mean_beta_GX" = expression("Mean "*hat(beta)[GX]),
                       "median_beta_GX" = expression("Median "*hat(beta)[GX]),
                       "mean_beta_GY" = expression("Mean "*hat(beta)[GY]),
                       "median_beta_GY" = expression("Median "*hat(beta)[GY]),
                       "mean_beta_XY" = expression("Mean "*hat(beta)[XY]),
                       "median_beta_XY" = expression("Median "*hat(beta)[XY]),
                       
                       "PR_GU" = expression("Positive rate for "*beta[GU]),
                       "PR_GA" = expression("Positive rate for "*beta[GA]),
                       "PR_AU" = expression("Positive rate for "*beta[AU]),
                       "PR_XA" = expression("Positive rate for "*beta[XA]),
                       "PR_GX" = expression("Positive rate for "*beta[GX]),
                       "PR_GY" = expression("Positive rate for "*beta[GY]),
                       "PR_XY" = expression("Positive rate for "*beta[XY]),
                       
                       "mean_se_GU" = expression("Mean "* se(hat(beta)[GU])),
                       "median_se_GU" = expression("Median "* se(hat(beta)[GU])),
                       "mean_se_GA" = expression("Mean "* se(hat(beta)[GA])),
                       "median_se_GA" = expression("Median "* se(hat(beta)[GA])),
                       "mean_se_AU" = expression("Mean "* se(hat(beta)[AU])),
                       "median_se_AU" = expression("Median "* se(hat(beta)[AU])),
                       "mean_se_XA" = expression("Mean "* se(hat(beta)[XA])),
                       "median_se_XA" = expression("Median "* se(hat(beta)[XA])),
                       "mean_se_GX" = expression("Mean "* se(hat(beta)[GX])),
                       "median_se_GX" = expression("Median "* se(hat(beta)[GX])),
                       "mean_se_GY" = expression("Mean "* se(hat(beta)[GY])),
                       "median_se_GY" = expression("Median "* se(hat(beta)[GY])),
                       "mean_se_XY" = expression("Mean "* se(hat(beta)[XY])),
                       "median_se_XY" = expression("Median "* se(hat(beta)[XY]))
)

metric_y_and_to_show = c("N" = "mean_N", 
                         "N_survivors" = "mean_N_survivors",
                         "MAF" = "mean_MAF",
                         "Q" = "mean_Q",
                         "p_het" = "PR_het",
                         "p_XA_Y" = "PR_interaction",
                         "beta_GU" = "median_beta_GU", 
                         "p_GU" = "PR_GU",
                         "se_GU" = "median_se_GU",
                         "beta_GA" = "median_beta_GA", 
                         "p_GA" = "PR_GA",
                         "se_GA" = "median_se_GA",
                         "beta_AU" = "median_beta_AU", 
                         "p_AU" = "PR_AU",
                         "se_AU" = "median_se_AU",
                         "beta_XA" = "median_beta_XA", 
                         "p_XA" = "PR_XA",
                         "se_XA" = "median_se_XA",
                         "beta_GX" = "median_beta_GX", 
                         "p_GX" = "PR_GX",
                         "se_GX" = "median_se_GX",
                         "beta_GY" = "median_beta_GY", 
                         "p_GY" = "PR_GY",
                         "se_GY" = "median_se_GY",
                         "beta_XY" = "median_beta_XY", 
                         "p_XY" = "PR_XY",
                         "se_XY" = "median_se_XY")



## Compare metrics in all replicates across all subscenarios
plot_metric_across_subscenarios <- function(scenario, metric_y, boxplots = F){
  
  metric_to_show = metric_y_and_to_show[metric_y]
  
  ## Results and metrics
  all_res <- get(paste0("agg_res_", scenario$sub_sce_varying_par))
  all_metrics <- get(paste0("agg_metrics_", scenario$sub_sce_varying_par))
  
  ## Age strata
  if(metric_y %in% c(paste0(rep(c("beta", "p", "se"), rep(7, 3)), "_", c("GU", "GA", "AU", "XA", "GX", "GY", "XY")),
                     "MAF")){
    groups = paste0(metric_y, "_survivors", paste0("_", 1:5))
  }
  
  else{
    groups = paste0(metric_y, "")
  }
  
  
  if(!is.na(metric_to_show)){
    if(metric_y == "MAF" | metric_y %in% paste0(rep(c("beta", "p", "se"), rep(7, 3)), "_", c("GU", "GA", "AU", "XA", "GX", "GY", "XY"))){
      groups_metric_to_show = paste0(metric_to_show, "_survivors", paste0("_", 1:5))
    } 
    else{
      groups_metric_to_show = paste0(metric_to_show, "")
    }
  }
  
  
  if(scenario["sub_sce_varying_par"] %in% c("gammaX", "gammaA", "gammaXA")){
    all_res$par1 <- stringr::str_split_i(all_res$sub_sce_varying_par_value, "=", 2)
    all_res$par1 <- factor(all_res$par1, levels = unique(all_res$par1))
    all_metrics$par1 <- stringr::str_split_i(all_metrics$sub_sce_varying_par_value, "=", 2)
    
    ## Expand each replicate's metrics across age strata as rows
    data = all_res[, c("main_scenario", "main_scenario_val", "sub_sce_varying_par", "sub_sce_varying_par_value", 
                       "replicate", "par1", groups)] %>% 
      pivot_longer(cols = groups)
    data$par1 <- factor(data$par1, levels = unique(data$par1))
    
    if(metric_y %in% c("N", "N_survivors", "Q", "p_het", "p_XA_Y")){
      data$stratum <- "all"
    }else{
      data$stratum <- gsub("_", " ", gsub(paste0(metric_y, "_"), "", data$name))
      data$stratum <- factor(data$stratum, levels = unique(data$stratum))
    }
    
    metrics = all_metrics[, c("main_scenario", "main_scenario_val", "sub_sce_varying_par", "sub_sce_varying_par_value", 
                              "par1", groups_metric_to_show)] %>% 
      pivot_longer(cols =  groups_metric_to_show)
    
    metrics$par1 <- factor(metrics$par1, levels = unique(data$par1))
    
    if(metric_y %in% c("N", "N_survivors", "Q", "p_het", "p_XA_Y")){
      metrics$stratum <- "all"
    }else{
      metrics$stratum = gsub("_", " ", gsub(paste0(metric_to_show, "_"), "", metrics$name))
      metrics$stratum = factor(metrics$stratum, levels = levels(data$stratum))
    }
  }
  
  else if(scenario["sub_sce_varying_par"] %in% c("gammaX_gammaA", "gammaX_gammaXA", "gammaA_gammaXA")){
    
    all_res$par1 <- gsub(",.*", "", stringr::str_split_fixed(all_res$sub_sce_varying_par_value, "=", 4)[, 2]) %>% as.character()
    all_res$par2 <- gsub(",.*", "", stringr::str_split_fixed(all_res$sub_sce_varying_par_value, "=", 4)[, 3]) %>% as.character()
    all_res$par1 <- factor(all_res$par1, levels = unique(all_res$par1)) 
    all_res$par2 <- factor(all_res$par2, levels = unique(all_res$par2)) 
    
    all_metrics$par1 <- gsub(",.*", "", stringr::str_split_fixed(all_metrics$sub_sce_varying_par_value, "=", 4)[,2])
    all_metrics$par2 <- gsub(",.*", "", stringr::str_split_fixed(all_metrics$sub_sce_varying_par_value, "=", 4)[,3])
    all_metrics$par1 <- factor(all_metrics$par1, levels = unique(all_res$par1)) 
    all_metrics$par2 <- factor(all_metrics$par2, levels = unique(all_res$par2)) 
    
    data = all_res[, c("main_scenario", "main_scenario_val", "sub_sce_varying_par", "sub_sce_varying_par_value", 
                       "replicate", "par1", "par2", groups)] %>% 
      pivot_longer(cols = groups)
    
    if(metric_y %in% c("N", "N_survivors", "Q", "p_het", "p_XA_Y")){
      data$stratum <- "all"
    }else{
      data$stratum <- gsub("_", " ", gsub(paste0(metric_y, "_"), "", data$name))
      data$stratum <- factor(data$stratum, levels = unique(data$stratum))
    }
    
    metrics = all_metrics[, c("main_scenario", "main_scenario_val", "sub_sce_varying_par", "sub_sce_varying_par_value", 
                              "par1", "par2", groups_metric_to_show)] %>% 
      pivot_longer(cols =  groups_metric_to_show)
  
      if(metric_y %in% c("N", "N_survivors", "Q", "p_het", "p_XA_Y")){
      metrics$stratum <- "all"
    }else{
      metrics$stratum = gsub("_", " ", gsub(paste0(metric_to_show, "_"), "", metrics$name))
      metrics$stratum = factor(metrics$stratum, levels = levels(data$stratum))
    }
  }
  
  else if(scenario["sub_sce_varying_par"] %in% "gammaX_gammaA_gammaXA"){
    all_res$par1 <- stringr::str_split_fixed(stringr::str_split_fixed(all_res$sub_sce_varying_par_value, "=", 4)[,2], ",", 2)[,1] %>% as.character()
    all_res$par2 <- stringr::str_split_fixed(stringr::str_split_fixed(all_res$sub_sce_varying_par_value, "=", 4)[,3], ",", 2)[,1] %>% as.character()
    all_res$par3 <- stringr::str_split_fixed(all_res$sub_sce_varying_par_value, "=", 4)[,4] %>% as.character()
    all_res$par1 <- factor(all_res$par1, levels = unique(all_res$par1)) 
    all_res$par2 <- factor(all_res$par2, levels = unique(all_res$par2)) 
    all_res$par3 <- factor(all_res$par3, levels = unique(all_res$par3)) 
    
    all_metrics$par1 <- stringr::str_split_fixed(stringr::str_split_fixed(all_metrics$sub_sce_varying_par_value, "=", 4)[,2], ",", 2)[,1]
    all_metrics$par2 <- stringr::str_split_fixed(stringr::str_split_fixed(all_metrics$sub_sce_varying_par_value, "=", 4)[,3], ",", 2)[,1]
    all_metrics$par3 <- stringr::str_split_fixed(all_metrics$sub_sce_varying_par_value, "=", 4)[,4] 
    all_metrics$par1 <- factor(all_metrics$par1, levels = unique(all_res$par1)) 
    all_metrics$par2 <- factor(all_metrics$par2, levels = unique(all_res$par2)) 
    all_metrics$par3 <- factor(all_metrics$par3, levels = unique(all_res$par3)) 
    
    data = all_res[, c("main_scenario", "main_scenario_val", "sub_sce_varying_par", "sub_sce_varying_par_value", 
                       "replicate", "par1", "par2", "par3", groups)] %>% 
      pivot_longer(cols = groups)
    
    if(metric_y %in% c("N", "N_survivors", "Q", "p_het", "p_XA_Y")){
      data$stratum <- "all"
    }else{
      data$stratum <- gsub("_", " ", gsub(paste0(metric_y, "_"), "", data$name))
      data$stratum <- factor(data$stratum, levels = unique(data$stratum))
    }
    
    metrics = all_metrics[, c("main_scenario", "main_scenario_val", "sub_sce_varying_par", "sub_sce_varying_par_value", 
                              "par1", "par2", "par3", groups_metric_to_show)] %>% 
      pivot_longer(cols =  groups_metric_to_show)
    if(metric_y %in% c("N", "N_survivors", "Q", "p_het", "p_XA_Y")){
      metrics$stratum <- "all"
    }else{
      metrics$stratum = gsub("_", " ", gsub(paste0(metric_to_show, "_"), "", metrics$name))
      metrics$stratum = factor(metrics$stratum, levels = levels(data$stratum))
    }
  }
  
  ## Plots with boxplots
  if(boxplots == T){
    
    ## Plot for single par
    if(scenario["sub_sce_varying_par"] %in% c("gammaX", "gammaA", "gammaXA")){
      
      p = ggplot(data, aes(x = par1, y = value, colour = stratum)) + 
        geom_boxplot(outliers = F, width = 0.4, linewidth = 0.12, fill = NA, alpha = 0.9,
                     position = position_dodge(width = 0.8), show.legend = F) +
        geom_point(data = metrics, aes(colour = stratum, group = stratum), alpha = 1, size = 1.1,
                   position = position_dodge(width = 0.8)) +
        facet_grid(cols = vars(main_scenario_val), scales = "fixed") + 
        theme_classic() +
        scale_color_manual(values = c("survivors 1" = "#C1CDC1", 
                                      "survivors 2" = "#EEAEEE",
                                      "survivors 3" = "#43CD80", 
                                      "survivors 4" = "#1E90FF", 
                                      "survivors 5" = "#EE4000")) +
        labs(x = var_labs[[unlist(scenario["sub_sce_varying_par"])]], 
             y = yvar_labels[[metric_y]],
             subtitle = expression(beta[X])) +
        theme(axis.title.y = element_text(size = 9),
              axis.title.x = element_text(size = 9),
              plot.subtitle = element_text(size = 9, hjust = 0.5),
              axis.text = element_text(size = 7),
              axis.line = element_line(linewidth = 0.4),
              axis.ticks = element_line(linewidth = 0.3))
    }
    
    ## Plot for two pars
    else if(scenario["sub_sce_varying_par"] %in% c("gammaX_gammaA", "gammaX_gammaXA", "gammaA_gammaXA")){
      
      p = ggplot(data, aes(x = par1, y = value, colour = stratum)) + 
        # geom_line(data = metrics, aes(colour = stratum, group = stratum), linewidth = 0.5, alpha = 0.5) +
        geom_boxplot(outliers = F, width = 0.4, linewidth = 0.12, fill = NA, alpha = 0.9,
                      position = position_dodge(width = 0.8), show.legend = F) +
        geom_point(data = metrics, aes(colour = stratum, group = stratum), alpha = 1, size = 1.1,
                   position = position_dodge(width = 0.8)) +
        facet_grid(rows = vars(par2), cols = vars(main_scenario_val), scales = "fixed") + 
        theme_classic() +
        scale_color_manual(values = c("survivors 1" = "#C1CDC1", 
                                      "survivors 2" = "#EEAEEE",
                                      "survivors 3" = "#43CD80", 
                                      "survivors 4" = "#1E90FF", 
                                      "survivors 5" = "#EE4000")) +
        labs(x = var_labs[[stringr::str_split_i(scenario["sub_sce_varying_par"], "_", 1)]], 
             y = yvar_labels[[metric_y]], 
             title = expression(beta[X]), 
             caption = var_labs[[stringr::str_split_i(scenario["sub_sce_varying_par"], "_", 2)]])  +
        theme(axis.title.y = element_text(size = 10),
              axis.title.x = element_text(size = 9),
              plot.title = element_text(size = 10, hjust = 0.5),
              plot.caption = element_text(size = 10, hjust = 1.05, vjust = 11),
              axis.text = element_text(size = 7),
              axis.line = element_line(linewidth = 0.4),
              axis.ticks = element_line(linewidth = 0.3))
    }
    
    else if(scenario["sub_sce_varying_par"] %in% c("gammaX_gammaA_gammaXA")){
      
      p = ggplot(data, aes(x = par1, y = value, colour = stratum)) + 
        geom_boxplot(outliers = F, width = 0.4, linewidth = 0.12, fill = NA, alpha = 0.9,
                     position = position_dodge(width = 0.8), show.legend = F) +
        geom_point(data = metrics, aes(colour = stratum, group = stratum), alpha = 1, size = 1.1,
                   position = position_dodge(width = 0.8)) +
        facet_grid(rows = vars(par2), cols = vars(par3, main_scenario_val), scales = "fixed", ) + 
        theme_classic() +
        scale_color_manual(values = c("survivors 1" = "#C1CDC1", 
                                      "survivors 2" = "#EEAEEE",
                                      "survivors 3" = "#43CD80", 
                                      "survivors 4" = "#1E90FF", 
                                      "survivors 5" = "#EE4000")) +
        labs(x = var_labs[[stringr::str_split_i(scenario["sub_sce_varying_par"], "_", 1)]], 
             y = yvar_labels[[metric_y]], 
             caption = var_labs[[stringr::str_split_i(scenario["sub_sce_varying_par"], "_", 2)]],
             title = var_labs[[stringr::str_split_i(scenario["sub_sce_varying_par"], "_", 3)]],
             subtitle = expression(beta[X]))  +
        theme(axis.title.y = element_text(size = 10),
              axis.title.x = element_text(size = 9),
              plot.title = element_text(size = 10, hjust = 0.5),
              plot.subtitle = element_text(size = 10, hjust = 0.5),
              plot.caption = element_text(size = 10, hjust = 1.05, vjust = 11),
              axis.text = element_text(size = 7),
              axis.line = element_line(linewidth = 0.4),
              axis.ticks = element_line(linewidth = 0.3))
    }
    
  } 
  
  else{
    ## Plot for single par
    if(scenario["sub_sce_varying_par"] %in% c("gammaX", "gammaA", "gammaXA")){
      
      p = ggplot(data, aes(x = par1, y = value, colour = stratum)) + 
        geom_line(data = metrics, aes(colour = stratum, group = stratum), linewidth = 0.5, alpha = 0.5) +
        geom_point(data = metrics, aes(colour = stratum, group = stratum), alpha = 1, size = 1.1) +
        facet_grid(cols = vars(main_scenario_val), scales = "fixed") + 
        theme_classic() +
        scale_color_manual(values = c("survivors 1" = "#C1CDC1", 
                                      "survivors 2" = "#EEAEEE",
                                      "survivors 3" = "#43CD80", 
                                      "survivors 4" = "#1E90FF", 
                                      "survivors 5" = "#EE4000")) +
        labs(x = var_labs[[unlist(scenario["sub_sce_varying_par"])]], 
             y = yvar_labels[[metric_y]],
             subtitle = expression(beta[X])) +
        theme(axis.title.y = element_text(size = 9),
              axis.title.x = element_text(size = 9),
              plot.subtitle = element_text(size = 9, hjust = 0.5),
              axis.text = element_text(size = 7),
              axis.line = element_line(linewidth = 0.4),
              axis.ticks = element_line(linewidth = 0.3))
    }
    
    ## Plot for two pars
    else if(scenario["sub_sce_varying_par"] %in% c("gammaX_gammaA", "gammaX_gammaXA", "gammaA_gammaXA")){
      
      p = ggplot(data, aes(x = par1, y = value, colour = stratum)) + 
        geom_line(data = metrics, aes(colour = stratum, group = stratum), linewidth = 0.5, alpha = 0.5) +
        geom_point(data = metrics, aes(colour = stratum, group = stratum), alpha = 1, size = 1.1) +
        facet_grid(rows = vars(par2), cols = vars(main_scenario_val), scales = "fixed") + 
        theme_classic() +
        scale_color_manual(values = c("survivors 1" = "#C1CDC1", 
                                      "survivors 2" = "#EEAEEE",
                                      "survivors 3" = "#43CD80", 
                                      "survivors 4" = "#1E90FF", 
                                      "survivors 5" = "#EE4000")) +
        labs(x = var_labs[[stringr::str_split_i(scenario["sub_sce_varying_par"], "_", 1)]], 
             y = yvar_labels[[metric_y]], 
             title = expression(beta[X]), 
             caption = var_labs[[stringr::str_split_i(scenario["sub_sce_varying_par"], "_", 2)]])  +
        theme(axis.title.y = element_text(size = 10),
              axis.title.x = element_text(size = 9),
              plot.title = element_text(size = 10, hjust = 0.5),
              plot.caption = element_text(size = 10, hjust = 1.05, vjust = 11),
              axis.text = element_text(size = 7),
              axis.line = element_line(linewidth = 0.4),
              axis.ticks = element_line(linewidth = 0.3))
    }
    
    else if(scenario["sub_sce_varying_par"] %in% c("gammaX_gammaA_gammaXA")){
      
      p = ggplot(data, aes(x = par1, y = value, colour = stratum)) + 
        geom_line(data = metrics, aes(colour = stratum, group = stratum), linewidth = 0.5, alpha = 0.5) +
        geom_point(data = metrics, aes(colour = stratum, group = stratum), alpha = 1, size = 1.1) +
        facet_grid(rows = vars(par2), cols = vars(par3, main_scenario_val), scales = "fixed", ) + 
        theme_classic() +
        scale_color_manual(values = c("survivors 1" = "#C1CDC1", 
                                      "survivors 2" = "#EEAEEE",
                                      "survivors 3" = "#43CD80", 
                                      "survivors 4" = "#1E90FF", 
                                      "survivors 5" = "#EE4000")) +
        labs(x = var_labs[[stringr::str_split_i(scenario["sub_sce_varying_par"], "_", 1)]], 
             y = yvar_labels[[metric_y]], 
             caption = var_labs[[stringr::str_split_i(scenario["sub_sce_varying_par"], "_", 2)]],
             title = var_labs[[stringr::str_split_i(scenario["sub_sce_varying_par"], "_", 3)]],
             subtitle = expression(beta[X]))  +
        theme(axis.title.y = element_text(size = 10),
              axis.title.x = element_text(size = 9),
              plot.title = element_text(size = 10, hjust = 0.5),
              plot.subtitle = element_text(size = 10, hjust = 0.5),
              plot.caption = element_text(size = 10, hjust = 1.05, vjust = 11),
              axis.text = element_text(size = 7),
              axis.line = element_line(linewidth = 0.4),
              axis.ticks = element_line(linewidth = 0.3))
    }
  }

  ## To show yline
  if(grepl("^p_", metric_y)){yintercept = 0.05}
  else if(metric_y == "MAF"){yintercept = 0.3}
  else if(metric_y == "beta_GU"){yintercept = 0}
  else if(metric_y == "beta_GA"){yintercept = 0}
  else if(metric_y == "beta_AU"){yintercept = 0}
  else if(metric_y == "beta_XA"){yintercept = 0}
  else if(metric_y == "beta_GX"){yintercept = unique(all_res$alpha_G)}
  else if(metric_y == "beta_GY"){yintercept = NA}
  else if(metric_y == "beta_XY"){yintercept = unique(all_res$beta_X)}
  else if(grepl("se", metric_y)){yintercept = 0}
  else{yintercept = NA}
  
  if(length(yintercept) == 1){
    if(!is.na(yintercept)){
      p <- p + geom_hline(yintercept = yintercept, linewidth = 0.3, alpha = 0.8, color = "gray40", linetype = 2)
    }
  } else{
    yintercept_df <- data.frame("main_scenario_val" = yintercept)
    p <- p + geom_hline(data = yintercept_df, aes(yintercept = main_scenario_val), 
                        linewidth = 0.3, alpha = 0.9, color = "gray20", linetype = 2)
  }
  
  return(p)
}



## Plot metrics across replicates, across subscenarios
for(i in 1:nrow(scenarios)){
  
  scenario <- scenarios[i, ]
  
  plots <- list()
  j = 1
  for(metric in names(metric_y_and_to_show)){
    plots[[j]] <- plot_metric_across_subscenarios(scenario, metric, boxplots = F)
    j = j + 1
  }
  
  plot_dir_sce <- paste(plot_dir, paste(scenario, collapse = "/"), sep = "/")
  dir.create(plot_dir_sce, recursive = T, showWarnings = F)
  plot_grid(plotlist = plots, ncol = 6, align = "vh")
  if(i < 4){
    ggsave(filename = paste0(plot_dir_sce, "/metrics_across_subscenario_values.pdf"), width = 49, height = 14)
  } else if(i %in% 4:6){
    ggsave(filename = paste0(plot_dir_sce, "/metrics_across_subscenario_values.pdf"), width = 49, height = 30)
  } else{
    ggsave(filename = paste0(plot_dir_sce, "/metrics_across_subscenario_values.pdf"), width = 49, height = 40)
  }
  
  ## Showing boxplots
  plots <- list()
  j = 1
  for(metric in names(metric_y_and_to_show)){
    plots[[j]] <- plot_metric_across_subscenarios(scenario, metric, boxplots = T)
    j = j + 1
  }
  plot_grid(plotlist = plots, ncol = 6, align = "vh")
  if(i < 4){
    ggsave(filename = paste0(plot_dir_sce, "/metrics_across_subscenario_values_with_boxplots.pdf"), width = 49, height = 14)
  } else if(i %in% 4:6){
    ggsave(filename = paste0(plot_dir_sce, "/metrics_across_subscenario_values_with_boxplots.pdf"), width = 49, height = 30)
  } else{
    ggsave(filename = paste0(plot_dir_sce, "/metrics_across_subscenario_values_with_boxplots.pdf"), width = 49, height = 40)
  }
  
}







## Reproducibility info
session_info()

# ─ Session info ────────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16)
# os       Rocky Linux 8.10 (Green Obsidian)
# system   x86_64, linux-gnu
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       GB
# date     2026-07-22
# rstudio  2023.09.1+494 Desert Sunflower (server)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────────────
# !  package     * version date (UTC) lib source
# cli           3.6.5   2025-04-23 [1] CRAN (R 4.3.1)
# colorspace    2.1-1   2024-07-26 [2] CRAN (R 4.3.1)
# cowplot     * 1.1.3   2024-01-22 [2] CRAN (R 4.3.1)
# crayon        1.5.3   2024-06-20 [2] CRAN (R 4.3.1)
# VP dplyr         1.1.4   2026-04-03 [2] CRAN (R 4.3.1) (on disk 1.2.1)
# farver        2.1.2   2024-05-13 [2] CRAN (R 4.3.1)
# generics      0.1.3   2022-07-05 [2] CRAN (R 4.3.1)
# ggplot2     * 3.5.1   2024-04-23 [2] CRAN (R 4.3.1)
# glue          1.8.0   2024-09-30 [2] CRAN (R 4.3.1)
# gtable        0.3.6   2024-10-25 [2] CRAN (R 4.3.1)
# labeling      0.4.3   2023-08-29 [2] CRAN (R 4.3.1)
# VP lifecycle     1.0.4   2026-01-08 [2] CRAN (R 4.3.1) (on disk 1.0.5)
# magrittr      2.0.3   2022-03-30 [2] CRAN (R 4.3.1)
# munsell       0.5.1   2024-04-01 [2] CRAN (R 4.3.1)
# pillar        1.10.1  2025-01-07 [2] CRAN (R 4.3.1)
# pkgconfig     2.0.3   2019-09-22 [2] CRAN (R 4.3.1)
# purrr         1.0.4   2025-02-05 [2] CRAN (R 4.3.1)
# R6            2.6.1   2025-02-15 [2] CRAN (R 4.3.1)
# ragg          1.3.3   2024-09-11 [2] CRAN (R 4.3.1)
# VP rlang         1.1.5   2026-04-06 [2] CRAN (R 4.3.1) (on disk 1.2.0)
# rstudioapi    0.17.1  2024-10-22 [2] CRAN (R 4.3.1)
# scales        1.3.0   2023-11-28 [2] CRAN (R 4.3.1)
# sessioninfo * 1.2.2   2021-12-06 [2] CRAN (R 4.3.1)
# stringi       1.8.4   2024-05-06 [2] CRAN (R 4.3.1)
# stringr       1.5.1   2023-11-14 [2] CRAN (R 4.3.1)
# systemfonts   1.3.2   2026-03-05 [1] CRAN (R 4.3.1)
# textshaping   1.0.0   2025-01-20 [2] CRAN (R 4.3.1)
# tibble        3.2.1   2023-03-20 [2] CRAN (R 4.3.1)
# VP tidyr       * 1.3.1   2025-12-19 [2] CRAN (R 4.3.1) (on disk 1.3.2)
# tidyselect    1.2.1   2024-03-11 [2] CRAN (R 4.3.1)
# utf8          1.2.4   2023-10-22 [2] CRAN (R 4.3.1)
# VP vctrs         0.6.5   2026-03-21 [2] CRAN (R 4.3.1) (on disk 0.7.2)
# withr         3.0.2   2024-10-28 [2] CRAN (R 4.3.1)
# 
# [1] /home/dg745/R/x86_64-pc-linux-gnu-library/4.3
# [2] /usr/local/Cluster-Apps/R/4.3.1-icelake/lib64/R/library
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────


