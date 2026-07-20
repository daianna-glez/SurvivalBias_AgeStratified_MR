
library(tidyr)
library(ggplot2)
library(cowplot)
library(sessioninfo)

rm(list = ls())

# ______________________________________________________________________________
#    2.0  Plot results across subscenarios of a varying parameter(s) scenario  
# ______________________________________________________________________________
#  This script plots relevant per-replicate metrics across subscenarios of a
#  varying parameter(s) scenario. 
# ______________________________________________________________________________

input_dir <- paste(getwd(), "02_summarize_and_plot_results", "inputs", sep = "/")
out_dir <- paste(getwd(), "02_summarize_and_plot_results", "outputs", sep = "/")
plot_dir <- paste(getwd(), "02_summarize_and_plot_results", "plots", sep = "/")

## Input dirs
input_dir00 <- paste(getwd(), "00_define_scenarios", "outputs", sep = "/")
input_dir01 <- paste(getwd(), "01_run_replicates_x_subscenario", "outputs", sep = "/")


## Scenarios
all_scenarios <- get(load(paste0(input_dir00, "/scenario.01.Rdata")))
scenarios <- all_scenarios[, c("main_scenario", "main_scenario_val", "sub_sce_varying_par")] %>% unique()

var_labs = list("gammaX" = expression(gamma[X]),
                "gammaA" = expression(gamma[A]), 
                "gammaXA" = expression(gamma[XA]), 
                "gammaX_gammaA" = expression(gamma[X]*", "*gamma[A]),
                "gammaX_gammaXA" = expression(gamma[X]*", "*gamma[XA]),
                "gammaA_gammaXA" = expression(gamma[A]*", "*gamma[XA]),
                "gammaX_gammaA_gammaXA" = expression(gamma[X]*", "*gamma[A]*", "*gamma[XA]))

yvar_labels = list("N_survivors" = "N survivors", 
                   "min_p" = "Min survival prob among survivors",
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
                   "Q" = "Cochran's Q across age strata",
                   "p_het" = expression(p[Q]*" for heterogeneity across age strata")
                   )

metric_to_show_lab = c("mean_min_p" = "Mean min survival probability",
                       "median_min_p" = "Median min survival probability",
                       "mean_Q" = "Mean Cochran's Q statistic",
                       "PR_het" = "Positive Rate for age heterogeneity",
                       "mean_beta_GU" = expression("Mean "*hat(beta)[GU]),
                       "median_beta_GU" = expression("Median "*hat(beta)[GU]))

metric_y = "min_p"
metric_to_show = "mean_min_p"

metric_y = "Q"
metric_to_show = "mean_Q"

metric_y = "p_het"
metric_to_show = "PR_het"

metric_y = "N_survivors"
metric_to_show = NA

metric_y = "N_survivors_sampled"
metric_to_show = NA

metric_y = "beta_GU"
metric_to_show = "mean_beta_GU"
metric_to_show = "median_beta_GU"

metric_y = "beta_GA"
metric_to_show = "mean_beta_GA"
metric_to_show = "median_beta_GA"

metric_y = "beta_AU"
metric_to_show = "mean_beta_AU"
metric_to_show = "median_beta_AU"

metric_y = "beta_XA"
metric_to_show = "mean_beta_XA"
metric_to_show = "median_beta_XA"

metric_y = "beta_GX"
metric_to_show = "mean_beta_GX"
metric_to_show = "median_beta_GX"


## Compare metrics in all replicates across all subscenarios
plot_metric_across_subscenarios <- function(scenario, metric_y, metric_to_show, plot_boxplots = F){
  
  all_res <- get(load(paste(out_dir, paste(scenario, collapse = "/"), "agg_results_subscenarios_replicates.Rdata", sep = "/")))
  all_metrics <- get(load(paste(out_dir, paste(scenario, collapse = "/"), "agg_metrics_subscenarios.Rdata", sep = "/")))
  
  if(scenario["sub_sce_varying_par"] %in% c("gammaX", "gammaA", "gammaXA")){
    all_res$x <- stringr::str_split_i(all_res$sub_sce_varying_par_value, "=", 2)
    all_res$x <- factor(all_res$x, levels = unique(all_res$x))
    all_metrics$x <- stringr::str_split_i(all_metrics$sub_sce_varying_par_value, "=", 2)
  }
  
  ## Plot metric without age stratification
  if(metric_y %in% c("min_p", "Q", "p_het")){
    
    p = ggplot(all_res, aes(x = x, y = get(metric_y))) + 
      geom_point(color='gray', alpha = 0.3, size = 1) +
      theme_classic() +
      labs(x = var_labs[[unlist(scenario["sub_sce_varying_par"])]], 
           y = yvar_labels[[metric_y]], 
          subtitle = metric_to_show_lab[[metric_to_show]]) +
      geom_text(data = all_metrics, aes(x = x,
                                        y = max(all_res[, metric_y]), 
                                    label = signif(get(metric_to_show), digits = 3)), size = 2.4) +
      theme(axis.title.y = element_text(size = 9),
            axis.title.x = element_text(size = 8),
            plot.subtitle = element_text(size = 8),
            axis.text = element_text(size = 7),
            axis.line = element_line(linewidth = 0.4),
            axis.ticks = element_line(linewidth = 0.3))
  }
  
  ## Age-stratified metrics
  else{
    
    ## Age strata
    if(metric_y %in% c("N_survivors", "N_survivors_sampled")){
      groups = c(metric_y, paste0(metric_y, "_", c("30_40", "40_50", "50_60", "60_70", "70_80")))
      scale = "free"
    } 
    else if(metric_y %in% c("beta_GU", "beta_GA", "beta_AU", "beta_XA")){
      groups = paste0(metric_y, c("_survivors", "_sampled", "_sampled_30_40", "_sampled_40_50", "_sampled_50_60", 
                                  "_sampled_60_70", "_sampled_70_80"))
      scale = "fixed"
    }
    else if(metric_y %in% c("beta_GX", "beta_GY", "beta_XY")){
      groups = paste0(metric_y, c("_sampled_all", "_sampled_30_40", "_sampled_40_50", "_sampled_50_60", 
                                  "_sampled_60_70", "_sampled_70_80"))
      scale = "fixed"
    }
    
    ## Expand each replicate's metrics across age strata as rows
    data = all_res[, c("main_scenario", "main_scenario_val", "sub_sce_varying_par", "sub_sce_varying_par_value", 
                       "replicate", "x", groups)] %>% 
      pivot_longer(cols = groups)
    data$metric_y <- metric_y
    data$stratum <- gsub("_", "-", gsub(paste0(metric_y, "_"), "", data$name))
    data$stratum <- factor(data$stratum, levels = unique(data$stratum))
    data$x <- factor(data$x, levels = unique(data$x))
  
    
    ## Plot for 1 varying par
    if(scenario["sub_sce_varying_par"] %in% c("gammaX", "gammaA", "gammaXA")){
      
      p = ggplot(data, aes(x = x, y = value)) + 
        facet_grid(rows = list("stratum"), scales = scale, axes = "all_y") +
        geom_point(color='gray', alpha = 0.3, size = 1) +
        theme_classic() +
        labs(x = var_labs[[unlist(scenario["sub_sce_varying_par"])]], 
             y = yvar_labels[[metric_y]],
             subtitle = metric_to_show_lab[[metric_to_show]]) +
        theme(axis.title.y = element_text(size = 9),
              axis.title.x = element_text(size = 8),
              plot.subtitle = element_text(size = 8),
              axis.text = element_text(size = 7),
              axis.line = element_line(linewidth = 0.4),
              axis.ticks = element_line(linewidth = 0.3))
    }
    
    if(!is.na(metric_to_show)){
      if(metric_y %in% c("beta_GU", "beta_GA", "beta_AU", "beta_XA")){
        groups = paste0(metric_to_show, c("_survivors", "_sampled", "_sampled_30_40", "_sampled_40_50", "_sampled_50_60", 
                                    "_sampled_60_70", "_sampled_70_80"))
      } 
      else if(metric_y %in% c("beta_GX", "beta_GY", "beta_XY")){
        groups = paste0(metric_to_show, c("_sampled_all", "_sampled_30_40", "_sampled_40_50", "_sampled_50_60", 
                                    "_sampled_60_70", "_sampled_70_80"))
      }
      metrics = all_metrics[, c("main_scenario", "main_scenario_val", "sub_sce_varying_par", "sub_sce_varying_par_value", "x", 
                                groups)] %>% 
        pivot_longer(cols =  groups)
      metrics$stratum = gsub("_", "-", gsub(paste0(metric_to_show, "_"), "", metrics$name))
      metrics$stratum = factor(metrics$stratum, levels = levels(data$stratum))
      
      p <- p + geom_text(data = metrics, aes(x = x,
                                             y = max(data$value), 
                                             label = signif(value, digits = 3)), size = 2.4) +
        theme(plot.subtitle = element_text(size = 8))
    }
  }
  
  if(plot_boxplots == T){
    p <- p + geom_boxplot(outliers = F, colour = "gray30", fill = NA, width = 0.1, 
                         linewidth = 0.2, inherit.aes = T) 
  }
  
  ## To show yline
  if(metric_y == "p_het"){yintercept = 0.05}
  else if(metric_y == "MAF_sampled"){yintercept = 0.3}
  else if(metric_y == "beta_GU"){yintercept = 0}
  else if(metric_y == "beta_GA"){yintercept = 0}
  else if(metric_y == "beta_AU"){yintercept = 0}
  else if(metric_y == "beta_XA"){yintercept = 0}
  else if(metric_y == "beta_GX"){yintercept = 0.1}
  else if(metric_y == "beta_XY"){yintercept = 0}
  else{yintercept = NA}
  
  if(length(yintercept) == 1){
    if(!is.na(yintercept)){
      p <- p + geom_hline(yintercept = yintercept, linewidth = 0.5, alpha = 0.8, color = "gray40")
    }
  }
  

  
  
  ## Plot for 2 varying pars
  if(scenario["sub_sce_varying_par"] %in% c("N_r", "N_q1_q2", "N_BGX1", "N_BXY1", "r_q1_q2", "r_BGX1", "r_BXY1", 
                                            "q1_q2_BGX1", "q1_q2_BXY1", "BGX1_BXY1")){
    
    if(!grepl("q1_q2", scenario["sub_sce_varying_par"])){
      xvar <- strsplit(scenario$sub_sce_varying_par, "_")[[1]][1]
      col_var <- strsplit(scenario$sub_sce_varying_par, "_")[[1]][2]
    }
    else{
      xvar = cols_names[1]
      col_var = cols_names[2]
    }
    
    p = ggplot(all_res, aes(x = x, y = get(metric_y))) + 
      geom_point(color='gray', alpha = 0.3, size = 1) +
      theme_classic() +
      facet_grid(.~x2, scales = "fixed", axes = "all_y") +
      labs(x = var_labs[[xvar]], 
           y = yvar_labels[[metric_y]], subtitle = var_labs[[col_var]]) +
      # scale_x_(sec.axis = sec_axis(~ . , name = var_labs[[col_var]], breaks = NULL, labels = NULL)) +
      geom_text(x = 0, y = max(all_res[metric_y])+(max(all_res[metric_y]) - min(all_res[metric_y]))/13.5,
                label = as.character(metric_to_show_lab[metric_to_show]), size = 2.1, parse = T) +
      geom_text(data = all_metrics, aes(x = x, y = max(all_res[metric_y])+(max(all_res[metric_y]) - min(all_res[metric_y]))/25, 
                                        label = signif(get(metric_to_show), digits = 3)), size = 2.1) +
      coord_cartesian(clip = "off", ylim = c(min(all_res[metric_y]), max(all_res[metric_y]))) +
      theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
            strip.background = element_blank(),
            panel.spacing.x = unit(0.25, "cm"),
            strip.text.x = element_text(size = 7, margin = margin(0,0,0.25,0, "cm")),
            axis.title.y = element_text(size = 9),
            axis.title.x = element_text(size = 8),
            axis.text = element_text(size = 7),
            axis.line = element_line(linewidth = 0.4),
            axis.ticks = element_line(linewidth = 0.3), 
            plot.margin = unit(c(1.3, 1, 1, 1), "lines"))
  }
  
  return(p)
}


  

## Plot metrics across replicates, across subscenarios
for(i in 1:nrow(scenarios)){
  
  scenario <- scenarios[i, ]
  plot_dir_sce <- paste(plot_dir, paste(scenario, collapse = "/"), sep = "/")
  dir.create(plot_dir_sce, recursive = T, showWarnings = F)
  width <- c("N" = 17, "r" = 17, "q1_q2" = 22, "BGX1" = 17, "BXY1" = 17, "BUX_BUY" = 17, 
             "N_r" = 27, "N_q1_q2" = 49, "N_BGX1" = 27, "N_BXY1" = 27, "r_q1_q2" = 49,
             "r_BGX1" = 27, "r_BXY1" = 27, "q1_q2_BGX1" = 40, "q1_q2_BXY1" = 49, "BGX1_BXY1" = 27)
  
  ## Plot HWE p-vals and chi-square stats 
  p1 <- plot_metric_across_subscenarios(scenario, "HWE_P_1", "FPR_HWE_1")
  
  
  