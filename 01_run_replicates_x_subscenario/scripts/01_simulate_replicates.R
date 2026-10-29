#!/usr/bin/env Rscript

library(tidyr, quietly = T, warn.conflicts = F)
library(ggplot2, quietly = T, warn.conflicts = F)
library(cowplot, quietly = T, warn.conflicts = F)
library(sessioninfo, quietly = T, warn.conflicts = F)
library(metafor, quietly = T, warn.conflicts = F)
rm(list = ls())

# ------------------------------------------------------------------------------
#                   1. Run simulation replicates per subscenario 
# ------------------------------------------------------------------------------
#  This script takes a subscenario parameters and simulates individual level
#  data across 500,000 individuals. It then selects "survivors" and 
#  estimates the MR ratio estimate among all survivors and across equally 
#  sized age strata. This is repeated 1,000 times per subscenario.
# ______________________________________________________________________________

## Run for task ID (= subscenario row num)
id <- commandArgs(trailingOnly = TRUE)[1]
main <- commandArgs(trailingOnly = TRUE)[2]
# id = 2


## Different seed across subscenarios, 
## same seed across multiple runs of the same subscenario
set.seed(052226 + as.numeric(id))
options(scipen = 0)


## Define dirs
setwd("/home/dg745/SurvivalBias_AgeStratified_MR")
input_dir <- paste(getwd(), "01_run_replicates_x_subscenario", "inputs", sep = "/")
out_dir <- paste("../rds/hpc-work/SurvivalBias_AgeStratified_MR", "01_run_replicates_x_subscenario", "outputs", sep = "/")
plot_dir <- paste("../rds/hpc-work/SurvivalBias_AgeStratified_MR", "01_run_replicates_x_subscenario", "plots", sep = "/")

dir.create(input_dir, showWarnings = F)
dir.create(out_dir, showWarnings = F)
dir.create(plot_dir, showWarnings = F)

## Input dirs
input_dir00 <- paste(getwd(), "00_define_scenarios", "outputs", sep = "/")


## Load helper functions for simulation:
source("helper_functions/simulate_genotype.R")
source("helper_functions/simulate_confounder.R")
source("helper_functions/simulate_age.R")
source("helper_functions/simulate_error.R")
source("helper_functions/simulate_exposure.R")
source("helper_functions/simulate_outcome.R")
source("helper_functions/simulate_survival_probability.R")


# ==============================================================================
#  Plotting functions (for internal use in script only)

# 1. Plot relationship between 2 variables with/without interaction
plot_relationship <- function(data, x, y, coeffs, interaction = F){
 
   labels = c("G" = expression(G), 
              "U" = expression(U), 
              "X" = expression(X),
              "Y" = expression(Y),
              "A" = expression(A),
              "p" = expression(p),
              "S" = expression(S))
  
  data$G <- as.character(data$G)
  data$S <- as.character(data$S)
  
  alpha_G = coeffs["alpha_G"]
  alpha_U = coeffs["alpha_U"]
  beta_X = coeffs["beta_X"]
  beta_A = coeffs["beta_A"]
  beta_U = coeffs["beta_U"]
  beta_XA = coeffs["beta_XA"]
  gamma_X = coeffs["gamma_X"]
  gamma_A = coeffs["gamma_A"]
  gamma_XA = coeffs["gamma_XA"]
  gamma_U = coeffs["gamma_U"]

  subt <- list("G_X" = bquote(alpha[G] == .(alpha_G)),
                "U_X" = bquote(alpha[U] == .(alpha_U)),
                 
                "G_Y" = bquote((beta[X] + beta[XA]*A) * alpha[G] == (.(beta_X) + .(beta_XA)*A) * .(alpha_G)),
                "X_Y" = bquote((beta[X] + beta[XA]*A) == .(beta_X) + .(beta_XA)*A),
                "A_Y" = bquote((beta[A] + beta[XA]*X) == (.(beta_A) + .(beta_XA)*X)),
                "U_Y" = bquote(beta[U] == .(beta_U)),
                 
                "G_p" = bquote((gamma[X] + gamma[XA]*A) * alpha[G] == (.(gamma_X) + .(gamma_XA)*A) * .(alpha_G)),
                "X_p" = bquote(gamma[X] + gamma[XA]*A == .(gamma_X) + .(gamma_XA)*A),
                "A_p" = bquote((gamma[A] + gamma[XA]*X) == (.(gamma_A) + .(gamma_XA)*X)),
                "U_p" = bquote(gamma[U] == .(gamma_U)),
                "S_p" = bquote(P(S == 1)),
                 
                "G_U" = bquote(beta[GU] == 0),
                "G_A" = bquote(beta[GA] == 0),
                "A_U" = bquote(beta[AU] == 0),
                "X_A" = bquote(beta[XA] == 0)
  )
    
  
  if(class(data[,y]) == "numeric" & class(data[,x]) == "character"){
    
    p = ggplot(data, aes(x = get(x), y = get(y))) + 
      geom_violin(alpha = 0, size = 0.3, colour = 'gray30', width = 0.8) +
      geom_boxplot(alpha = 0, size = 0.3, width = 0.1, colour='gray30', outliers = F) +
      theme_classic() +
      labs(x = labels[x], y = labels[y], subtitle = subt[[paste(x, y, sep = "_")]]) +
      theme(axis.title = element_text(size = 8),
            axis.text = element_text(size = 7),
            axis.line = element_line(linewidth = 0.4),
            axis.ticks = element_line(linewidth = 0.3))
    
    if(y != "p" & x != "S"){
      p <- p + geom_smooth(data = data, 
                                  aes(x = as.numeric(factor(get(x))),  
                                      y = get(y)), 
                                  method = "lm", se = F, colour = "gray30", linewidth = 0.6) 
    } else if(y == "p" & x != "S"){
      p <- p + geom_smooth(aes(group = 1), method = "glm", method.args = list(family = binomial),  
                           se = F, colour = "gray30", linewidth = 0.6)  
    }
  }
  
  else if(class(data[,y]) == "numeric" & class(data[,x]) == "numeric"){
    
    ## Downsample data
    if(nrow(data) >1000){
      data <- data[sample(seq(1:nrow(data)), floor(dim(data)[1]*0.2)), ]
    }

    if(interaction == F){
      p <- ggplot(data, aes(x = get(x), y = get(y))) + 
              geom_point(color='gray', alpha = 0.3, size = 1) +
              theme_classic() +
              labs(x = labels[x], y = labels[y], subtitle = subt[[paste(x, y, sep = "_")]]) +
              theme(axis.title = element_text(size = 8),
                    axis.text = element_text(size = 7),
                    axis.line = element_line(linewidth = 0.4),
                    axis.ticks = element_line(linewidth = 0.3))
      if(y != "p"){
        p <- p + geom_smooth(aes(group = 1), method = "lm",  se = F, colour = "gray30", linewidth = 0.6)  
      } else{
        p <- p + geom_smooth(aes(group = 1), method = "glm", method.args = list(family = binomial),  
                             se = F, colour = "gray30", linewidth = 0.6)  
      }
    }

    ## Interactions
    else{
      p <-  ggplot(data, aes(x = get(x), y = get(y), color = age_group)) + 
        facet_grid(cols = vars(age_group)) +
        geom_point(alpha = 0.3, size = 1, color = "gray") +
        theme_classic() +
        labs(x = labels[x], y = labels[y], subtitle = subt[[paste(x, y, sep = "_")]]) +
        guides(color = "none") +
        theme(axis.subtitle = element_text(size = 6),
              axis.text = element_text(size = 7),
              axis.line = element_line(linewidth = 0.4),
              axis.ticks = element_line(linewidth = 0.3),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold", size = 8))
      
      if(y != "p"){
        p <- p + geom_smooth(aes(group = 1), method = "lm",  se = F, linewidth = 0.6)  
      } else{
        p <- p + geom_smooth(aes(group = 1), method = "glm", method.args = list(family = binomial),  
                             se = F, linewidth = 0.6)  
      }
    }
    
  }
  
  return(p)
}

# 2. Plot distribution of 1 variable 
plot_distributions <- function(data, var, interaction = F){
  
  all <- data
  survivors <- subset(data, S == 1)
  
  df <- cbind(all, "who" = "all") %>% 
    rbind(cbind(survivors, "who" = "survivors"))
  
  # Only add survivors in each age group if there were
  for(grp in c("30-40", "40-50", "50-60", "60-70", "70-80")){
    sub <- subset(survivors, age_group == grp)
    if(nrow(sub) > 0){
      df <- rbind(df, cbind(sub, "who" = paste("survivors", grp)))
    }
  }
  
  df$G = as.character(df$G)
  
  labels = c("G" = expression(G), 
             "U" = expression(U), 
             "X" = expression(X),
             "Y" = expression(Y),
             "A" = expression(A),
             "p" = expression(p),
             "S" = expression(S))
  
  if(var == "X"){
    p = ggplot(df, aes(x = who, y = get(var))) + 
      geom_violin(alpha = 0, size = 0.3, colour = 'gray30', width = 0.8) +
      geom_boxplot(alpha = 0, size = 0.3, width = 0.1, colour='gray30', outliers = F) +
      theme_classic() +
      labs(x = "", y = labels[var], subtitle = paste(labels[var], "among survivors")) +
      theme(axis.title = element_text(size = 8),
            axis.text = element_text(size = 7, angle = 45, hjust = 1),
            axis.line = element_line(linewidth = 0.4),
            axis.ticks = element_line(linewidth = 0.3))
  }
  
  else if(var == "U"){
    p = ggplot(df, aes(x = get(var))) + 
      geom_histogram(fill = "gray90", colour = 'gray30', width = 0.8, linewidth = 0.15, boundary = 0) +
      facet_grid(cols = vars(who)) +
      theme_classic() +
      labs(x = "", y = labels[var], subtitle = paste(labels[var], "among survivors")) +
      theme(axis.title = element_text(size = 8),
            axis.text = element_text(size = 7, angle = 45, hjust = 1),
            axis.line = element_line(linewidth = 0.4),
            axis.ticks = element_line(linewidth = 0.3),
            strip.background = element_rect(fill = NA, color = NA))
  }
  
  else if(var == "A"){
    p = ggplot(df, aes(x = get(var))) + 
      geom_histogram(fill = "gray90", colour = 'gray30', width = 0.8, linewidth = 0.15, boundary = 30) +
      facet_grid(cols = vars(who)) +
      theme_classic() +
      labs(x = "", y = labels[var], subtitle = paste(labels[var], "among survivors")) +
      theme(axis.title = element_text(size = 8),
            axis.text = element_text(size = 7, angle = 45, hjust = 1),
            axis.line = element_line(linewidth = 0.4),
            axis.ticks = element_line(linewidth = 0.3),
            strip.background = element_rect(fill = NA, color = NA))
  }
  
  else if(var == "G"){
    p = ggplot(df, aes(x = get(var))) + 
      geom_bar(fill = "gray90", colour = 'gray30', width = 0.8, linewidth = 0.15) +
      facet_grid(cols = vars(who)) +
      theme_classic() +
      labs(x = "", y = labels[var], subtitle = paste(labels[var], "among survivors")) +
      theme(axis.title = element_text(size = 8),
            axis.text = element_text(size = 7),
            axis.line = element_line(linewidth = 0.4),
            axis.ticks = element_line(linewidth = 0.3),
            strip.background = element_rect(fill = NA, color = NA))
  }
  
  return(p)
  
}

# 3. Plot relationship between 2 variables after selection
plot_relationships_survivors <- function(data, x, y, coeffs){
  
  labels = c("G" = expression(G), 
             "U" = expression(U), 
             "X" = expression(X),
             "Y" = expression(Y),
             "A" = expression(A),
             "p" = expression(p),
             "S" = expression(S))
  
  data$G <- as.character(data$G)
  data$S <- as.character(data$S)
  
  alpha_G = coeffs["alpha_G"]
  alpha_U = coeffs["alpha_U"]
  beta_X = coeffs["beta_X"]
  beta_A = coeffs["beta_A"]
  beta_U = coeffs["beta_U"]
  beta_XA = coeffs["beta_XA"]
  gamma_X = coeffs["gamma_X"]
  gamma_A = coeffs["gamma_A"]
  gamma_XA = coeffs["gamma_XA"]
  gamma_U = coeffs["gamma_U"]
  
  subt <- list("G_X" = bquote(alpha[G] == .(alpha_G)),
               "U_X" = bquote(alpha[U] == .(alpha_U)),
               
               "G_Y" = bquote((beta[X] + beta[XA]*A) * alpha[G] == (.(beta_X) + .(beta_XA)*A) * .(alpha_G)),
               "X_Y" = bquote((beta[X] + beta[XA]*A) == .(beta_X) + .(beta_XA)*A),
               "A_Y" = bquote((beta[A] + beta[XA]*X) == (.(beta_A) + .(beta_XA)*X)),
               "U_Y" = bquote(beta[U] == .(beta_U)),
               
               "G_p" = bquote((gamma[X] + gamma[XA]*A) * alpha[G] == (.(gamma_X) + .(gamma_XA)*A) * .(alpha_G)),
               "X_p" = bquote(gamma[X] + gamma[XA]*A == .(gamma_X) + .(gamma_XA)*A),
               "A_p" = bquote((gamma[A] + gamma[XA]*X) == (.(gamma_A) + .(gamma_XA)*X)),
               "U_p" = bquote(gamma[U] == .(gamma_U)),
               "S_p" = bquote(P(S == 1)),
               
               "G_U" = bquote(beta[GU] == 0),
               "G_A" = bquote(beta[GA] == 0),
               "A_U" = bquote(beta[AU] == 0)
  )
  
  
  if(class(data[,y]) == "numeric" & class(data[,x]) == "character"){
    
    p = ggplot(data, aes(x = get(x), y = get(y))) + 
      facet_grid(cols = vars(age_group)) +
      geom_violin(alpha = 0, size = 0.3, colour = 'gray30', width = 0.8) +
      geom_boxplot(alpha = 0, size = 0.3, width = 0.1, colour='gray30', outliers = F) +
      geom_smooth(data = data, 
                  aes(x = as.numeric(factor(get(x))),  
                      y = get(y), group = 1), 
                  method = "lm", se = F, colour = "gray30", linewidth = 0.6) +
      theme_classic() +
      labs(x = labels[x], y = labels[y], subtitle = subt[[paste(x, y, sep = "_")]]) +
      theme(axis.title = element_text(size = 8),
            axis.text = element_text(size = 7),
            axis.line = element_line(linewidth = 0.4),
            axis.ticks = element_line(linewidth = 0.3))
    
  }
  
  else if(class(data[,y]) == "numeric" & class(data[,x]) == "numeric"){
    
    p <- ggplot(data, aes(x = get(x), y = get(y))) + 
      facet_grid(cols = vars(age_group)) +
      geom_point(color='gray', alpha = 0.3, size = 1) +
      geom_smooth(aes(group = 1), method = "lm",  se = F, colour = "gray30", linewidth = 0.6) +
      theme_classic() +
      labs(x = labels[x], y = labels[y], subtitle = subt[[paste(x, y, sep = "_")]]) +
      theme(axis.title = element_text(size = 8),
            axis.text = element_text(size = 7),
            axis.line = element_line(linewidth = 0.4),
            axis.ticks = element_line(linewidth = 0.3))
    
  }
  
  return(p)
}



# ==============================================================================
#  Main simulation function:
simulation <- function(N, main_scenario, main_scenario_val, sub_sce_varying_par, sub_sce_varying_par_value, 
                       alpha_G, alpha_U, 
                       beta_U, beta_A, beta_X, beta_XA, 
                       gamma_0, gamma_X, gamma_A, gamma_XA, gamma_U, 
                       replicate){
  
  ## Outputs and plots subdirs x subscenario x replicate
  out_dir_sc <- paste(out_dir, main_scenario, main_scenario_val, sub_sce_varying_par, sub_sce_varying_par_value, sep = "/")
  plot_dir_sc <- paste(plot_dir, main_scenario, main_scenario_val, sub_sce_varying_par, sub_sce_varying_par_value, sep = "/")
  
  dir.create(out_dir_sc, recursive = T, showWarnings = F)
  dir.create(plot_dir_sc, recursive = T, showWarnings = F)
  
  ## df to save results (and inputs)
  out <- data.frame("main_scenario" = main_scenario, "main_scenario_val" = main_scenario_val, "sub_sce_varying_par" = sub_sce_varying_par, "sub_sce_varying_par_value" = sub_sce_varying_par_value, 
                    "replicate" = replicate, "N" = N,
                    "alpha_G" = alpha_G, "alpha_U" = alpha_U, 
                    "beta_U" = beta_U, "beta_A" = beta_A, "beta_X" = beta_X, "beta_XA" = beta_XA, 
                    "gamma_0" = gamma_0, "gamma_X" = gamma_X, "gamma_A" = gamma_A, "gamma_XA" = gamma_XA, "gamma_U" = gamma_U)
  
  message(paste("Started simulation for replicate", replicate, "of subscenario:", 
                main_scenario, main_scenario_val, sub_sce_varying_par, sub_sce_varying_par_value))
  
  #  Step 1: Simulate genotype: G ~ Binom(2, q = 0.3)
  # ___________________________________________________________________________
  q = 0.3
  G = simulate_genotype(N, q)
  indiv_data <- data.frame("G" = G)
  
  # Step 2: Simulate unmeasured confounder U ~ Unif(0,1) 
  # ___________________________________________________________________
  U = simulate_confounder(N)
  indiv_data <- cbind(indiv_data, "U" = U)
  
  # Step 3: Simulate age A ~ Unif(30,80) 
  # ___________________________________________________________________
  A = simulate_age(min_age = 30, max_age = 80, N = N)
  indiv_data <- cbind(indiv_data, "A" = A)

  
  # Step 4: Simulate error terms for X and Y:  εx,εʏ ~ N(0,1) 
  # ___________________________________________________________________
  eX = simulate_error(N)
  eY = simulate_error(N)
  
  indiv_data <- cbind(indiv_data, "eX" = eX, "eY" = eY)
  
  # Step 5: Generate exposure X = αɢ(G) + αuU + εx
  # ___________________________________________________________________
  X = simulate_exposure(indiv_data, cols = c("G", "U", "eX"), alpha_G, alpha_U)
  indiv_data <- cbind(indiv_data, "X" = X)
  
  
  # Step 6: Generate outcome Y = (βx + βxA*A)X + βA*A + βuU + εʏ
  # ___________________________________________________________________
  Y = simulate_outcome(indiv_data, cols = c("X", "A", "U", "eY"), beta_X, beta_A, beta_XA, beta_U)
  indiv_data <- cbind(indiv_data, "Y" = Y)

  
  # Step 7: Generate selection p = 1 / 1 + e^-(γo + γxX + γA*A + γxA*XA + γuU)
  # ___________________________________________________________________
  probs = simulate_survival_probability(indiv_data, cols = c("X", "A", "U"), gamma_0, gamma_X, gamma_A, gamma_XA, gamma_U)
  indiv_data <- cbind(indiv_data, "p" = probs)
  
  #-----------------------------------------------------------------------------
  ## Previous approaches for selection:
  
  # Step 8: select surviors as those with p>0.5
  # If not at least 1000 oldest selected, select up to Z including at least 1000 
  # indiv_data <- indiv_data %>% dplyr::arrange(-p) 
  # n_oldest <- length(which(indiv_data$age_group == "70-80" & indiv_data$p>0.5))
  # 
  # if(n_oldest < 1000){
  #   indiv_data <- indiv_data %>% dplyr::dplyr::mutate(oldest = dplyr::dplyr::case_when(age_group == "70-80" ~ 1, .default = 0)) %>% dplyr::dplyr::mutate(sum = cumsum(oldest))
  #   S = 1:dim(indiv_data)[1] <= which(indiv_data$sum == 1000)[1]
  #   indiv_data <- cbind(indiv_data, "S" = S)
  # } else{
  #   indiv_data <- indiv_data %>% dplyr::dplyr::mutate(S = dplyr::dplyr::case_when(p>0.5 ~ TRUE, .default = FALSE)) 
  # }
  # 
  # out$N_survivors <- table(S)["TRUE"]
  # out$min_p = min(subset(indiv_data, S == TRUE)$p)
  
  # Step 9: Random sample from survivors across ages
  # ___________________________________________________________________
  # ## Randomly sample equal numbers of survivors across age groups
  # sampled <- survivors %>%
  #   dplyr::group_by(age_group) %>%
  #   dplyr::slice_sample(n = min(table(survivors$age_group))) %>% as.data.frame()
  # 
  # ## Num of indiv per age group
  # out$N_survivors_sampled <- dim(sampled)[1]
  # for(group in ages){
  #   out[, paste0("N_survivors_sampled_", gsub("-", "_", group))] <- table(factor(sampled$age_group, levels = ages))[group] 
  # }

  #-----------------------------------------------------------------------------
  
  # Step 8: Generate selection indicator S ~ Bernoulli(p)
  # ___________________________________________________________________
  S =  sapply(probs, function(p){rbinom(size = 1, prob = p, n = 1)})
  table(S)
  out$N_survivors <- table(S)["1"]
  indiv_data <- cbind(indiv_data, "S" = S)
  survivors <- subset(indiv_data, S == 1)
  
  ## Define age stratification variable 
  indiv_data <- indiv_data %>% dplyr::mutate(age_group = dplyr::case_when(A <= quantile(survivors$A, probs = 0.20) ~ "1",
                                                          A > quantile(survivors$A, probs = 0.20) & A <= quantile(survivors$A, probs = 0.4) ~ "2",
                                                          A > quantile(survivors$A, probs = 0.4) & A <= quantile(survivors$A, probs = 0.6) ~ "3",
                                                          A > quantile(survivors$A, probs = 0.6) & A <= quantile(survivors$A, probs = 0.8) ~ "4",
                                                          A > quantile(survivors$A, probs = 0.8)  ~ "5"))
  
  survivors <- survivors %>% dplyr::mutate(age_group = dplyr::case_when(A <= quantile(survivors$A, probs = 0.20) ~ "1",
                                                          A > quantile(survivors$A, probs = 0.20) & A <= quantile(survivors$A, probs = 0.4) ~ "2",
                                                          A > quantile(survivors$A, probs = 0.4) & A <= quantile(survivors$A, probs = 0.6) ~ "3",
                                                          A > quantile(survivors$A, probs = 0.6) & A <= quantile(survivors$A, probs = 0.8) ~ "4",
                                                          A > quantile(survivors$A, probs = 0.8)  ~ "5"))
  
  for(group in 1:5){
    out[, paste0("N_survivors_", group)] <-  table(factor(survivors$age_group, levels = 1:5))[group]
  }
  
  
  ## Num of survivors from each genotype
  n <- table(factor(survivors$age_group, levels = 1:5),
             factor(survivors$G, levels = c(0:2)))
  
  # ============================================================================
  ## Stop if <200 survivors and/or no genotype variance exists across age strata (and rerun function with larger N)
  if(table(S)["1"] < 200 | any(by(survivors$G, survivors$age_group, var) == 0)){
    return(list("N_survivors" = table(S)["1"], 
                "mean_p" = mean(probs),
                "var_G" = as.vector(by(survivors$G, survivors$age_group, var))
      )
    )
  }
  # ============================================================================
  
  ## Continue othwerwise
  for(group in 1:5){
    
    out[, paste0("N_survivors_", group, "_hom_REF")] <- n[group, "0"]
    out[, paste0("N_survivors_", group, "_het")] <- n[group, "1"]
    out[, paste0("N_survivors_", group, "_hom_ALT")] <- n[group, "2"]
  }
  
  ## MAF among survivors
  out[, "MAF_survivors"] <- ((2*table(factor(survivors$G, levels = c(0:2)))["2"]) + table(factor(survivors$G, levels = c(0:2)))["1"]) / (2*dim(survivors)[1])
  
  ## MAF in each age group
  for(group in 1:5){
    out[, paste0("MAF_survivors_", group)] <- ((2*n[group, "2"]) + n[group, "1"])/ (2*sum(n[group, ]))
  }
  
  
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  #             !!!  Sanity check plots  !!!
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  coeffs <- c("alpha_G" = alpha_G,
              "alpha_U" = alpha_U,
              "beta_X" = beta_X,
              "beta_A" = beta_A,
              "beta_U" = beta_U,
              "beta_XA" = beta_XA,
              "gamma_X" = gamma_X,
              "gamma_A" = gamma_A,
              "gamma_XA" = gamma_XA,
              "gamma_U" = gamma_U)
  
  plot_file = paste0(plot_dir_sc, "/effects_before_and_after_selection.pdf")
  if(replicate == 1){
    if(!file.exists(plot_file)){
      
      ## Plot G-X, U-X, G-Y, X-Y, A-Y, U-Y, XA-Y in whole population
      p1 <- plot_relationship(indiv_data, "G", "X", coeffs)
      p2 <- plot_relationship(indiv_data, "U", "X", coeffs)
      p3 <- plot_relationship(indiv_data, "G", "Y", coeffs)
      p4 <- plot_relationship(indiv_data, "X", "Y", coeffs)
      p5 <- plot_relationship(indiv_data, "X", "Y", coeffs, interaction = T)
      p6 <- plot_relationship(indiv_data, "A", "Y", coeffs)
      p7 <- plot_relationship(indiv_data, "U", "Y", coeffs)
      
      ## Plot effects on surviving probability
      p8 <- plot_relationship(indiv_data, "S", "p", coeffs)
      p9 <- plot_relationship(indiv_data, "X", "p", coeffs)
      p10 <- plot_relationship(indiv_data, "X", "p", coeffs, interaction = T)
      p11 <- plot_relationship(indiv_data, "A", "p", coeffs)
      p12 <- plot_relationship(indiv_data, "U", "p", coeffs)
      
      ## Compare variables in whole population vs survivors
      p13 <- plot_distributions(indiv_data, var = "X")
      p14 <- plot_distributions(indiv_data, var = "A")
      p15 <- plot_distributions(indiv_data, var = "U")
      p16 <- plot_distributions(indiv_data, var = "G")
    }    
  }

  ## Variable distributions across age strata
  for(variable in c("X", "Y", "U", "A")){
    out[, paste0(variable, "_min_", 1:5)] <- by(survivors[, variable], survivors$age_group, min) %>% as.vector()
    out[, paste0(variable, "_max_", 1:5)] <- by(survivors[, variable], survivors$age_group, max) %>% as.vector()
    out[, paste0(variable, "_mean_", 1:5)] <- by(survivors[, variable], survivors$age_group, mean) %>% as.vector()
    out[, paste0(variable, "_var_", 1:5)] <- by(survivors[, variable], survivors$age_group, var) %>% as.vector()
    out[, paste0(variable, "_1st_quartile_", 1:5)] <- by(survivors[, variable], survivors$age_group, quantile, 0.25) %>% as.vector()
    out[, paste0(variable, "_2nd_quartile_", 1:5)] <- by(survivors[, variable], survivors$age_group, quantile, 0.50) %>% as.vector()
    out[, paste0(variable, "_3rd_quartile_", 1:5)] <- by(survivors[, variable], survivors$age_group, quantile, 0.75) %>% as.vector()
  }
  
  
  # Step 9: Estimate induced G-U, G-A, A-U, X-A associations among survivors ("conditioning on S")
  # ___________________________________________________________________
  k = 1
  plots <- list()
  
  for(group in c("all", "survivors", paste0("survivors_", 1:5))){
  
    if(group == "all"){data = indiv_data}
    else if(group == "survivors"){data = survivors} 
    else if(group == "survivors_1"){data = subset(survivors, age_group == "1")}
    else if(group == "survivors_2"){data = subset(survivors, age_group == "2")}
    else if(group == "survivors_3"){data = subset(survivors, age_group == "3")}
    else if(group == "survivors_4"){data = subset(survivors, age_group == "4")}
    else if(group == "survivors_5"){data = subset(survivors, age_group == "5")}
    
    out[, paste0("beta_GU_", group)] <- summary(lm(U ~ G, data))$coefficients["G",]["Estimate"]
    out[, paste0("se_GU_", group)] <- summary(lm(U ~ G, data))$coefficients["G",]["Std. Error"]
    out[, paste0("t_GU_", group)] <- summary(lm(U ~ G, data))$coefficients["G",]["t value"]
    out[, paste0("p_GU_", group)] <- summary(lm(U ~ G, data))$coefficients["G",]["Pr(>|t|)"]
    
    out[, paste0("beta_GA_", group)] <- summary(lm(A ~ G, data))$coefficients["G",]["Estimate"]
    out[, paste0("se_GA_", group)] <- summary(lm(A ~ G, data))$coefficients["G",]["Std. Error"]
    out[, paste0("t_GA_", group)] <- summary(lm(A ~ G, data))$coefficients["G",]["t value"]
    out[, paste0("p_GA_", group)] <- summary(lm(A ~ G, data))$coefficients["G",]["Pr(>|t|)"]
    
    out[, paste0("beta_AU_", group)] <- summary(lm(A ~ U, data))$coefficients["U",]["Estimate"]
    out[, paste0("se_AU_", group)] <- summary(lm(A ~ U, data))$coefficients["U",]["Std. Error"]
    out[, paste0("t_AU_", group)] <- summary(lm(A ~ U, data))$coefficients["U",]["t value"]
    out[, paste0("p_AU_", group)] <- summary(lm(A ~ U, data))$coefficients["U",]["Pr(>|t|)"]
    
    out[, paste0("beta_XA_", group)] <- summary(lm(X ~ A, data))$coefficients["A",]["Estimate"]
    out[, paste0("se_XA_", group)] <- summary(lm(X ~ A, data))$coefficients["A",]["Std. Error"]
    out[, paste0("t_XA_", group)] <- summary(lm(X ~ A, data))$coefficients["A",]["t value"]
    out[, paste0("p_XA_", group)] <- summary(lm(X ~ A, data))$coefficients["A",]["Pr(>|t|)"]
   
    ## Plot associations per group
    plots[[k]] <- plot_relationship(data, "G", "U", coeffs)
    plots[[k+1]] <- plot_relationship(data, "G", "A", coeffs)
    plots[[k+2]] <- plot_relationship(data, "A", "U", coeffs)
    plots[[k+3]] <- plot_relationship(data, "A", "X", coeffs)

    k = k + 4
  }
  
  ## Plot associations per group
  plot_file2 = paste0(plot_dir_sc, "/collider_effects_before_and_after_selection_and_sampling.pdf")
  if(replicate == 1){
    if(!file.exists(plot_file2)){
      plot_grid(plotlist = plots, align = "vh", ncol = 4, 
                labels = sapply(c("whole population", "survivors", paste0("survivors_", 1:5)), 
                                function(x){c(x, NA, NA, NA)}) %>% as.vector(),
                label_size = 10, label_x = 0, label_y = 1)
      ggsave(filename = plot_file2, width = 20, height = 24)
    }
  }
  
  
  # Step 10: Estimate causal effect among sampled survivors
  # ___________________________________________________________________
  ## Plot X-Y association per age group
  if(replicate == 1){
    if(!file.exists(plot_file)){
      p17 <- plot_relationships_survivors(survivors, "G", "X", coeffs)
      p18 <- plot_relationships_survivors(survivors, "G", "Y", coeffs)
      p19 <- plot_relationships_survivors(survivors, "X", "Y", coeffs)
      plots <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                    p11, p12, p13, p14, p15, p16, p17, p18, p19)
      plot_grid(plotlist = plots, ncol = 5)
      ggsave(filename = paste0(plot_dir_sc, "/effects_before_and_after_selection.pdf"), width = 26, height = 10)
    }
  }
  
  for(group in c("all", "survivors", paste0("survivors_", 1:5))){
    
    if(group == "all"){
      data = indiv_data
    } else if(group == "survivors"){
      data = survivors
    } else{
      data = subset(survivors, age_group == gsub("^.*_", "", group))
    }
    ## Overall effect 
    if(!var(data$G) %in% c(NA, 0)){
      
        ## G-X association
        fit <- summary(lm(X ~ G, data))
        beta_GX <- fit$coefficients["G",]["Estimate"]
        se_GX <- fit$coefficients["G",]["Std. Error"]
        t_GX <- fit$coefficients["G",]["t value"]
        p_GX <- fit$coefficients["G",]["Pr(>|t|)"]
        
        ## Instrument strength
        Fstat <- fit$fstatistic["value"]
        
        ## G-Y association
        beta_GY <- summary(lm(Y ~ G, data))$coefficients["G",]["Estimate"]
        se_GY <- summary(lm(Y ~ G, data))$coefficients["G",]["Std. Error"]
        t_GY <- summary(lm(Y ~ G, data))$coefficients["G",]["t value"]
        p_GY <- summary(lm(Y ~ G, data))$coefficients["G",]["Pr(>|t|)"]
        
        ## Ratio estimate, se, Z and p
        beta_XY <- beta_GY / beta_GX
        se_XY <- se_GY / abs(beta_GX)
        z_XY <- beta_XY / se_XY
        p_XY <- 2 * pnorm(-abs(z_XY))
      } 
      else{
        message("No variation in G.")
        stop()
      }
    
    out[, paste0("beta_GX_", group)] <- beta_GX
    out[, paste0("se_GX_", group)] <- se_GX
    out[, paste0("t_GX_", group)] <- t_GX
    out[, paste0("p_GX_", group)] <- p_GX
    out[, paste0("Fstat_", group)] <- Fstat
    
    out[, paste0("beta_GY_", group)] <- beta_GY
    out[, paste0("se_GY_", group)] <- se_GY
    out[, paste0("t_GY_", group)] <- t_GY
    out[, paste0("p_GY_", group)] <- p_GY
    
    out[, paste0("beta_XY_", group)] <- beta_XY
    out[, paste0("se_XY_", group)] <- se_XY
    out[, paste0("z_XY_", group)] <- z_XY
    out[, paste0("p_XY_", group)] <- p_XY

  }

  # Step 11: Test XxA interaction among  survivors
  # ___________________________________________________________________
  
  ## Assess XxA interaction term (using genetiCcally predicted X)
  survivors$X_pred <- survivors$G*beta_GX
  beta_XA_Y <- summary(lm(Y ~ X_pred*A, survivors))$coefficients["X_pred:A",]["Estimate"]
  se_XA_Y <- summary(lm(Y ~ X_pred*A, survivors))$coefficients["X_pred:A",]["Std. Error"]
  t_XA_Y <- summary(lm(Y ~ X_pred*A, survivors))$coefficients["X_pred:A",]["t value"]
  p_XA_Y <- summary(lm(Y ~ X_pred*A, survivors))$coefficients["X_pred:A",]["Pr(>|t|)"]
  
  out$beta_XA_Y <- beta_XA_Y
  out$se_XA_Y <- se_XA_Y
  out$t_XA_Y <- t_XA_Y
  out$p_XA_Y <- p_XA_Y
  
  # Meta-regression of effect estimates with median stratum age
  # data <-  data.frame("stratum" = 1:5,
  #                     "age_mid" = unlist(by(survivors$A, survivors$age_group, median)),
  #                     "beta" = unlist(out[paste0("beta_XY_survivors_", 1:5)]),
  #                     "se" = unlist(out[paste0("se_XY_survivors_", 1:5)]))
  # model_trend <- rma(
  #   yi = beta, 
  #   sei = se,
  #   mods = ~ age_mid,
  #   data = data,
  #   method = "REML",
  #   test = "knha"
  # )
  # 
  # out$beta_meta_regression = model_trend$beta["age_mid",]
  # out$se_meta_regression = model_trend$se[2]
  # out$p_meta_regression = model_trend$pval[2]
  
  # Cochran's Q test for heterogeneity:
  se <- out[, paste0("se_XY_survivors_", 1:5)]
  beta <- out[, paste0("beta_XY_survivors_",1:5)]
  
  if(any(is.na(beta))){
    beta_pooled <- Q <- p_het <- NA
  } else{
    w <- 1 / se^2
    beta_pooled <- sum(w * beta) / sum(w)
    Q  <- sum(w * (beta - beta_pooled)^2)
    p_het <- pchisq(Q, df = length(beta) - 1, lower.tail = FALSE)
  }

  out$beta_XY_pooled = beta_pooled
  out$Q = Q
  out$p_het = p_het

  message(paste("Finished simulation for replicate", replicate, "of subscenario:", 
                main_scenario, main_scenario_val, sub_sce_varying_par, sub_sce_varying_par_value))
  
  return(list("out" = out, 
              "N_survivors" = table(S)["1"], 
              "mean_p" = mean(probs),
              "var_G" = as.vector(by(survivors$G, survivors$age_group, var))
  ))
}



# ==============================================================================
# Run simulation replicates x subscenario: 

## Main scenario subscenarios:
if(main == "scenario.01"){
  ## Scenario 1
  scenarios <- get(load(paste0(input_dir00, "/scenario.01.Rdata")))
} else if(main == "scenario.02"){
   ## Scenario 2
  scenarios <- get(load(paste0(input_dir00, "/scenario.02.Rdata")))
} else if(main == "scenario.03"){
  ## Scenario 3
  scenarios <- get(load(paste0(input_dir00, "/scenario.03.Rdata")))
} 

## Subscenario pars
sim_args <- scenarios[id, ]

## 1000 replicates x scenario
n_rep = 1000


## Dir to save subscenario results 
out_dir_sc <- paste0(out_dir, "/", sim_args[1], "/", sim_args[2], "/", sim_args[3],  "/", sim_args[4])

## Run n_rep replicates
res = vector()
for(i in 1:n_rep){
  
  ## Start with 500,000 individuals
  res_i <- do.call(simulation, c("N" = 500000, sim_args, "replicate" = i))
  
  
  ## Repeat simulation until we get (on average) 200 survivors according to E[p] 
  ## and genotype variabilty to estimate genetic effects
 
   while(res_i$N_survivors <200 | any(res_i$var_G == 0)){
    
    N <- 210/res_i$mean_p 
    res_i <- do.call(simulation, c("N" = N, sim_args, "replicate" = i))
    
  }
  
  res = rbind(res, res_i$out)
}

#   res = rbind(res, do.call(simulation, c(sim_args, i)))

## Save subscenario results 
save(res, file = paste0(out_dir_sc, "/subscenario_res_across_", n_rep, "replicates.Rdata"))



## Skip if 100 replicates were run already for scenario
# if("scenario_res_across_100replicates.Rdata" %in% list.files(out_dir_sc) & length(list.files(out_dir_sc)) >100){
#   stop("Outputs already generated for scenario.", call. = T)
# } else{
#   
#   scenario_res = vector()
#   for(i in 1:n_rep){
#     scenario_res = rbind(scenario_res, do.call(simulation_indiv_data_1IV, c(sim_args, i)))
#   }
#   
#   ## Save scenario results across replicates
#   save(scenario_res, file = paste0(out_dir_sc, "/scenario_res_across_", n_rep, "replicates.Rdata"))
# }


## Example test:

# gamma_0  =  2      # baseline survival — controls overall survival rate
# gamma_X  = -0.1    # higher X → lower survival (negative as intended)
# gamma_A  = -0.05   # older → lower survival (per year, so -0.05 * 50yrs = -2.5 logit units)
# gamma_XA = -0.1    # interaction: effect of X on survival stronger at older ages
# gamma_U  =  -0.1    # confounder effect

#
main_scenario = sim_args$main_scenario
main_scenario_val = sim_args$main_scenario_val
sub_sce_varying_par = sim_args$sub_sce_varying_par
sub_sce_varying_par_value = sim_args$sub_sce_varying_par_value
alpha_G = sim_args$alpha_G
alpha_U = sim_args$alpha_U
beta_U = sim_args$beta_U
beta_A = sim_args$beta_A
beta_X = sim_args$beta_X
beta_XA = sim_args$beta_XA
gamma_0 = sim_args$gamma_0
gamma_X = sim_args$gamma_X
gamma_A = sim_args$gamma_A
gamma_XA = sim_args$gamma_XA
gamma_U = sim_args$gamma_U
replicate = 1







## Reproducibility info
# session_info()

# ─ Session info ─────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.1 (2025-06-13)
# os       macOS Monterey 12.7.6
# system   x86_64, darwin20
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       Europe/London
# date     2025-10-01
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# quarto   1.6.42 @ /private/var/folders/j1/klb6dgpn3dxfdrxfmbb4f0jc0000gp/T/AppTranslocation/7D11FDEE-A995-4F90-9AC5-8C647803C12A/d/RStudio.app/Contents/Resources/app/quarto/bin/quarto
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────
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
# tibble         3.3.0   2025-06-08 [1] CRAN (R 4.5.0)
# tidyr        * 1.3.1   2024-01-24 [1] CRAN (R 4.5.0)
# tidyselect     1.2.1   2024-03-11 [1] CRAN (R 4.5.0)
# usethis      * 3.2.1   2025-09-06 [1] CRAN (R 4.5.1)
# vctrs          0.6.5   2023-12-01 [1] CRAN (R 4.5.0)
# withr          3.0.2   2024-10-28 [1] CRAN (R 4.5.0)
# 
# [1] /Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/library
# * ── Packages attached to the search path.
# 
# ────────────────────────────────────────────────────────────────────────────────────────────────────