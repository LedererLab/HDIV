#!/usr/bin/env Rscript
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################################################################
# Dependencies
#args <- commandArgs(trailingOnly = TRUE
suppressMessages(library(purrr))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
#suppressMessages(source("kernel/utils.R"))

#########################################################################
# Analysis

analyze <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  # res_dir <- args[1]
  res_dir <- "/Users/David/johannes/hdiv/res"
  # project_dir <- args[2]
  project_dir <- "/Users/David/johannes/hdiv"
  suppressMessages(source(paste(project_dir, "src/kernel/utils.R", sep = "/")))
  
  df_est <- paste(res_dir, "est.csv", sep = "/") %>% read.csv
  df_stats <- paste(res_dir, "stats.csv", sep = "/") %>% read.csv
  configs <- paste(project_dir, "src/config/configs.csv", sep = "/") %>% read.csv

  coverage <- .coverage(df_est, df_stats, configs) %>%
    print
  print(coverage)
  
  l2bias <- .l2bias(df_est)
  print(l2bias)
  
  filter(df_stats, config_id == 1,
         estimator == "Debiased") %>% 
    View
}

..n <- function(configs, .config_id) {
  map_dbl(.config_id, ~ filter(configs, config_id == .)$n %>% as.numeric)
}


.coverage <- function(df_est, df_stats, configs) {
  df_est %>%
    # filter(estimator == "Debiased") %>%
    inner_join(dplyr::select(configs, config_id, n, sigma0_h), 
               by = "config_id") %>%
    inner_join(filter(df_stats, estimator == "Debiased") %>%
                 dplyr::select(config_id, trial_id, sigma0_hhat),
               by = c("config_id", "trial_id")) %>%
    mutate(SE = vhat * sigma0_h / sqrt(n)) %>%
    
    # filter(trial_id == 1, config_id == 1) %>%
    # ggplot(aes(j, estimate_j, color=estimator)) +
    #   geom_point()
  
    # View
    # mutate(SE = vhat * ..sigma0_hhat(df_stats, trial_id, config_id) / sqrt(..n(configs, config_id))) %>%
    # mutate(SE = 1) %>%
    group_by(config_id, j) %>%
    mutate(coverage_j = covered(estimate_j, beta0_j, SE)) %>%
    # summarize(coverage_j = mean(covered(estimate_j, beta0_j, SE))) %>%
    # group_by(config_id) %>%
    summarize(coverage = mean(coverage_j)) %>% 
    inner_join(configs, by = "config_id")
}

.l2bias <- function(df_est) {
  df_est %>%
    group_by(config_id, j, estimator) %>%
    summarize(avg_bias = mean(estimate_j - beta0_j)) %>%
    group_by(estimator) %>%
    summarize(l2_avg_bias = l2(avg_bias))
}

analyze()

df_stats %>%
  ggplot(aes(trial_id, mse_debiased)) +
    geom_point(aes(color = config_id))

View(df_stats)
