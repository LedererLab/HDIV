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
  res_dir <- args[1]
  project_dir <- args[2]
  suppressMessages(source(paste(project_dir, "src/kernel/utils.R", sep = "/")))
  
  df_est <- paste(res_dir, "est.csv", sep = "/") %>% read.csv
  df_stats <- paste(res_dir, "stats.csv", sep = "/") %>% read.csv
  configs <- paste(project_dir, "src/config/configs.csv", sep = "/") %>% read.csv

  coverage <- .coverage(df_est, df_stats, configs)
  print(cvg)
  
  l2bias <- .l2bias(df_est)
  print(l2bias)
}

..n <- function(configs, .config_id) { filter(configs, config_id == .config_id)$n %>% as.numeric }
..sigma_h <- function(configs, .config_id) { filter(configs, config_id == .config_id)$sigma_h %>% as.numeric }
..sigma_hhat <- function(df_stats, .trial_id, .config_id) {
  filter(df_stats, trial_id == .trial_id, config_id == .config_id,
         estimator == "Debiased")$sigma_hhat %>%
    as.numeric
} 

.coverage <- function(df_est, df_stats, configs) {
  df_est %>%
    filter(estimator == "Debiased") %>%
    mutate(SE = vhat * ..sigma_hhat(df_stats, trial_id, config_id) / sqrt(..n(configs, config_id))) %>%
    group_by(config_id, j) %>%
    summarize(coverage_j = mean(covered(estimate_j, beta0_j, SE))) %>%
    group_by(config_id) %>%
    summarize(coverage = mean(coverage_j))
}

.l2bias <- function(df_est) {
  df_est %>%
    group_by(config_id, j, estimator) %>%
    summarize(avg_bias = mean(estimate_j - beta0_j)) %>%
    group_by(estimator) %>%
    summarize(l2_avg_bias = l2(avg_bias))
}

analyze()

