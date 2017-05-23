#!/usr/bin/env Rscript
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################################################################
# Dependencies
suppressMessages(library(purrr))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(source("kernel/utils.R"))

#########################################################################
# Analysis

analyze <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  df_est <- args[1] %>% read.csv() %>%
    dplyr::select(config_id = 2, everything())
  
  cvg <- .cvg(df_est)
  print(cvg)
  
  l2bias <- .l2bias(df_est)
  print(l2bias)
}

.cvg <- function(df_est) {
  df_est %>%
    filter(estimator == "Debiased") %>%
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

