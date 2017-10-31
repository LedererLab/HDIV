#!/usr/bin/env Rscript

#########################################################################
# Dependencies
suppressMessages(library(purrr))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(xtable))
suppressMessages(source("src/utils.r"))

options(tibble.width = Inf)

#########################################################################
# Analysis

#########################################################################
# Data ingestion

ingest <- function() {
  est <- read.csv("res/est.csv")
  stats <- read.csv("res/stats.csv")
  configs <- read.csv("configs/configs.csv")

  list(est = est, stats = stats, configs = configs)
}

#########################################################################
# Coverage

cvg <-function(res) {
  est <- res$est; stats <- res$stats; configs <- res$configs
  est %>%
    filter(estimator %in% c("Debiased_CLIME", "Debiased_JM")) %>%
    inner_join(dplyr::select(configs, config_id, n, sigma0_u, sigma0_v),
               by = "config_id") %>%
    filter(sigma0_u == 0.7, sigma0_v == 0.7) %>%
    # inner_join(filter(stats, estimator %in% c("Debiased_CLIME", "Debiased_JM")) %>%
    #            dplyr::select(config_id, trial_id),
    #            by = c("config_id", "trial_id", "estimator")) %>%
    mutate(cvgj = covered(estimate_j, beta0_j,
                          sqrt(.7*Theta_jj)/sqrt(n))) %>%
    group_by(estimator, config_id, j) %>%
    summarize(cvgj = mean(cvgj),
              ntrials = n()) %>%
      # sdj = sd(estimate_j),
              # SEj = mean(SE1.la)) %>%
    group_by(estimator, config_id) %>%
    summarize(cvg = mean(cvgj),
              ntrials = mean(ntrials)) %>%
              # SE = mean(SEj),
              # sd = mean(sdj)) %>%
    inner_join(configs, by = "config_id") %>%
    dplyr::select(config_id, n, px, pz, s_beta, s.j, type, cvg, sigma0_u, sigma0_v, ntrials) %>%
    arrange(type)
}

diagnose.Theta <- function(res) {
  est <- res$est; stats <- res$stats; configs <- res$configs
  est %>%
    filter(estimator %in% c("Debiased_CLIME", "Debiased_JM")) %>%
    mutate(
      bias.Theta_jj = Theta.hat_jj-Theta_jj
    ) %>%
    group_by(estimator, config_id) %>%
    summarize(
      # Theta_jj = mean(Theta_jj),
      # Theta.hat_jj = mean(Theta.hat_jj),
      avg_bias.Theta_jj = mean(bias.Theta_jj),
      max_abs_bias.Theta_jj = max(abs(bias.Theta_jj))
    ) %>%
    inner_join(dplyr::select(configs, config_id, n, n, px, pz, s_beta, s.j, type, sigma0_u, sigma0_v),
             by = "config_id") %>%
    filter(sigma0_u == 0.7, sigma0_v == 0.7) %>%
    arrange(estimator, type)
}

diagnose.sd_u <- function(res) {
  est <- res$est; stats <- res$stats; configs <- res$configs
  stats %>%
    filter(estimator %in% c("Debiased_CLIME", "Debiased_JM")) %>%
    group_by(config_id) %>%
    summarize(sd_u.hat = mean(sd_u)) %>%
    inner_join(configs, by = "config_id") %>%
    filter(sigma0_u == 0.7, sigma0_v == 0.7) %>%
    dplyr::select(config_id, n, px, pz, s_beta, s.j, type, sd_u.hat, sigma0_u)
}

mse_beta <- function(res) {
  est <- res$est; stats <- res$stats; configs <- res$configs
  est %>%
    filter(estimator == "Lasso") %>%
    mutate(bias = estimate_j - beta0_j) %>%
    group_by(config_id, trial_id) %>%
    summarize(mse = mean(bias^2)) %>%
    group_by(config_id) %>%
    summarize(avg_mse = mean(mse)) %>%
    inner_join(configs, by = "config_id") %>%
    filter(sigma0_u == 0.7, sigma0_v == 0.7) %>%
    dplyr::select(config_id, n, px, pz, s_beta, s.j, type, avg_mse)
}

diagnose_rems <- function(res) {
  est <- res$est; stats <- res$stats; configs <- res$configs
  est %>%
    filter(estimator %in% c("Debiased_CLIME", "Debiased_JM")) %>%
    inner_join(dplyr::select(configs, config_id, n, sigma0_u),
               by = "config_id") %>%
    mutate(z_j = w/sqrt(Theta_jj),
           cvg_j = ((z_j >= -1.96) & (z_j <= 1.96))) %>%
    group_by(estimator, config_id) %>%
    summarize(cvg = mean(cvg_j),
              avg_rem_1 = mean(abs(rem_1)),
              avg_rem_2 = mean(abs(rem_2)),
              avg_rem_3 = mean(abs(rem_3)),
              avg_rem_4 = mean(abs(rem_4))) %>%
    inner_join(configs, by = "config_id") %>%
    dplyr::select(config_id, n, px, pz, s_beta, s.j, cor_hv, type, cvg, avg_rem_1, avg_rem_2, avg_rem_3, avg_rem_4)
}

res <- ingest()
res %>% cvg %>% print(n=Inf)
res %>% diagnose.Theta %>% print(n=Inf)
res %>% diagnose.sd_u %>% print(n=Inf)
res %>% mse_beta %>% print(n=Inf)
res %>% diagnose_rems %>% print(n=Inf)


# ingest() %>%
#   summ %>%
#   dplyr::select(config_id, n, px, pz, s_beta, s.j, b, a, cor_hv, type, cvg1.la, SE1.la) %>%
#   print

# tbl <- filter(res$est,
#                estimator == "Debiased") %>%
#   cvg(res$stats, res$configs)
# tblS0 <- filter(res$est,
#                 estimator == "Debiased",
#                 beta0_j != 0) %>%
#   cvg(res$stats, res$configs)
# tblS0C <- filter(res$est,
#                 estimator == "Debiased",
#                 beta0_j == 0) %>%
#   cvg(res$stats, res$configs)
