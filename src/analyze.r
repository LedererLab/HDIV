#!/usr/bin/env Rscript

#########################################################################
# Dependencies
suppressMessages(library(purrr))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(xtable))
suppressMessages(source("src/utils.r"))

#########################################################################
# Analysis

ingest <- function() {
  df_est <- read.csv("res/est.csv")
  df_stats <- read.csv("res/stats.csv")
  configs <- read.csv("configs/configs.csv")

  list(est = df_est, stats = df_stats, configs = configs)
}

cvg <-function(res) {
  est <- res$est; stats <- res$stats; configs <- res$configs
  est %>%
    inner_join(dplyr::select(res$configs, config_id, n, sigma0_h),
               by = "config_id") %>%
    inner_join(filter(res$stats, estimator == "Debiased") %>%
                 dplyr::select(config_id, trial_id, sigma0_hhat),
               by = c("config_id", "trial_id")) %>%
    mutate(cvgj = covered(estimate_j, beta0_j, SE1.la)) %>%
    group_by(config_id, j) %>%
    summarize(sdj = sd(estimate_j),
              cvgj = mean(cvgj),
              SEj = mean(SE1.la)) %>%
    group_by(config_id) %>%
    summarize(cvg = mean(cvgj),
              SE = mean(SEj),
              sd = mean(sdj)) %>%
    inner_join(res$configs, by = "config_id") %>%
    inner_join(group_by(res$stats, config_id) %>%
                 summarize(sigma0_hhat = mean(sigma0_hhat), by = "config_id"))
}
#

ingest() %>% cvg %>% print

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
