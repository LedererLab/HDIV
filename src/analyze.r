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
  est <- read.csv("res/est.csv")
  stats <- read.csv("res/stats.csv")
  configs <- read.csv("configs/configs.csv")

  list(est = est, stats = stats, configs = configs)
}

cvg <-function(res) {
  est <- res$est; stats <- res$stats; configs <- res$configs
  est %>%
    filter(estimator == "Debiased") %>%
    inner_join(dplyr::select(configs, config_id, n, sigma0_h),
               by = "config_id") %>%
    inner_join(filter(stats, estimator == "Debiased") %>%
                 dplyr::select(config_id, trial_id),
               by = c("config_id", "trial_id")) %>%
    mutate(cvgj = covered(estimate_j, beta0_j, SE1.la)) %>%
    group_by(config_id, j) %>%
    summarize(cvgj = mean(cvgj),
              ntrials = n()) %>%
      # sdj = sd(estimate_j),
              # SEj = mean(SE1.la)) %>%
    group_by(config_id) %>%
    summarize(cvg = mean(cvgj),
              ntrials = mean(ntrials)) %>%
              # SE = mean(SEj),
              # sd = mean(sdj)) %>%
    inner_join(configs, by = "config_id") %>%
    dplyr::select(config_id, n, px, pz, s_beta, s.j, cor_hv, type, cvg, ntrials) %>%
    arrange(type)
}

ingest() %>% cvg %>% print(n=Inf)


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
