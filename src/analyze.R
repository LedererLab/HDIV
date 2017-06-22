#!/usr/bin/env Rscript
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################################################################
# Dependencies
#args <- commandArgs(trailingOnly = TRUE
suppressMessages(library(purrr))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(xtable))
#suppressMessages(source("kernel/utils.R"))

#########################################################################
# Analysis

ingest <- function() {
  # args <- commandArgs(trailingOnly = TRUE)
  # res_dir <- args[1]
  # res_dir <- "/Users/David/johannes/hdiv/res"
  res_dir <- "/homes/dag89/hdiv/res"
  # src_dir <- args[2]
  # proj_dir <- "/Users/David/johannes/hdiv"
  proj_dir <- "/homes/dag89/hdiv/" 
  suppressMessages(source(paste(proj_dir, "src/kernel/utils.R", sep = "/")))
  
  df_est <- paste(res_dir, "est.csv", sep = "/") %>% read.csv
  df_stats <- paste(res_dir, "stats.csv", sep = "/") %>% read.csv
  configs <- paste(proj_dir, "src/config/configs.csv", sep = "/") %>% read.csv
  
  # df_est <- read.csv("/Users/David/johannes/hdiv/res/old/2/est.csv")
  # df_stats <- read.csv("/Users/David/johannes/hdiv/res/old/2/stats.csv")
  # configs <- read.csv("/Users/David/johannes/hdiv/res/old/2/configs.csv")
  
  list(est = df_est, stats = df_stats, configs = configs)
}

summ <- function(res) {
  res$est %>%
    filter(estimator == "Debiased") %>%
    inner_join(dplyr::select(res$configs, config_id, n, sigma0_h), 
               by = "config_id") %>%
    inner_join(filter(res$stats, estimator == "Debiased") %>%
                 dplyr::select(config_id, trial_id, sigma0_hhat),
               by = c("config_id", "trial_id")) %>%
    group_by(config_id) %>%
    mutate(cvgj1.la = covered(estimate_j, beta0_j, SE1.la)) %>%
           # cvgj1.db = covered(estimate_j, beta0_j, SE1.db),
           # cvgj2.la = covered(estimate_j, beta0_j, SE2.la),
           # cvgj2.db = covered(estimate_j, beta0_j, SE2.db)) %>%
    # summarize(coverage_j = mean(covered(estimate_j, beta0_j, SE))) %>%
    # group_by(config_id) %>%
    summarize(cvg1.la = mean(cvgj1.la),
              # cvg1.db = mean(cvgj1.db),
              # cvg2.la = mean(cvgj2.la),
              # cvg2.db = mean(cvgj2.db),
              SE1.la = mean(SE1.la)) %>%
              # SE1.db = mean(SE1.db),
              # SE2.la = mean(SE2.la),
              # SE2.db = mean(SE2.db)) %>% 
    inner_join(res$configs, by = "config_id") %>%
    inner_join(group_by(res$stats, config_id) %>%
                 summarize(sigma0_hhat = mean(sigma0_hhat), by = "config_id"))
}

# res <- ingest()
# tbl <- analyze(res)
# tbl %>% 
ingest() %>%
  summ %>%
  dplyr::select(config_id, n, px, pz, s_beta, s.j, b, a, cor_hv, type, cvg1.la, SE1.la) %>%
  print

# tblS0 <- filter(res$est,
#                 estimator == "Debiased",
#                 beta0_j != 0) %>%
#   cvg(res$stats, res$configs)
# tblS0C <- filter(res$est,
#                 estimator == "Debiased",
#                 beta0_j == 0) %>%
#   cvg(res$stats, res$configs)
# 
# tbl %>%
#   transmute(SE2.la <= SE1.la) %>%
#   View
# 
# d <- tbl %>%
#   filter(corstr == "c2") %>%
#   dplyr::select(-corstr, -rmse, -X, -id, -(cvg1.db:cvg2.db), -(SE1.db:SE2.db)) %>%
#   mutate(SE1.la = SE1.la * qnorm(1-.05/2)) %>%
#   unite(cvg.len.sduhat, cvg1.la, SE1.la, sep="_") %>%
#   spread(type, cvg.len.sduhat, sep = "_") %>%
#   separate(type_CS, c("cvg.CS", "len.CS"), sep="_") %>%
#   separate(type_TZ, c("cvg.TZ", "len.TZ"), sep="_") %>%
#   dplyr::select(-(b:by), cor_hv)
# 
# tbl %>%
#   filter(corstr == "c2") %>%
#   dplyr::select(-config_id, -corstr, -rmse, -X, -id, -(cvg1.db:cvg2.db), -(SE1.db:SE2.db), -sigma0_hhat) %>%
#   mutate(SE1.la = 2*SE1.la * qnorm(1-.05/2)) %>%
#   unite(cvg.len, cvg1.la, SE1.la, sep="_") %>%
#   spread(type, cvg.len, sep = "_") %>%
#   separate(type_CS, c("cvg.CS", "len.CS"), sep="_") %>%
#   separate(type_TZ, c("cvg.TZ", "len.TZ"), sep="_") %>%
#   dplyr::select(-(b:by), cor_hv) %>%
#   head(24) %>%
#   mutate(foo = rep(1:8, each=3)) %>%
#   arrange(foo, desc(cor_hv)) %>%
#   mutate(s = (function(x,y,z) {sprintf("(%d, %d, %d)", x,y,z)}) (s_beta,sj.min,sj.max) ) %>%
#   mutate_at(vars(cvg.CS:len.TZ), funs(round(as.numeric(.), 3))) %>%
#   dplyr::select(s, cor_hv, cvg.CS:len.TZ) %>%
#   xtable(digits=c(0,0,1,rep(3,4))) %>%
#   print(include.rownames=F, booktabs=T)
# 
# 
# 
# tblS0 %>%
#   dplyr::select(-rmse, -X, -id, -(cvg1.db:cvg2.db), -(SE1.db:SE2.db)) %>%
#   View
# 
# tblS0C %>%
#   dplyr::select(-rmse, -X, -id, -(cvg1.db:cvg2.db), -(SE1.db:SE2.db)) %>%
#   View
# 
# cvg <-function(est, stats, configs) {
#   est %>%
#     inner_join(dplyr::select(configs, config_id, n, sigma0_h), 
#                by = "config_id") %>%
#     inner_join(filter(stats, estimator == "Debiased") %>%
#                  dplyr::select(config_id, trial_id, sigma0_hhat),
#                by = c("config_id", "trial_id")) %>%
#     group_by(config_id) %>%
#     mutate(cvgj1.la = covered(estimate_j, beta0_j, SE1.la),
#            cvgj1.db = covered(estimate_j, beta0_j, SE1.db),
#            cvgj2.la = covered(estimate_j, beta0_j, SE2.la),
#            cvgj2.db = covered(estimate_j, beta0_j, SE2.db)) %>%
#     # summarize(coverage_j = mean(covered(estimate_j, beta0_j, SE))) %>%
#     # group_by(config_id) %>%
#     summarize(cvg1.la = mean(cvgj1.la),
#               cvg1.db = mean(cvgj1.db),
#               cvg2.la = mean(cvgj2.la),
#               cvg2.db = mean(cvgj2.db),
#               # summarize(cvg.th = mean(cvgj.th),
#               #           cvg.dt = mean(cvgj.dt),
#               rmse = mean((estimate_j-beta0_j)^2) %>% sqrt,
#               # med.bias = quantile(estimate_j - beta0_j, 0.5)
#               # bias = mean(estimate_j-beta0_j),
#               SE1.la = mean(SE1.la),
#               SE1.db = mean(SE1.db),
#               SE2.la = mean(SE2.la),
#               SE2.db = mean(SE2.db)) %>% 
#     inner_join(configs, by = "config_id") %>%
#     inner_join(group_by(stats, config_id) %>%
#                  summarize(sigma0_hhat = mean(sigma0_hhat), by = "config_id"))
# }
# 
# 
# 
# tbl %>% 
#   dplyr::select(-config_id, -X, -id) %>% 
#   unite(cvg, cvg.th, cvg.dt, sigma0_hhat, sep = "_") %>%
#   spread(cor_hv, cvg, sep = "_") %>% 
#   separate(cor_hv_0.1, c("cvg.th.1", "cvg.dt.1", "sdh.hat.1"), sep = "_") %>%
#   separate(cor_hv_0.3, c("cvg.th.3", "cvg.dt.3", "sdh.hat.3"), sep = "_") %>% 
#   separate(cor_hv_0.5, c("cvg.th.5", "cvg.dt.5", "sdh.hat.5"), sep = "_") %>% 
#   mutate_at(vars(cvg.th.1:sdh.hat.5), funs(as.numeric(.))) %>%
#   # mutate_at(vars(cvg.th.1:sdh.hat.5), funs(round(as.numeric(.),3))) %>%
#   dplyr::select(n:sj.max, 
#                 cvg.th.1, cvg.th.3, cvg.th.5,
#                 cvg.dt.1, cvg.dt.3, cvg.dt.5,
#                 sdh.hat.1, sdh.hat.3, sdh.hat.5) %>%
#   # xtable %>%
#   # xtable(digits = c(0,rep(0, 6), rep(c(3,3,2), 3))) %>%
#   xtable(digits = c(0,rep(0, 6), rep(3, 6), rep(2, 3))) %>%
#   print(include.rownames=F, booktabs=T)
# 
# tbl %>% 
#   dplyr::select(-config_id, -X, -id) %>% 
#   unite(cvg, cvg.th, cvg.dt, sigma0_hhat, sep = "_") %>%
#   spread(cor_hv, cvg, sep = "_") %>% 
#   separate(cor_hv_0.1, c("cvg.th.1", "cvg.dt.1", "sdh.hat.1"), sep = "_") %>%
#   separate(cor_hv_0.3, c("cvg.th.3", "cvg.dt.3", "sdh.hat.3"), sep = "_") %>% 
#   separate(cor_hv_0.5, c("cvg.th.5", "cvg.dt.5", "sdh.hat.5"), sep = "_") %>% 
#   mutate_at(vars(cvg.th.1:sdh.hat.5), funs(as.numeric(.))) %>%
#   mutate(cvg.1 = sprintf("%.3f  (%.3f)", cvg.dt.1, cvg.th.1)) %>%
#   mutate(cvg.3 = sprintf("%.3f  (%.3f)", cvg.dt.3, cvg.th.3)) %>%
#   mutate(cvg.5 = sprintf("%.3f  (%.3f)", cvg.dt.5, cvg.th.5)) %>%
#   # mutate_at(vars(cvg.th.1:sdh.hat.5), funs(round(as.numeric(.),3))) %>%
#   dplyr::select(n:sj.max, 
#                 cvg.1, sdh.hat.1, cvg.3, sdh.hat.3, cvg.5, sdh.hat.5) %>%
#   # xtable %>%
#   # xtable(digits = c(0,rep(0, 6), rep(c(3,3,2), 3))) %>%
#   # xtable(digits = c(0,rep(0, 6), rep(3, 6), rep(2, 3))) %>%
#   xtable %>%
#   print(include.rownames=F, booktabs=T)
# 
# # Take 3
# tbl %>% 
#   dplyr::select(-config_id, -X, -id, -cvg.th) %>% 
#   unite(summ, cvg.dt, rmse, SE.dt, sigma0_hhat, sep = "_") %>%
#   spread(cor_hv, summ, sep = "_") %>% 
#   separate(cor_hv_0.1, c("cvg.1", "rmse.1", "SE.1", "sdh.hat.1"), sep = "_") %>%
#   separate(cor_hv_0.3, c("cvg.3", "rmse.3", "SE.3", "sdh.hat.3"), sep = "_") %>%
#   separate(cor_hv_0.5, c("cvg.5", "rmse.5", "SE.5", "sdh.hat.5"), sep = "_") %>%
#   mutate_at(vars(cvg.1:sdh.hat.5), funs(as.numeric(.))) %>%
#   dplyr::select(s_beta:sj.max, cvg.1:sdh.hat.5) %>%
#   # mutate_at(vars(cvg.th.1:sdh.hat.5), funs(round(as.numeric(.),3))) %>%
#   # xtable %>%
#   # xtable(digits = c(0,rep(0, 6), rep(c(3,3,2), 3))) %>%
#   xtable(digits = c(0,rep(0, 3), rep(c(3,3,2,2), 3))) %>%
#   print(include.rownames=F, booktabs=T)
# 
# 
