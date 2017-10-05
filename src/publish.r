#!/usr/bin/env Rscript

#########################################################################
# Dependencies
suppressMessages(library(purrr))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(xtable))
suppressMessages(source("src/utils.r"))

#########################################################################

f.f1 <- function(x,y) {sprintf("%.3f (%.2f)", x,y)}
f.f2 <- function(x,y) {sprintf("%.2f (%.2f)", x,y)}
f.d <- function(x,y) {sprintf("(%d, %d)", x,y)}

tbl1 <- list(tbl, tblS0, tblS0C) %>%
  map(~ { mutate(., len = 2*SE * qnorm(1-.05/2)) %>%
            mutate(cvg.len = f.f1(cvg, len)) %>%
                   # SE.sd = f.f2(SE, sd)) %>%
            dplyr::select(config_id, cvg.len) }) %>%
  reduce(function(x,y){inner_join(x,y,by="config_id")}) %>%
  inner_join(res$configs, by="config_id") %>%
  filter(a == 1, b == 1) %>%
  mutate(ss = f.d(s_beta, s.j)) %>%
  dplyr::select(ss, cor_hv, type, cvg.len.x:cvg.len)
  # dplyr::select(n, px, pz, s_beta, s.j, b, a, cor_hv, type, cvg1.la.x:len1.la)

tbl1.CS <- filter(tbl1, type == "CS") %>%
  dplyr::select(-type)
tbl1.TZ <- filter(tbl1, type == "TZ") %>%
  dplyr::select(-type)

tbl1.CS %>%
  xtable %>%
  xtable(digits=c(0,0,1,rep(0,3))) %>%
  print(include.rownames=F, booktabs=T)
tbl1.TZ %>%
  xtable %>%
  xtable(digits=c(0,0,1,rep(0,3))) %>%
  print(include.rownames=F, booktabs=T)
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
