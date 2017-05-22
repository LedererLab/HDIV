#!/usr/bin/env Rscript
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
source("configs.R")
source("../kernel/DGM.R", chdir = TRUE)

configure <- function() {
  args = commandArgs(trailingOnly = TRUE)
  config_id. <- args[1] %>% as.numeric
  configs <- read.csv("configs.csv")
  config <- configs %>%
    filter(config_id == config_id.)
  
  Alpha0 <- .Alpha0(config$px, config$pz, config$a, config$s_jmin, config$s_jmax)
  beta0 <- .beta0(config$px, config$b, config$s_beta)
  
  write.matrix(Alpha0, paste(config_id., "Alpha0", sep = "/"))
  write.matrix(beta0, paste(config_id., "beta0", sep = "/"))
}

configure()
