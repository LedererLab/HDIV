#!/usr/bin/env Rscript
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
source("../kernel/DGM.R", chdir = TRUE)

configure <- function() {
  args = commandArgs(trailingOnly = TRUE)
  config_dir <- args[1]
  .config_id <- args[2] %>% as.numeric
  configs <- read.csv(paste(config_dir, "configs.csv", sep="/"))
  config <- configs %>%
    filter(config_id == .config_id)
  
  Alpha0 <- .Alpha0(config$px, config$pz, config$a, config$sj.min, config$sj.max)
  beta0 <- .beta0(config$px, config$b, config$s_beta)
  
  write.matrix(Alpha0, paste(config_dir, .config_id, "Alpha0", sep = "/"))
  write.matrix(beta0, paste(config_dir, .config_id, "beta0", sep = "/"))
}

configure()
