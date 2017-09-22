#!/usr/bin/env Rscript
#########################################################################
# Dependencies
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(MASS))
suppressMessages(source("src/dgm.r"))

#########################################################################

configure <- function() {
  args = commandArgs(trailingOnly = TRUE)
  .config_id <- args[1] %>% as.numeric
  configs <- read.csv("configs/configs.csv")
  config <- configs %>%
    filter(config_id == .config_id)

  Alpha0 <- .Alpha0(config$px, config$pz, config$a, config$s.j)
  beta0 <- .beta0(config$px, config$b, config$s_beta)
  Sigma.hv <- Sigma.hv.(config$px, config$sigma0_v, config$sigma0_h, config$cor_hv, config$corstr)

  # sprintf("Generating parameters for configuration %d", .config_id) %>% print
  write.matrix(Alpha0, paste("configs", .config_id, "Alpha0", sep = "/"))
  write.matrix(beta0, paste("configs", .config_id, "beta0", sep = "/"))
  write.matrix(Sigma.hv, paste("configs", .config_id, "Sigma.hv", sep = "/"))
}

configure()
