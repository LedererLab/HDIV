# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################################################################
# Dependencies

source("DGM.R", chdir = TRUE)
source("estimation.R", chdir = TRUE)
source("utils.R", chdir = TRUE)

#########################################################################
# Simulation

trial <- function() {
  # configs <- read.csv("config/configs.csv")
  args = commandArgs(trailingOnly=TRUE)
  config_id. <- args[1] %>% as.numeric
  trial_id <- Sys.getenv('SLURM_ARRAY_TASK_ID') %>% as.numeric
  
  obs <- .obs(config_id.)
  y <- obs$y; X <- obs$X; Z <- obs$Z;
  n <- nrow(X); px <- ncol(X); pz <- ncol(Z)
  sigma0_h <- obs$sigma0_h; sigma0_v <- obs$sigma0_v
  beta0 <- obs$beta0
  
  # first stage estimation
  lambda_j.Alpha0hat <- .lambda_j.Alpha0hat(X, Z, sigma0_v, tune_type = "CV")
  lambda_j <- lambda_j.Alpha0hat$lambda_j; Alpha0hat <- lambda_j.Alpha0hat$Alpha0hat
  
  # second-stage estimation
  Dhat <- Z %*% Alpha0hat
  lambda.beta_Lasso <- .lambda.beta_Lasso(y, Dhat, sigma0_h)
  lambda <- lambda.beta_Lasso$lambda; beta_Lasso <- lambda.beta_Lasso$beta_Lasso
  
  # relaxed inverse estimation
  Sigma_dhat <- .Sigma_dhat(Dhat)
  Thetahat <- .Thetahat(Dhat)
  
  # de-biased second-stage lasso estimation
  beta_debiased <- .beta_debiased(y, X, Dhat, beta_Lasso, Thetahat)
  sigma0_hhat <- .sigma0_hhat(y, X, beta_debiased)
  SE <- .SE(Sigma_dhat, Thetahat, sigma0_hhat, n)
  
  # estimator data
  df_est <- data.frame(
    config_id <- rep(config_id., 2*px),
    trial_id = rep(trial_id, 2*px),
    estimator = c(rep("Debiased", px), rep("Lasso", px)),
    j = rep(1:px, 2),
    estimate_j = c(beta_debiased, beta_Lasso),
    beta0_j = rep(beta0, 2),
    SE = c(SE, rep(NA, px)),
    lambda_j = rep(lambda_j, 2)
  )
  
  # statistics
  mu_star <- (Thetahat %*% Sigma_dhat - diag(1, ncol(Sigma_dhat))) %>% abs %>% max
  mse_debiased <- (y - X %*% beta_debiased)^2 %>% mean
  # mse_Lasso <- (y - X %*% beta_Lasso)^2 %>% mean
  SE <- .SE(Sigma_dhat, Thetahat, sigma0_hhat, n)
  trial_cvg <- df_est %>%
    filter(estimator == "Debiased") %>%
    summarize(avg_cvg = mean(covered(estimate_j, beta0_j, SE))) %>%
    as.numeric
  
  # estimation statistics data
  df_stats <- data.frame(
    config_id = config_id.,
    trial_id = trial_id,
    estimator = "Debiased",
    mse_debiased = mse_debiased,
    mu_star = mu_star,
    trial_cvg = trial_cvg,
    sigma0_hhat = sigma0_hhat,
    lambda = lambda
  )
  
  res_dir <- paste("res", config_id., "", sep = "/")
  write.csv(df_est, paste(res_dir, "est/", trial_id, ".csv", sep=""))
  write.csv(df_stats, paste(res_dir, "stats/", trial_id, ".csv", sep=""))
}
