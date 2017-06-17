# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################################################################
# Dependencies

source("DGM.R", chdir = TRUE)
source("estimation.R", chdir = TRUE)
source("utils.R", chdir = TRUE)

#########################################################################
# Simulation

trial <- function(res_dir) {
  # configs <- read.csv("config/configs.csv")
  args = commandArgs(trailingOnly=TRUE)
  config_id. <- args[1] %>% as.numeric
  trial_id <- Sys.getenv('SLURM_ARRAY_TASK_ID') %>% as.numeric
  # config_id. <- 3
  # trial_id <- 1
  
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
  lambda <- lambda.beta_Lasso$lambda; beta_Lasso_Dhat <- lambda.beta_Lasso$beta_Lasso
  
  # relaxed inverse estimation
  Sigma_dhat <- .Sigmahat(Dhat)
  Thetahat <- .Thetahat(Dhat)
  
  # de-biased second-stage lasso estimation
  beta_debiased <- .beta_debiased(y, X, Dhat, beta_Lasso_Dhat, Thetahat)
  sigma0_hhat <- .sigma0_hhat(y, X, beta_debiased)
  
  u.hat <- y - X %*% beta_debiased
  # SE <- .SE(Dhat, Thetahat, u.hat)
  # h.hat <- h.hat.(Dhat, Thetahat, u.hat)
  SE.db <- h.hat.(Dhat, Thetahat, u.hat) / sqrt(n)
  SE.la <- h.hat.(Dhat, Thetahat, y - X %*% beta_Lasso_Dhat) / sqrt(n)
  # vhat <- .vhat(Sigma_dhat, Thetahat)
  
  # estimator data
  df_est <- data.frame(
    config_id = rep(config_id., 2*px),
    trial_id = rep(trial_id, 2*px),
    estimator = c(rep("Debiased", px), rep("Lasso_Dhat", px)),
    j = rep(1:px, 2),
    estimate_j = c(beta_debiased, beta_Lasso_Dhat),
    beta0_j = rep(beta0, 2),
    # SE = c(SE, rep(NA, px)),
    SE.db = c(SE.db, rep(NA, px)),
    SE.la = c(SE.la, rep(NA, px)),
    # vhat = rep(vhat, 2),
    lambda_j = rep(lambda_j, 2)
  )
  
  # statistics
  mu_star <- (Thetahat %*% Sigma_dhat - diag(1, ncol(Sigma_dhat))) %>% abs %>% max
  mse_debiased <- (y - X %*% beta_debiased)^2 %>% mean
  mse_Lasso_Dhat <- (y - X %*% beta_Lasso_Dhat)^2 %>% mean
  # trial_cvg <- df_est %>%
  #   filter(estimator == "Debiased") %>%
  #   summarize(avg_cvg = mean(covered(estimate_j, beta0_j, SE))) %>%
  #   as.numeric
  
  # estimation statistics data
  df_stats <- data.frame(
    config_id = rep(config_id., 2),
    trial_id = rep(trial_id, 2),
    estimator = c("Debiased", "Lasso"),
    mse = c(mse_debiased, mse_Lasso_Dhat),
    mu_star = rep(mu_star, 2),
    # trial_cvg = c(trial_cvg, NA),
    sigma0_hhat = c(.sigma0_hhat(y, X, beta_debiased), .sigma0_hhat(y, X, beta_Lasso_Dhat)),
    lambda = rep(lambda, 2)
  )
  
  write.csv(df_est, paste(res_dir, config_id., "/est/est", trial_id, ".csv", sep=""))
  write.csv(df_stats, paste(res_dir, config_id., "/stats/stats", trial_id, ".csv", sep=""))
  # print(df_est)
}
