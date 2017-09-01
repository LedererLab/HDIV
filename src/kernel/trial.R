# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################################################################
# Dependencies

# source("DGM.R", chdir = TRUE)
# source("estimation.R", chdir = TRUE)
# source("utils.R", chdir = TRUE)

trial <- function(res_dir, tau=1.1) {
  args = commandArgs(trailingOnly=TRUE)
  config_id. <- args[1] %>% as.numeric
  trial_id <- Sys.getenv('SLURM_ARRAY_TASK_ID') %>% as.numeric
  obs <- .obs(config_id)
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
  Sigma_d.hat <- .Sigmahat(Dhat)
  mus <- find_mu(Sigma_d.hat) * tau
  Theta.hat <- .Theta.hat(Sigma_d.hat, mus)

  # de-biased second-stage lasso estimation
  beta_debiased <- .beta_debiased(y, X, Dhat, beta_Lasso_Dhat, Theta.hat)

  u.hat.db <- y - X %*% beta_debiased
  u.hat.la <- y - X %*% beta_Lasso_Dhat
  sd.u.hat.db <- u.hat.db^2 %>% mean %>% sqrt
  sd.u.hat.la <- u.hat.la^2 %>% mean %>% sqrt
  # SE <- .SE(Dhat, Theta.hat, u.hat)
  # h.hat <- h.hat.(Dhat, Theta.hat, u.hat)
  SE1.db <- h.hat1.(Dhat, Theta.hat, u.hat.db) / sqrt(n)
  SE1.la <- h.hat1.(Dhat, Theta.hat, u.hat.la) / sqrt(n)
  SE2.db <- h.hat2.(Sigma_d.hat, Theta.hat) * sd.u.hat.db / sqrt(n)
  SE2.la <- h.hat2.(Sigma_d.hat, Theta.hat) * sd.u.hat.la / sqrt(n)
  # vhat <- .vhat(Sigma_d.hat, Theta.hat)

  # estimator data
  df_est <- data.frame(
    config_id = rep(config_id, 2*px),
    trial_id = rep(trial_id, 2*px),
    estimator = c(rep("Debiased", px), rep("Lasso", px)),
    j = rep(1:px, 2),
    estimate_j = c(beta_debiased, beta_Lasso_Dhat),
    beta0_j = rep(beta0, 2),
    # SE = c(SE, rep(NA, px)),
    SE1.db = c(SE1.db, rep(NA, px)),
    SE1.la = c(SE1.la, rep(NA, px)),
    SE2.db = c(SE2.db, rep(NA, px)),
    SE2.la = c(SE2.la, rep(NA, px)),
    # vhat = rep(vhat, 2),
    lambda_j = rep(lambda_j, 2)
  )

  # statistics
  mu_star <- (Theta.hat %*% Sigma_d.hat - diag(1, ncol(Sigma_d.hat))) %>% abs %>% max
  mse_debiased <- (y - X %*% beta_debiased)^2 %>% mean
  mse_Lasso_Dhat <- (y - X %*% beta_Lasso_Dhat)^2 %>% mean
  # trial_cvg <- df_est %>%
  #   filter(estimator == "Debiased") %>%
  #   summarize(avg_cvg = mean(covered(estimate_j, beta0_j, SE))) %>%
  #   as.numeric

  # estimation statistics data
  df_stats <- data.frame(
    config_id = rep(config_id, 2),
    trial_id = rep(trial_id, 2),
    estimator = c("Debiased", "Lasso"),
    mse = c(mse_debiased, mse_Lasso_Dhat),
    # mu = c(mu, NA),
    tau = c(tau, NA),
    mu_star = c(mu_star, NA),
    # trial_cvg = c(trial_cvg, NA),
    lambda = rep(lambda, 2)
  )

  write.csv(df_est, paste(res_dir, "/est", config_id, "_", trial_id, ".csv", sep=""))
  write.csv(df_stats, paste(res_dir, "/stats", config_id, "_", trial_id, ".csv", sep=""))
  # print(df_est)
}
