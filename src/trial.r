#########################################################################
# Simulation trial

trial <- function(tau=1.1) {
  args = commandArgs(trailingOnly=TRUE)
  config_id <- args[1] %>% as.numeric
  trial_id <- Sys.getenv('SLURM_ARRAY_TASK_ID') %>% as.numeric
  obs <- .obs(config_id)
  y <- obs$y; X <- obs$X; Z <- obs$Z;
  n <- nrow(X); px <- ncol(X); pz <- ncol(Z)
  Sigma_z <- obs$Sigma_z
  Alpha0 <- obs$Alpha0
  sigma0_h <- obs$sigma0_h; sigma0_v <- obs$sigma0_v
  beta0 <- obs$beta0

  # first stage estimation
  lambda_j.Alpha0hat <- .lambda_j.Alpha0hat(X, Z, sigma0_v, tune_type = "CV")
  lambda_j <- lambda_j.Alpha0hat$lambda_j; Alpha0hat <- lambda_j.Alpha0hat$Alpha0hat

  # second-stage estimation
  D.hat <- Z %*% Alpha0hat
  lambda.beta_Lasso <- .lambda.beta_Lasso(y, D.hat, sigma0_h)
  lambda <- lambda.beta_Lasso$lambda; beta_Lasso_D.hat <- lambda.beta_Lasso$beta_Lasso

  # relaxed inverse estimation
  Sigma_d.hat <- .Sigmahat(D.hat)
  mus <- find_mu(Sigma_d.hat) * tau
  Theta.hat_CLIME <- .Theta.hat_CLIME(Sigma_d.hat, mus)
  Theta.hat_JM <- .Theta.hat_JM(Sigma_d.hat, n)

  # de-biased second-stage lasso estimation
  beta_debiased_CLIME <- .beta_debiased(y, X, D.hat, beta_Lasso_D.hat, Theta.hat_CLIME)
  beta_debiased_JM <- .beta_debiased(y, X, D.hat, beta_Lasso_D.hat, Theta.hat_JM)

  # u.hat.db_CLIME <- y - X %*% beta_debiased_CLIME
  # u.hat.db_JM <- y - X %*% beta_debiased_JM
  # u.hat.la <- y - X %*% beta_Lasso_D.hat
  # sd.u.hat.db_CLIME <- u.hat.db_CLIME^2 %>% mean %>% sqrt
  # sd.u.hat.db_JM <- u.hat.db_JM^2 %>% mean %>% sqrt
  # sd.u.hat.la <- u.hat.la^2 %>% mean %>% sqrt
  # SE <- .SE(D.hat, Theta.hat, u.hat)
  # h.hat <- h.hat.(D.hat, Theta.hat, u.hat)
  u.hat <- y - X %*% beta_Lasso_D.hat
  sd_u.hat <- u.hat^2 %>% mean %>% sqrt

  # SE1.db <- h.hat1.(D.hat, Theta.hat, u.hat.db) / sqrt(n)
  # SE1.la <- h.hat1.(D.hat, Theta.hat, u.hat.la) / sqrt(n)
  # SE2.db <- h.hat2.(Sigma_d.hat, Theta.hat) * sd.u.hat.db / sqrt(n)
  # SE2.la <- h.hat2.(Sigma_d.hat, Theta.hat) * sd.u.hat.la / sqrt(n)
  # vhat <- .vhat(Sigma_d.hat, Theta.hat)
  # SE3.la <- diag(Theta.hat) * sd.u.hat.la / sqrt(n)
  # SE3.db <- diag(Theta.hat) * sd.u.hat.db / sqrt(n)

  # To record/compute estimate of Theta_jj (and Theta_jj)
  Theta <- t(Alpha0) %*% Sigma_z %*% Alpha0

  # estimator data
  df_est <- data.frame(
    config_id = rep(config_id, 3*px),
    trial_id = rep(trial_id, 3*px),
    estimator = c(rep("Debiased_CLIME", px), rep("Debiased_JM", px), rep("Lasso", px)),
    j = rep(1:px, 3),
    estimate_j = c(beta_debiased_CLIME, beta_debiased_JM, beta_Lasso_D.hat),
    beta0_j = rep(beta0, 3),
    SE1 = c(.SE1(Sigma_d.hat, Theta.hat_CLIME, sd_u.hat),
            .SE1(Sigma_d.hat, Theta.hat_JM, sd_u.hat),
            rep(NA, px)),
    SE2 = c(.SE2(D.hat, Theta.hat_CLIME, u.hat),
            .SE2(D.hat, Theta.hat_JM, u.hat),
            rep(NA, px)),
    SE3 = c(.SE3(Theta.hat_CLIME, sd_u.hat),
            .SE3(Theta.hat_JM, sd_u.hat),
            rep(NA, px)),
    Theta_jj = c(diag(Theta), diag(Theta), rep(NA, px)),
    Theta.hat_jj = c(diag(Theta.hat_CLIME), diag(Theta.hat_JM), rep(NA, px)),
    # vhat = rep(vhat, 2),
    lambda_j = rep(lambda_j, 3)
  )

  # statistics
  mu_star_CLIME <- (Theta.hat_CLIME %*% Sigma_d.hat - diag(1, ncol(Sigma_d.hat))) %>% abs %>% max
  mu_star_JM <- (Theta.hat_JM %*% Sigma_d.hat - diag(1, ncol(Sigma_d.hat))) %>% abs %>% max
  mse_Debiased_CLIME <- (y - X %*% beta_debiased_CLIME)^2 %>% mean
  mse_Debiased_JM <- (y - X %*% beta_debiased_JM)^2 %>% mean
  mse_Lasso_D.hat <- (y - X %*% beta_Lasso_D.hat)^2 %>% mean

  # estimation statistics data
  df_stats <- data.frame(
    config_id = rep(config_id, 3),
    trial_id = rep(trial_id, 3),
    estimator = c("Debiased_CLIME", "Debiased_JM", "Lasso"),
    mse = c(mse_Debiased_CLIME, mse_Debiased_JM, mse_Lasso_D.hat),
    sd_u = rep(sd_u.hat, 3),
    # mu = c(mu, NA),
    tau = c(tau, NA, NA),
    mu_star = c(mu_star_CLIME, mu_star_JM, NA),
    # trial_cvg = c(trial_cvg, NA),
    lambda = rep(lambda, 3)
  )

  write.csv(df_est, paste("res/est_", config_id, "_", trial_id, ".csv", sep=""))
  write.csv(df_stats, paste("res/stats_", config_id, "_", trial_id, ".csv", sep=""))
}
