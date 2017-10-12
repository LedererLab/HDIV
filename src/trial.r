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
  lambda.beta_Lasso <- .lambda.beta_Lasso(y, D.hat, sigma0_h, no_pen_ids=c(1,2))
  lambda <- lambda.beta_Lasso$lambda; beta_Lasso_D.hat <- lambda.beta_Lasso$beta_Lasso

  # relaxed inverse estimation
  Sigma_d.hat <- .Sigmahat(D.hat)
  mus <- find_mu(Sigma_d.hat) * tau
  Id <- diag(1, px) # Identity matrix

  Theta.hat_CLIME <- .Theta.hat_CLIME(Sigma_d.hat, mus)
  mu_stars_CLIME <- map_dbl(1:px,
    ~ { (Sigma_d.hat %*% Theta.hat_CLIME[.,] - Id[.,]) %>% abs %>% max})
  objs_CLIME <- map_dbl(1:px,
    ~ { Theta.hat_CLIME[.,] %>% abs %>% sum })

  Theta.hat_JM <- .Theta.hat_JM(Sigma_d.hat, n)
  mu_stars_JM <- map_dbl(1:px,
    ~ { (Sigma_d.hat %*% Theta.hat_JM[.,] - Id[.,]) %>% abs %>% max})
  objs_JM <- map_dbl(1:px,
    ~ { Theta.hat_JM[.,] %>% abs %>% sum })

  # de-biased second-stage lasso estimation
  beta_debiased_CLIME <- .beta_debiased(y, X, D.hat, beta_Lasso_D.hat, Theta.hat_CLIME)
  # beta_debiased_JM <- .beta_debiased(y, X, D.hat, beta_Lasso_D.hat, Theta.hat_JM)

  # Predict the second-stage noise
  u.hat <- y - X %*% beta_Lasso_D.hat
  sd_u.hat <- u.hat^2 %>% mean %>% sqrt

  # To record/compute estimate of Theta_jj (and Theta_jj)
  Theta <- solve(t(Alpha0) %*% Sigma_z %*% Alpha0)

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
    mu_stars = c(mu_stars_CLIME, mu_stars_JM, rep(NA, px)),
    objs = c(objs_CLIME, objs_JM, rep(NA, px))
    lambda_j = rep(lambda_j, 3)
  # )
  # df_est <- data.frame(
  #   config_id = rep(config_id, 2*px),
  #   trial_id = rep(trial_id, 2*px),
  #   estimator = c(rep("Debiased_CLIME", px), rep("Lasso", px)),
  #   j = rep(1:px, 2),
  #   estimate_j = c(beta_debiased_CLIME, beta_Lasso_D.hat),
  #   beta0_j = rep(beta0, 2),
  #   SE1 = c(.SE1(Sigma_d.hat, Theta.hat_CLIME, sd_u.hat),
  #           rep(NA, px)),
  #   SE2 = c(.SE2(D.hat, Theta.hat_CLIME, u.hat),
  #           rep(NA, px)),
  #   SE3 = c(.SE3(Theta.hat_CLIME, sd_u.hat),
  #           rep(NA, px)),
  #   Theta_jj = c(diag(Theta), rep(NA, px)),
  #   Theta.hat_jj = c(diag(Theta.hat_CLIME), rep(NA, px)),
  #   mu_stars = c(mu_stars_CLIME, rep(NA, px)),
  #   objs = c(objs_CLIME, rep(NA, px))
  #   # vhat = rep(vhat, 2),
  #   lambda_j = rep(lambda_j, 2)
  # )

  # statistics
  mu_star_CLIME <- (Theta.hat_CLIME %*% Sigma_d.hat - diag(1, ncol(Sigma_d.hat))) %>% abs %>% max
  # mu_star_JM <- (Theta.hat_JM %*% Sigma_d.hat - diag(1, ncol(Sigma_d.hat))) %>% abs %>% max
  mse_Debiased_CLIME <- (y - X %*% beta_debiased_CLIME)^2 %>% mean
  # mse_Debiased_JM <- (y - X %*% beta_debiased_JM)^2 %>% mean
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
  # df_stats <- data.frame(
  #   config_id = rep(config_id, 2),
  #   trial_id = rep(trial_id, 32),
  #   estimator = c("Debiased_CLIME", "Lasso"),
  #   mse = c(mse_Debiased_CLIME, mse_Lasso_D.hat),
  #   sd_u = rep(sd_u.hat, 2),
  #   # mu = c(mu, NA),
  #   tau = c(tau, NA),
  #   mu_star = c(mu_star_CLIME, NA),
  #   # trial_cvg = c(trial_cvg, NA),
  #   lambda = rep(lambda, 2)
  # )

  write.csv(df_est, paste("res/est_", config_id, "_", trial_id, ".csv", sep=""))
  write.csv(df_stats, paste("res/stats_", config_id, "_", trial_id, ".csv", sep=""))
}
