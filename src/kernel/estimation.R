# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################################################################
# Dependencies

library(purrr)
library(glmnet)

#########################################################################
# Tuning parameter

.Lambdahat <- function(X, t = .05, m = 500){
  n <- nrow(X)
  g <- rnorm(n * m, 0, 1)
  map(1:m, ~ (X * rnorm(n, 0, 1)) %>% 
        apply(2, sum) %>% 
        max) %>%
    as.numeric %>%
    quantile(1 - t)
}

.lambda <- function(X, sigma0hat, t = 0.05, c = 1.1) {  2 * c * sigma0hat * .Lambdahat(X, t = t) / nrow(X) }

.sigma0_hhat <- function(y, x, beta_est) { sd(y - x %*% beta_est) }

# .sigma0_hhat <- function(Y, X, t = .05, c = 1.1, psi = .1, K = 100){
#   n <- nrow(X)
#   p <- ncol(X) + 1
#   Lambdahat <- .Lambdahat(X, t = t)
#   sigma0hat_k0 <- Inf
#   sigma0hat_k1 <- psi * sd(Y)
#   nu <- .2 * sd(Y)
#   # sigma0hats <- numeric(K)
#   k <- 1
#   fit <- glmnet(X, Y)
#   while ( abs(sigma0hat_k1 - sigma0hat_k0) > nu & k < K ) {
#     sigma0hat_k0 <- sigma0hat_k1
#     lambda <- 2 * c * sigma0hat_k1 * Lambdahat
#     sigma0hat_k1 <- (predict(fit, X, s = lambda/n, exact = TRUE, x = X, y = Y) - Y) %>% sd
#     k <- k + 1
#   }
# }

#########################################################################
# Estimation

# First-stage
.lambda_j.Alpha0hat <- function(X, Z, sigma0_v, tune_type = "CV"){
  px <- ncol(X); pz <- ncol(Z)
  if ( tune_type == "CV") {
    fits <- map(1:px, ~ cv.glmnet(Z, X[,.], intercept = FALSE, nfolds = 5)) %>%
      map(~ list(lambda_j = .$lambda.min,
                 alpha0_jhat = coef(., s = "lambda.min")))
  } else {
    if ( tune_type == "semioracle" ) {
      sigma0_vhat <- sigma0_v
    } else if ( tune_type == "feasible" ) {
      # ...
    }
    lambda <- .lambda(Z, sigma0_vhat)
    fits <- map(1:px, ~ list(X_j = X[,.], fit = glmnet(Z, X[,.], intercept = FALSE))) %>%
      map(~ list(lambda_j = lambda,
                 alpha0_jhat = predict(.$fit, type = "coefficients", 
                                       s = lambda, exact = TRUE, x = Z, y = .$X_j)))
  }
  Alpha0hat <- map(fits, ~ as.numeric(.$alpha0_jhat)[2:(pz+1)]) %>%
    reduce(cbind) %>% { colnames(.) <- NULL; . }
  list(lambda_j = map_dbl(fits, ~ .$lambda_j), Alpha0hat = Alpha0hat)
}

# Second-stage
.lambda.beta_Lasso <- function(y, Dhat, sigma0_h, tune_type = "CV") {
  px <- ncol(Dhat)
  if ( tune_type == "CV") {
    res <- cv.glmnet(Dhat, y, intercept = FALSE, nfolds = 5) %>%
    { list(lambda = .$lambda.min,
           beta_Lasso = as.numeric(coef(., s = "lambda.min"))[2:(px+1)]) }
  } else {
    if ( tune_type == "infeasible" ) {
      sigma0_hhat <- sigma0_h
    } else if ( tune_type == "feasible" ) {
      # ...
    }
    lambda <- .lambda(Dhat, sigma0_hhat)
    res <- glmnet(Dhat, y, intercept = FALSE) %>%
    { list(lambda = lambda,
           beta_Lasso = as.numeric(predict(., type = "coefficients", s = lambda, 
                                           exact = TRUE, x = Dhat, y = y))[2:(px+1)]) }
  }
  res
}

# Empirical Gram matrix
.Sigmahat <- function(x) { (t(x) %*% x) / nrow(x) }

# Relaxed inverse
# .mu_star.Thetahat <- function(Dhat) {
#   Sigma_dhat <- .Sigmahat(Dhat)
#   Thetahat <- InverseLinfty(gramhat, nrow(Dhat))
#   mu_star <- (Thetahat %*% Sigma_dhat - diag(1, ncol(Sigma_dhat))) %>% abs %>% max
#   mu_star.Thetahat <- list(mu_star = mu_star, Thetahat = Thetahat)
#   mu_star.Thetahat
# }

.Thetahat <- function(Dhat) {
  if ( nrow(Dhat) > ncol(Dhat) ) {
    Thetahat <- .Sigmahat(Dhat) %>% solve
  } else {
    Thetahat <- InverseLinfty(.Sigmahat(Dhat), nrow(Dhat))
  }
  Thetahat
}

# .mu.Thetahat <- function(x, tune_type = "CV", mu = NULL, nmu = 10) {
#   # gramhat <- .gramhat(Dhat)
#   if ( tune_type == "CV" ) {
#     mu.Thetahat <- clime(x, standardize = FALSE, lambda = mu, nlambda = nmu) %>%
#       { list(mu = .$lambda, 
#              Thetahat = .$Omegalist) }
#   }
#   mu.Thetahat
# }

# De-biased second-stage Lasso estimation

.beta_debiased <- function(y, X, Dhat, beta_Lasso, Thetahat) {
  n <- length(y)
  lambda_kappahat <- t(Dhat) %*% (y - Dhat %*% beta_Lasso) / n
  beta_debiased <- beta_Lasso + Thetahat %*% lambda_kappahat + (Thetahat %*% t(Dhat) %*% (Dhat - X) %*% beta_Lasso)/n
  beta_debiased
}

.vhat <-function(Sigma_dhat, Thetahat) { 
  (Thetahat %*% Sigma_dhat %*% t(Thetahat)) %>% diag %>% sqrt
}
# De-biased second-stage Lasso standard errors
.SE <- function(vhat, sigma0_hhat, n) { vhat * sigma0_hhat / sqrt(n) }


