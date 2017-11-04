# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################################################################
# Dependencies

# library(purrr)
# library(glmnet)

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

.sigma0_u.hat <- function(y, x, beta_est) { sd(y - x %*% beta_est) }

#########################################################################
# Estimation

# First-stage
.lambda_j.Alpha0hat <- function(X, Z, sigma0_v, tune_type = "CV"){
  px <- ncol(X); pz <- ncol(Z)
  if ( tune_type == "CV") {
    # fits <- map(1:px, ~ cv.glmnet(Z, X[,.], intercept = FALSE, nfolds = 5)) %>%
    # ncores <- 100
    # cl <- makeCluster(ncores, type="FORK")
    # fits <- parLapply(cl, 1:px, function(j) cv.glmnet(Z, X[,j], intercept=F, nfolds=5))
    # stopCluster(cl)
    fits <- lapply(1:px, function(j) cv.glmnet(Z, X[,j], intercept=F, nfolds=5))
    fits <- fits %>%
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
.lambda.beta_Lasso <- function(y, Dhat, sigma0_u, tune_type = "CV", no_pen_ids = c()) {
  px <- ncol(Dhat)
  if ( length(no_pen_ids)==0 ) {
    penalties <- rep(1, px)
  } else {
    penalties <- rep(1, px); penalties[no_pen_ids] <- 0
  }
  if ( tune_type == "CV") {
    res <- cv.glmnet(Dhat, y, intercept = FALSE, standardize=F, nfolds = 5, penalty.factor=penalties) %>%
    { list(lambda = .$lambda.min,
           beta_Lasso = as.numeric(coef(., s = "lambda.min"))[2:(px+1)]) }
  } else {
    if ( tune_type == "infeasible" ) {
      sigma0_u.hat <- sigma0_u
    } else if ( tune_type == "feasible" ) {
      # ...
    }
    lambda <- .lambda(Dhat, sigma0_u.hat)
    res <- glmnet(Dhat, y, intercept = FALSE, penalty.factor=penalties) %>%
    { list(lambda = lambda,
           beta_Lasso = as.numeric(predict(., type = "coefficients", s = lambda,
                                           exact = TRUE, x = Dhat, y = y))[2:(px+1)]) }
  }
  res
}

# Empirical Gram matrix
.Sigmahat <- function(x) { (t(x) %*% x) / nrow(x) }

find_mus <- function(Sigma.hat) {
  px <- ncol(Sigma.hat)
  Id <- diag(1, px)
  zeros <- matrix(ncol=px,nrow=px); zeros[,] <- 0

  c1 <- cbind(Sigma.hat, -1*Sigma.hat, -1*Id, rep(0, px))
  c2 <- cbind(-1*Sigma.hat, Sigma.hat, -1*Id, rep(0, px))
  c3 <- cbind(zeros, zeros, diag(1, px), rep(-1, px))
  A <- rbind(c1, c2, c3)

  obj <- c(rep(0, 3*px), 1)
  dir <- rep("<=", 3*px)
  mus <- lapply(
    1:px,
    function(j) {
      b1.j <- rep(0,px); b1.j[j] <- -1
      b2.j <- rep(0,px); b2.j[j] <- 1
      b3.j <- rep(0,px)
      b.j <- c(b1.j, b2.j, b3.j)
      res.j <- lp(direction="min", objective.in=obj, const.mat=A, const.dir=dir, const.rhs=b.j)
      res.j$objval
    }
  ) %>% as.numeric
  mus
}

.Theta.hat_JM <- function(Sigma.hat, n) {
  Theta.hat <- InverseLinfty(Sigma.hat, n)
  Theta.hat
}


.Theta.hat_CLIME <- function(Sigma.hat, mus) {
  #################################
  # lpSolve method

  # px <- ncol(Sigma.hat)
  # c1 <- lapply(1:px,
  #              function(j) { a <- rep(0,3*px); a[j] <- -1; a[j+px] <- 1; a[j+2*px] <- -1; a } ) %>%
  #   reduce(rbind)
  # c2 <- lapply(1:px,
  #              function(j) { a <- rep(0,3*px); a[j] <- 1;a[j+px] <- -1; a[j+2*px] <- -1; a } ) %>%
  #   reduce(rbind)
  # c3 <- lapply(1:px,
  #              function(j) { c(-1*Sigma.hat[j,], Sigma.hat[j,], rep(0, px)) }) %>%
  #   reduce(rbind)
  # c4 <- lapply(1:px,
  #              function(j) { c(Sigma.hat[j,], -1*Sigma.hat[j,], rep(0, px)) }) %>%
  #   reduce(rbind)
  # A <- rbind(c1, c2, c3, c4)
  # dir <- rep("<=", 4*px)
  # obj <- c(rep(0,px), rep(0,px), rep(1,px))
  #
  # Theta.hat <- lapply(
  #   1:px,
  #   function(j) {
  #     mu <- mus[j]
  #     b1 <- rep(mu, px); b1[j] = mu-1
  #     b2 <- rep(mu, px); b2[j] = mu+1
  #     b <- c(rep(0, 2*px), b1, b2)
  #     res_j <- lp(direction="min", objective.in=obj, const.mat=A, const.dir=dir, const.rhs=b)
  #     theta_j <- res_j$solution[1:px] - res_j$solution[(px+1):(2*px)]
  #     theta_j
  #   }
  # ) %>%
  #   reduce(rbind)
  # Theta.hat

  #################################
  # Mosek method

  px <- ncol(Sigma.hat)
  Id <- diag(1, px)
  zeros <- matrix(ncol=px,nrow=px); zeros[,] <- 0
  Theta.hat <- matrix(ncol=px,nrow=px)

  # constraint matrix
  A1 <- cbind(Id, -1*Id)
  A2 <- cbind(-1*Id, -1*Id)
  A3 <- cbind(Sigma.hat, zeros)
  A4 <- cbind(-1*Sigma.hat, zeros)
  A <- rbind(A1, A2, A3, A4) %>%
    Matrix(sparse=T)

  # lower and upper bounds on variables
  blx <- c(rep(-Inf, px), rep(0, px))
  bux <- rep(Inf, 2*px)

  objective <- c(rep(0,px), rep(1,px))

  for ( j in 1:px ) {
    mu <- mus[j]

    # lower and upper bounds on constraints
    blc <- rep(-Inf, 4*px)
    buc3 <- rep(mu, px); buc3[j] <- mu+1
    buc4 <- rep(mu, px); buc4[j] <- mu-1
    buc <- c(rep(0, 2*px), buc3, buc4)

    mosek.prob <- list(
      sense="min",
      c=objective,
      A=A,
      bc=rbind(blc, buc),
      bx=rbind(blx, bux)
      )
    mosek.res <- try(mosek(mosek.prob, list(verbose=0)), silent=TRUE)
    Theta.hat[j,] <- mosek.res$sol$bas$xx[1:px]
  }
  Theta.hat
}

# De-biased second-stage Lasso estimation

.beta_debiased <- function(y, X, Dhat, beta_Lasso, Thetahat) {
  n <- length(y)
  lambda_kappahat <- t(Dhat) %*% (y - Dhat %*% beta_Lasso) / n
  beta_debiased <- beta_Lasso + Thetahat %*% lambda_kappahat + (Thetahat %*% t(Dhat) %*% (Dhat - X) %*% beta_Lasso)/n
  beta_debiased
}

#########################################################################
# Standard errors

# SE1
# This is given by sigma_u.hat * theta_j^T * Sigma_d.hat * theta_j
.SE1 <-function(Sigma_d.hat, Theta.hat, sd_u.hat) {
  sd_u.hat * (Theta.hat %*% Sigma_d.hat %*% t(Theta.hat)) %>% diag %>% sqrt
}

# SE2
# This is given by E_n[{theta_j, d_i}^2u_i^2] %>% sqrt
.SE2 <- function(D.hat, Theta.hat, u.hat) {
  # n <- length(u.hat); p <- ncol(D.hat)
  (Theta.hat %*% t(D.hat)) %>%
    apply(1, function(X) { mean(X^2 * u.hat^2) }) %>%
    sqrt
}

# SE3
# This is given by sigma_u.hat *
.SE3 <- function(Theta.hat, sd_u.hat) {
  sqrt(diag(Theta.hat)) * sd_u.hat
}
