# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################################################################
# Dependencies

library(dplyr)
library(tidyr)
library(purrr)
library(MASS)
library(mvtnorm)

#########################################################################
# Data-generating mechanism

# First-stage regression coefficients
.alpha0_j <- function(pz, a, sj.min, sj.max) {
  s_j <- sample(sj.min:sj.max, size = 1)
  S_j <- sample(1:pz, size = s_j, replace = FALSE)
  alpha0_j <- numeric(pz) %>%
  { .[S_j] <- a; . }
  alpha0_j
}

# ... - arguments to alpha0_j:
#   pz, a, sj.min, sj.max
.Alpha0 <- function(px, ...) {
  Alpha0 <- map(1:px, ~ { .alpha0_j(...) }) %>%
    reduce(cbind) %>%
    { dimnames(.) <- NULL; . }
  Alpha0
}

# Second-stage regression coefficients
.beta0 <- function(px, b, s_beta) {
  S_beta <- sample(1:px, size = s_beta, replace = FALSE)
  beta0 <- numeric(px) %>%
    { .[S_beta] <- b; .}
  beta0
}


# types: (1) "TZ" (Toeplitz), (2) "CS" (circulant-symmetic), 
# (3) "ID" (identity), (4) "RN" (scaled Gram of m draws from multivariate-p Normal)
.Sigma_z <- function(pz, type, rho0 = .7, K = 5, m = 2*pz){
  if ( type == "TZ" ) {
    # TZ
    Sigma_z <- rho0^abs(outer(1:(pz), 1:(pz), "-"))
  } else if ( type == "CS" ) {
    # CS
    Sigma_z <- array(dim = c(pz, pz))
    for (j in 1:pz){
      for (k in j:pz){
        if (k == j){
          Sigma_z[j, k] <- 1
        }
        else if (( k %in% (j+1):(j+K)) || (k %in% (j+pz-K):(j+pz-1)) ){
          Sigma_z[j, k] <- .1
        }
        else {
          Sigma_z[j, k] <- 0
        }
      }
    }
    for (k in 1:pz){
      for (j in k:pz){
        Sigma_z[j, k] <- Sigma_z[k, j]
      }
    }
  } else if ( type == "EC" ) {
    # ...
  }
  Sigma_z
}

# hvcorstr: "
Sigma.hv. <- function(px, sigma0_v, sigma0_h, cor_hv, corstr) {
  sigma0_hv <- sigma0_v * sigma0_h * cor_hv
  if ( length(sigma0_v == 1) ) {
    Sigma0_v <- diag(sigma0_v^2, px)
  } else if ( length(sigma0_v == px) ) {
    Sigma0_v <- diag(sigma0_v^2) 
  } else { simpleError("Unsuitable length for sigma0_v") }
        
  if ( corstr == "c1" ) {
    corstr. <- rep(1, px)
  } else if (corstr == "c2") {
    corstr. <- runif(px, min = .9, max = 1.1)
  } else {
    # ...
  }
  
  top <- c(sigma0_h^2, sigma0_hv * corstr.)
  bottom <- cbind(sigma0_hv * corstr., Sigma0_v)
  Sigma_hv <- rbind(top, bottom)
  Sigma_hv
}

# ... = sigma0_v, sigma0_h, cor_hv, corstr
.hV <- function(n, Sigma.hv) {
  # Sigma_hv <- .Sigma_hv(px, ...)
  hV <- rmvnorm(n, rep(0, ncol(Sigma.hv)), Sigma.hv, method = "svd")
  list(h = hV[,1], V = hV[,2:ncol(hV)])
}

.Sigma_z.Z <- function(n, pz, type, ...) {
  Sigma_z <- .Sigma_z(pz = pz, type = type, ...)
  Z <- rmvnorm(n, rep(0, pz), Sigma_z)
  list(Sigma_z = Sigma_z, Z = Z)
}

.D <- function(Z, Alpha0) { Z %*% Alpha0 }

# ... = sigma0_v, sigma0_h, cor_hv, corstr
.X.y <- function(D, beta0, Sigma.hv) {
  n <- nrow(D); px <- ncol(D)
  hV <- .hV(n, Sigma.hv)
  h <- hV$h; V <- hV$V;
  X <- D + V
  y <- X %*% beta0 + h
  list(X = X, y = y, V = V, h = h)
}

#########################################################################
# Generate observations

.obs <- function(config_id.) {
  configs <- read.csv("config/configs.csv")
  config <- configs %>%
    filter(config_id == config_id.)
  
  Alpha0 <- read.table(paste("config", config_id., "Alpha0", sep = "/")) %>%
    as.matrix %>%
    { dimnames(.) <- NULL; . }
  beta0 <- read.table(paste("config", config_id., "beta0", sep = "/")) %>%
    as.matrix %>%
    { dimnames(.) <- NULL; . }
  Sigma.hv <- read.table(paste("config", config_id., "Sigma.hv", sep = "/")) %>%
    as.matrix %>%
    { dimnames(.) <- NULL; . }
  
  Sigma_z.Z <- .Sigma_z.Z(n = config$n, pz = config$pz, type = config$type)
  Z <- Sigma_z.Z$Z
  D <- .D(Z, Alpha0)
  
  X.y <- .X.y(D, beta0, Sigma.hv = Sigma.hv)
  X <- X.y$X; y <- X.y$y
  
  obs <- list(y = y, X = X, Z = Z, 
              sigma0_h = config$sigma0_h, sigma0_v = config$sigma0_v,
              beta0 = beta0)
  obs
}
