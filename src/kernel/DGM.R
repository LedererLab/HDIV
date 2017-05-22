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
.alpha0_j <- function(pz, a, s_jmin, s_jmax) {
  s_j <- sample(s_jmin:s_jmax, size = 1)
  S_j <- sample(1:pz, size = s_j, replace = FALSE)
  alpha0_j <- numeric(pz) %>%
  { .[S_j] <- a; . }
  alpha0_j
}

# ... - arguments to alpha0_j:
#   pz, a, s_jmin, s_jmax
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


# types: (1) "AC" (auto-correlative), (2) "CS" (circulant-symmetic), 
# (3) "ID" (identity), (4) "RN" (scaled Gram of m draws from multivariate-p Normal)
.Sigma_z <- function(pz, type = "AC", rho0 = .5, K = 5, m = 2*pz){
  if ( type == "AC" ) {
    # AC
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
  }
  Sigma_z
}

# hvcorstr: "
.Sigma_hv <- function(px, sigma0_v, sigma0_h, sigma0_hv_mult, corstr) {
  sigma0_hv <- sigma0_v * sigma0_h * sigma0_hv_mult
  if ( length(sigma0_v == 1) ) {
    Sigma0_v <- diag(sigma0_v^2, px)
  } else if ( length(sigma0_v == px) ) {
    Sigma0_v <- diag(sigma0_v^2) 
  } else { simpleError("Unsuitable length for sigma0_v") }
        
  if ( corstr == "c1" ) {
    corstr. <- rep(1, px)
  } else {
    # ...
  }
  
  top <- c(sigma0_h^2, sigma0_hv * corstr.)
  bottom <- cbind(sigma0_hv * corstr., Sigma0_v)
  Sigma_hv <- rbind(top, bottom)
  Sigma_hv
}

# ... = sigma0_v, sigma0_h, sigma0_hv_mult, corstr
.hV <- function(n, px, ...) {
  Sigma_hv <- .Sigma_hv(px, ...)
  hV <- rmvnorm(n, rep(0, ncol(Sigma_hv)), Sigma_hv, method = "svd")
  list(h = hV[,1], V = hV[,2:ncol(hV)], Sigma_hv = Sigma_hv)
}

.Sigma_z.Z <- function(n, pz, cov_type = "AC", ...) {
  Sigma_z <- .Sigma_z(pz, cov_type, ...)
  Z <- rmvnorm(n, rep(0, pz), Sigma_z)
  list(Sigma_z = Sigma_z, Z = Z)
}

.D <- function(Z, Alpha0) { Z %*% Alpha0 }

# ... = sigma0_v, sigma0_h, sigma0_hv_mult, corstr
.X.y <- function(D, beta0, ...) {
  n <- nrow(D); px <- ncol(D)
  hV <- .hV(n, px, ...)
  h <- hV$h; V <- hV$V; Sigma_hv = hV$Sigma_hv
  X <- D + V
  y <- X %*% beta0 + h
  list(X = X, y = y, V = V, h = h, Sigma_hv = Sigma_hv)
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
  
  Sigma_z.Z <- .Sigma_z.Z(n = config$n, pz = config$pz, type = config$type)
  Z <- Sigma_z.Z$Z
  D <- .D(Z, Alpha0)
  
  X.y <- .X.y(D, beta0, sigma0_v = config$sigma0_v, sigma0_h = config$sigma0_h, 
              sigma0_hv_mult = config$sigma0_hv_mult, corstr = config$corstr)
  X <- X.y$X; y <- X.y$y
  
  obs <- list(y = y, X = X, Z = Z, 
              sigma0_h = config$sigma0_h, sigma0_v = config$sigma0_v,
              beta0 = beta0)
  obs
}
