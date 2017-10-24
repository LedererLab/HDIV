#########################################################################
# Data-generating mechanism

# First-stage regression coefficients
.alpha0_j <- function(pz, a, s.j) {
  S.j <- sample(1:pz, size = s.j, replace = FALSE)
  alpha0.j <- numeric(pz) %>%
  { .[S.j] <- a; . }
  alpha0.j
}

# ... - arguments to alpha0_j:
#   pz, a, sj.min, sj.max
.Alpha0 <- function(px, pz, a, s.j) {
  Alpha0 <- map(1:px, ~ { .alpha0_j(pz, a, s.j) }) %>%
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

Sigma.uv. <- function(px, sigma0_v, sigma0_u) {
  r <- 5
  b <- sigma0_u*sigma0_v
  v <- c(.5*b, rep(.25*b, r-1), rep(.25*b, r), rep(.05*b, px-2*r)) %>%
    sample
  V <- diag(sigma0_v, px)
  rbind(c(sigma0_u, v),
        cbind(v, V))
}

# ... = sigma0_v, sigma0_u, cor_hv, corstr
.uV <- function(n, Sigma.uv) {
  # Sigma_hv <- .Sigma_hv(px, ...)
  uV <- rmvnorm(n, rep(0, ncol(Sigma.uv)), Sigma.uv)
  list(u = uV[,1], V = uV[,2:ncol(uV)])
}

.Sigma_z.Z <- function(n, pz, type, ...) {
  Sigma_z <- .Sigma_z(pz = pz, type = type, ...)
  Z <- rmvnorm(n, rep(0, pz), Sigma_z)
  list(Sigma_z = Sigma_z, Z = Z)
}

.D <- function(Z, Alpha0) { Z %*% Alpha0 }

# ... = sigma0_v, sigma0_u, cor_hv, corstr
.X.y <- function(D, beta0, Sigma.uv) {
  n <- nrow(D); px <- ncol(D)
  uV <- .uV(n, Sigma.uv)
  u <- uV$u; V <- uV$V;
  X <- D + V
  y <- X %*% beta0 + u
  list(X = X, y = y, V = V, u = u)
}

#########################################################################
# Generate observations

.obs <- function(config_id.) {
  configs <- read.csv("configs/configs.csv")
  config <- configs %>%
    filter(config_id == config_id.)

  Alpha0 <- read.table(paste("configs", config_id., "Alpha0", sep = "/")) %>%
    as.matrix %>%
    { dimnames(.) <- NULL; . }
  beta0 <- read.table(paste("configs", config_id., "beta0", sep = "/")) %>%
    as.matrix %>%
    { dimnames(.) <- NULL; . }
  Sigma.uv <- read.table(paste("configs", config_id., "Sigma.uv", sep = "/")) %>%
    as.matrix %>%
    { dimnames(.) <- NULL; . }

  Sigma_z.Z <- .Sigma_z.Z(n = config$n, pz = config$pz, type = config$type)
  Sigma_z <- Sigma_z.Z$Sigma_z; Z <- Sigma_z.Z$Z
  D <- .D(Z, Alpha0)

  X.y <- .X.y(D, beta0, Sigma.uv = Sigma.uv)
  X <- X.y$X; y <- X.y$y

  obs <- list(y = y, X = X, Z = Z, D = D, u = X.y$h,
              sigma0_u = config$sigma0_u, sigma0_v = config$sigma0_v,
              beta0 = beta0, Sigma_z=Sigma_z, Alpha0=Alpha0)
  obs
}
