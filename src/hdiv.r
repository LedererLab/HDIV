#!/usr/bin/env Rscript

library(methods)
# library(Matrix)
library(dplyr)
library(purrr)
library(MASS)
library(mvtnorm)
library(lpSolve)
library(glmnet)
library(Rmosek)
source("src/dgm.r", chdir = TRUE)
source("src/estimation.r", chdir = TRUE)
source("src/utils.r", chdir = TRUE)
source("src/trial.r", chdir = TRUE)

trial()
