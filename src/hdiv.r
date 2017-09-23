#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(MASS)
library(mvtnorm)
library(Matrix)
# library(methods)
library(lpSolve)
library(glmnet)
source("src/dgm.r", chdir = TRUE)
source("src/estimation.r", chdir = TRUE)
source("src/utils.r", chdir = TRUE)
source("src/trial.r", chdir = TRUE)

trial()
