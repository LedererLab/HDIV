# dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
library(purrr)
library(dplyr)

make_configs <- function() {
  
  A1 <- data.frame(
    n = c(400, 400, 400, 600, 600, 600),
    px = c(300, 500, 600, 450, 750, 900),
    pz = c(500, 600, 800, 750, 900, 1200)
  )
  
  B1 <- data.frame(
    s_beta = c(10, 30),
    s_jmin = c(15, 40),
    s_jmax = c(20, 50)
  )
  
  C <- data.frame(
    b = c(1),
    a = c(1)
  )
  
  D <- data.frame(
    sigma_h = c(1),
    sigma0_v = c(1),
    sigma0_hv_mult = c(.5)
  )
  
  E <- data.frame(
    type = c("CS")
  )
  
  F0 <- data.frame(
    corstr = c("c1")
  )
  
  configs1 <- list(A1, B1, C, D, E, F0)
  configs <- list(configs1)
  configs %>%
    map(~ map(., ~ mutate(., id = 1)) %>%
          reduce(inner_join, by = "id") %>%
          tibble::rownames_to_column(var = "config_id")) %>%
    reduce(rbind) %>%
    write.csv("configs.csv")
}

make_configs()