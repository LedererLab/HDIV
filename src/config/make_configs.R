# dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
library(purrr)
library(dplyr)

make_configs <- function() {

  A1 <- data.frame(
    n = c(400, 400, 800, 800),
    px = c(300, 500, 600, 1000),
    pz = c(600, 700, 1200, 1400)
  )  
  # A1 <- data.frame(
  #   n = c(400, 400, 400, 600, 600, 600, 800, 800, 800),
  #   px = c(300, 500, 600, 450, 750, 900, 600, 1000, 1200),
  #   pz = c(500, 600, 700, 750, 900, 1050, 1000, 1200, 1400)
  # )
  
  B0 <- data.frame(
    s_beta = c(2, 5, 10, 15, 25),
    sj.min = c(5, 10, 15, 20, 30),
    sj.max = c(10, 15, 20, 30, 50)
  )
  
  B1 <- data.frame(
    s_beta = c(10, 30),
    sj.min = c(15, 40),
    sj.max = c(20, 50)
  )
  
  C <- data.frame(
    b = c(1),
    a = c(1)
  )
  
  D <- data.frame(
    sigma0_h = c(1),
    sigma0_v = c(1),
    cor_hv = c(.5, .3, .1)
  )
  
  E <- data.frame(
    type = c("CS")
  )
  
  F0 <- data.frame(
    corstr = c("c1")
  )
  
  configs0 <- list(A1, B0, C, D, E, F0)
  # configs1 <- list(A1, B1, C, D, E, F0)
  configs <- list(configs0)
  configs %>%
    map(~ map(., ~ mutate(., id = 1)) %>%
          reduce(inner_join, by = "id") %>%
          tibble::rownames_to_column(var = "config_id")) %>%
    reduce(rbind) %>%
    write.csv("configs.csv")
}

make_configs()
