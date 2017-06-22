# dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
library(purrr)
library(dplyr)

make_configs <- function() {

  A1 <- data.frame(
    # n = c(100, 400, 400, 800, 800),
    # px = c(200, 300, 500, 600, 1000),
    # pz = c(500, 600, 700, 1200, 1400)
    n = c(100, 400, 1000),
    px = c(250, 800, 1500),
    pz = c(500, 1000, 1750)
  )  
  # A1 <- data.frame(
  #   n = c(400, 400, 400, 600, 600, 600, 800, 800, 800),
  #   px = c(300, 500, 600, 450, 750, 900, 600, 1000, 1200),
  #   pz = c(500, 600, 700, 750, 900, 1050, 1000, 1200, 1400)
  # )
  
  B0 <- data.frame(
    # s_beta = c(2, 5, 10, 15, 25),
    # sj.min = c(5, 10, 15, 20, 30),
    # sj.max = c(10, 15, 20, 30, 50)
    s_beta = c(3, 10, 20),
    s.j = c(5, 15, 25)
    # sj.min = c(5, 10, 15, 30),
    # sj.max = c(10, 15, 20, 50)
  )
  
  C1 <- data.frame(
    b = c(1, .1)
  )
  C2 <- data.frame(
    a = c(1, .1)
  )
  
  D1 <- data.frame(
    sigma0_h = c(1)
    # cor_hv = c(.9, .7, .5, .3, .1)
  )
  
  D2 <- data.frame(
    sigma0_v = c(1)
  )
  
  D3 <- data.frame(
    cor_hv = c(.9, .5, .1)
  )
  
  E <- data.frame(
    type = c("CS", "TZ")
  )
  
  F0 <- data.frame(
    # corstr = c("c1", "c2")
    corstr= c("c1")
  )
  
  configs0 <- list(A1, B0, C1, C2, D1, D2, D3, E, F0)
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
