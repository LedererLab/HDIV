library(purrr)
library(dplyr)

make_configs <- function() {

  A1 <- data.frame(
    n = c(100, 400, 1000),
    px = c(250, 800, 1250),
    pz = c(500, 1000, 1500)
  )

  B0 <- data.frame(
    s_beta = c(3, 10, 20),
    s.j = c(5, 15, 25)
  )

  C1 <- data.frame(
    b = c(1)
  )
  C2 <- data.frame(
    a = c(1)
  )

  D1 <- data.frame(
    sigma0_h = c(1)
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
    corstr= c("c1")
  )

  configs0 <- list(A1, B0, C1, C2, D1, D2, D3, E, F0)
  configs <- list(configs0)
  configs %>%
    map(~ map(., ~ mutate(., id = 1)) %>%
          reduce(inner_join, by = "id") %>%
          tibble::rownames_to_column(var = "config_id")) %>%
    reduce(rbind) %>%
    write.csv("config/configs.csv")
}

make_configs()
