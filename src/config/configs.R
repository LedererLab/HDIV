# dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
library(purrr)
library(dplyr)

make_configs <- function() {
  configs_A1 <- data.frame(
    n = c(400),
    px = c(500),
    pz = c(600)
    # n = c(400, 400),
    # px = c(500, 600),
    # pz = c(600, 800)
  )
  
  configs_B1 <- data.frame(
    s_beta = c(10),
    s_jmin = c(15),
    s_jmax = c(20)
  )
  
  configs_C <- data.frame(
    b = c(1),
    a = c(1)
  )
  
  configs_D <- data.frame(
    sigma_h = c(1),
    sigma0_v = c(1),
    sigma0_hv_mult = c(.5)
  )
  
  configs_E <- data.frame(
    type = c("CS")
  )
  
  configs_F <- data.frame(
    corstr = c("c1")
  )
  
  configs <- list(configs_A1, configs_B1, configs_C, configs_D, configs_E, configs_F)
  configs %>%
    map(~ mutate(., id = 1)) %>%
    reduce(inner_join, by = "id") %>%
    tibble::rownames_to_column(var = "config_id") %>%
    # write.csv(paste(dir, "configs.csv", sep="/"))
    write.csv("configs.csv")
}

make_configs()
