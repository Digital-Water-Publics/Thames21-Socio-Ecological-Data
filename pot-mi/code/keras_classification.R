library(keras)
library(dplyr)
library(ggplot2)
library(purrr)

set.seed(19)
test_data = readRDS("data/river_queries/raw_data.RDS") %>%
  sample_n(2000)
