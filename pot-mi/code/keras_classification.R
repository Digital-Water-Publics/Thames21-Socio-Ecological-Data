library(keras)
library(dplyr)
library(ggplot2)
library(purrr)

set.seed(19)
test_data = clean_tweet %>%
  sample_n(200)

test_min = sample_n(test_data,20)
