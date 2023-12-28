library(tidyverse)
library(dplyr)
library(readr)

data <- read_csv('original-data.csv')

big_tot_chole_count <- nrow(data %>%
  filter(tot_chole > 500))

clean_data <- data %>% filter(tot_chole < 500)

write_csv(clean_data, file = "clean-data.csv")