library(tidyverse)
library(rstanarm)
library(ggplot2)

# Create dataset for Bayesian exercise:
set.seed(123)
group_1_conversion <- rbinom(2000, 1, 0.045)
group_1_spend <- rnorm(2000, 160, 75)

group_1 <- tibble(group_1_conversion) %>%
  bind_cols(tibble(group_1_spend)) %>%
  mutate(spend = ifelse(group_1_conversion == 1, round(group_1_spend, 2), NA),
         group = 'Designer') %>%
  rename(conversion = group_1_conversion) %>%
  select(group, conversion, spend)

group_2_conversion <- rbinom(2000, 1, 0.035)
group_2_spend <- rnorm(2000, 150, 65)

group_2 <- tibble(group_2_conversion) %>%
  bind_cols(tibble(group_2_spend)) %>%
  mutate(spend = ifelse(group_2_conversion == 1, round(group_2_spend, 2), NA),
         group = 'In-House') %>%
  rename(conversion = group_2_conversion) %>%
  select(group, conversion, spend)

combined <- group_1 %>%
  bind_rows(group_2) %>%
  filter(!(conversion == 1 & spend < 5))

### Save a copy
saveRDS(combined, "data.RDS")
######################################################
######################################################