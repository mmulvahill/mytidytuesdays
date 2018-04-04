#-------------------------------------------------------------------------------
# week1_ustuitioncosts.R
#   First week's exercise for R4DS's #TidyTuesday.
#
# Author: Matt Mulvahill
# week1 == 20180403
#-------------------------------------------------------------------------------


library(tidyverse)
library(readxl)
library(rlang)

orig <- read_xlsx("./data/us_avg_tuition.xlsx")

dat <- orig %>% 
  gather(key = "schyear", value = "cost", -State) %>%
  mutate(schyear = factor(schyear, ordered = TRUE))

dat <- dat %>%
  group_by(schyear) %>%
  summarise(cost = mean(cost)) %>%
  mutate(State = "National avg") %>%
  full_join(., dat) %>%
  ungroup

dat %>% 
  mutate(schyear = as.numeric(schyear)) %>%
  ggplot(aes(x = schyear, y = cost, color = State)) +
  geom_point() +
  geom_path()

dat %>%
  filter(schyear %in% c("2004-05", "2015-16")) %>%
  arrange(State, schyear) %>%
  spread(key = schyear, value = cost) %>%
  mutate(perc_change = (`2015-16` - `2004-05`)/ `2004-05`) %>%
  arrange(perc_change) %>%
  mutate(State = factor(State) %>% fct_reorder(., perc_change)) %>%
  ggplot(aes(x = State, y = perc_change)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))





