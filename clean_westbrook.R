library(tidyverse)
library(ggplot2)
library(rstan)
library(lubridate)

setwd("~/basketball")

# This parses the NBA play by play data from bigdataball
csv = read_csv("data/[10-25-2016]-[06-12-2017]-combined-stats.csv")

df = csv %>% filter(event_type == "shot" | event_type == "miss") %>%
  filter(player == "Russell Westbrook") %>%
  group_by(date) %>% 
  mutate(result = as.integer(result == "made")) %>%
  filter(period < 5) %>%
  mutate(elapsed = as.duration(elapsed) / 60.0 + (period - 1) * 12) %>%
  ungroup() %>%
  select(result, period, elapsed) %>%
  mutate(x = elapsed / 48.0 - 0.5)

write_csv(df, "data/rw.csv")

