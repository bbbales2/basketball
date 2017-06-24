library(tidyverse)
library(ggplot2)
library(rstan)
library(lubridate)

setwd("~/basketball")

csv = read_csv('data/shots.csv', col_types = cols(.default = "?", GAME_CLOCK = "c"))

df = csv %>% select(time = GAME_CLOCK, period = PERIOD, result = SHOT_RESULT, name = NAME, pts_type = PTS_TYPE) %>%
  filter(name == "Russell Westbrook") %>%
  mutate(result = as.integer(result == "made")) %>%
  filter(period < 5) %>%
  mutate(time = as.numeric(as.duration(ms(time)))) %>%
  mutate(elapsed = (period - 1) * 12 + time / 60.0) %>%
  select(result, period, elapsed) %>%
  mutate(x = elapsed / 48.0 - 0.5)

write_csv(df, 'data/rw_nba.csv')
