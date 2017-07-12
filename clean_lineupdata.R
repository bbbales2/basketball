library(tidyverse)
library(ggplot2)
library(rstan)
library(lubridate)
library(stringr)
library(purrr)

setwd("~/basketball")

# This parses the NBA play by play data from bigdataball
csv = read_csv("data/combined.csv") %>% filter(event_type == "shot" | event_type == "free throw")
# %>% sample_n(10000)

dft = csv %>% select(a1, a2, a3, a4, a5, h1, h2, h3, h4, h5, player, points) %>%
  mutate(away = (a1 == player) + (a2 == player) + (a3 == player) + (a4 == player) + (a5 == player)) %>%
  mutate(home = (h1 == player) + (h2 == player) + (h3 == player) + (h4 == player) + (h5 == player))

dftt = bind_rows(dft %>% rename(p1 = a1, p2 = a2, p3 = a3, p4 = a4, p5 = a5, pts = points) %>%
                   mutate(diff = (away == 1) * pts - (away == 0) * pts) %>% select(home, away, diff, p1, p2, p3, p4, p5),
                 dft %>% rename(p1 = h1, p2 = h2, p3 = h3, p4 = h4, p5 = h5, pts = points) %>%
                   mutate(diff = (away == 0) * pts - (away == 1) * pts) %>% select(home, away, diff, p1, p2, p3, p4, p5)) %>%
  select(p1, p2, p3, p4, p5, diff) %>%
  mutate(rn = row_number()) %>%
  gather(position, name, c(p1, p2, p3, p4, p5)) %>%
  group_by(rn) %>%
  summarize(lineup = paste(sort(name), collapse = ', ')) %>%
  group_by(lineup) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
