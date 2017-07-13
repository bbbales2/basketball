library(tidyverse)
library(ggplot2)
library(rstan)
library(lubridate)
library(stringr)
library(purrr)

setwd("~/basketball")

# This parses the NBA play by play data from bigdataball
csv1 = read_csv("data/combined.csv")
csv = csv1 %>% sample_n(10000) %>% group_by(game_id)

csv = csv1 %>% sample_n(10) %>% filter(event_type == "shot" | event_type == "miss") %>%
  mutate(three = str_detect(description, "3PT"))# | event_type == "free throw"
# %>% sample_n(10000)

dft = csv %>% select(a1, a2, a3, a4, a5, h1, h2, h3, h4, h5, player, points, three, team) %>%
  mutate(away = (a1 == player) + (a2 == player) + (a3 == player) + (a4 == player) + (a5 == player)) %>%
  mutate(home = (h1 == player) + (h2 == player) + (h3 == player) + (h4 == player) + (h5 == player))

df3 = bind_rows(dft %>% rename(p1 = a1, p2 = a2, p3 = a3, p4 = a4, p5 = a5, pts = points) %>% filter(away == 1),
          dft %>% rename(p1 = h1, p2 = h2, p3 = h3, p4 = h4, p5 = h5, pts = points) %>% filter(home == 1)) %>%
  select(team, three, pts, p1, p2, p3, p4, p5)

df3 %>% mutate(lineup = pmap(list(p1, p2, p3, p4, p5), c))

model.matrix( ~ p1 + p2, data = df3 %>% gather(pos, name, c(p1, p2, p3, p4, p5)) %>% mutate(name = as.factor(name)) %>% spread(pos, name))

df3 %>% group_by(team) %>% mutate(shotNumber = row_number()) %>% gather(pos, name, c(p1, p2, p3, p4, p5)) %>%
  select(name) %>% unique %>% arrange(team)

dftt = bind_rows(dft %>% rename(p1 = a1, p2 = a2, p3 = a3, p4 = a4, p5 = a5, pts = points) %>%
                   mutate(diff = (away == 1) * pts - (away == 0) * pts) %>% select(home, away, diff, p1, p2, p3, p4, p5),
                 dft %>% rename(p1 = h1, p2 = h2, p3 = h3, p4 = h4, p5 = h5, pts = points) %>%
                   mutate(diff = (away == 0) * pts - (away == 1) * pts) %>% select(home, away, diff, p1, p2, p3, p4, p5))

#%>%
  select(p1, p2, p3, p4, p5, diff) %>%
  mutate(rn = row_number()) %>%
  gather(position, name, c(p1, p2, p3, p4, p5)) %>%
  group_by(rn) %>%
  summarize(lineup = paste(sort(name), collapse = ', ')) %>%
  group_by(lineup) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
