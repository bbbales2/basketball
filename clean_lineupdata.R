library(tidyverse)
library(ggplot2)
library(rstan)
library(lubridate)
library(stringr)
library(purrr)
library(rstanarm)
options(mc.cores = parallel::detectCores())

setwd("~/basketball")

# This parses the NBA play by play data from bigdataball
csv1 = read_csv("data/combined.csv")
csv = csv1 %>% sample_n(10000) %>% group_by(game_id)

csv = csv1 %>% filter(event_type == "shot" | event_type == "miss") %>%
  mutate(three = str_detect(description, "3PT"))# | event_type == "free throw"
# %>% sample_n(10000)

csv  %>%
  select(team, dteam)

dft = csv %>% left_join(csv %>%
                        select(game_id, team) %>%
                        group_by(game_id, team) %>%
                        unique %>%
                        group_by(game_id) %>%
                        mutate(dteam = rev(team)), by = c("game_id", "team")) %>%
  select(a1, a2, a3, a4, a5, h1, h2, h3, h4, h5, player, points, three, team, dteam) %>% rename(pts = points) %>%
  mutate(away = (a1 == player) + (a2 == player) + (a3 == player) + (a4 == player) + (a5 == player)) %>%
  mutate(home = (h1 == player) + (h2 == player) + (h3 == player) + (h4 == player) + (h5 == player))

df3 = bind_rows(dft %>% filter(away == 1) %>% rename(p1 = a1, p2 = a2, p3 = a3, p4 = a4, p5 = a5,
                                                     d1 = h1, d2 = h2, d3 = h3, d4 = h4, d5 = h5),
          dft %>% filter(home == 1) %>% rename(p1 = h1, p2 = h2, p3 = h3, p4 = h4, p5 = h5,
                                               d1 = a1, d2 = a2, d3 = a3, d4 = a4, d5 = a5)) %>%
    mutate_at(vars(p1, p2, p3, p4, p5, d1, d2, d3, d4, d5), funs(str_replace_all(., "[^[:alnum:]]", ""))) %>%
    mutate(lineup = pmap(list(p1, p2, p3, p4, p5), c),
           dlineup = pmap(list(d1, d2, d3, d4, d5), c)) %>%
    mutate(lineup_str = as.character(map(lineup, function(x) { paste(x, collapse = "\n") })),
           dlineup_str = as.character(map(dlineup, function(x) { paste(x, collapse = "\n") })))

saveRDS(df3, file = "data/cleaned_lineup.Rdata")
