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

dft = csv %>% select(a1, a2, a3, a4, a5, h1, h2, h3, h4, h5, player, points, three, team) %>%
  mutate(away = (a1 == player) + (a2 == player) + (a3 == player) + (a4 == player) + (a5 == player)) %>%
  mutate(home = (h1 == player) + (h2 == player) + (h3 == player) + (h4 == player) + (h5 == player))

df3 = bind_rows(dft %>% rename(p1 = a1, p2 = a2, p3 = a3, p4 = a4, p5 = a5, pts = points) %>% filter(away == 1),
          dft %>% rename(p1 = h1, p2 = h2, p3 = h3, p4 = h4, p5 = h5, pts = points) %>% filter(home == 1)) %>%
    mutate_at(vars(p1, p2, p3, p4, p5), funs(str_replace_all(., "[^[:alnum:]]", ""))) %>%
    select(team, three, pts, p1, p2, p3, p4, p5) %>% filter(team == "POR") %>%
    filter(three == TRUE) %>% select(-team, -three)

players = df3 %>% gather(pos, name, c(p1, p2, p3, p4)) %>% pull(name) %>% unique()

detectors = map(players, function(x) { quo(str_detect(lineup, !!x)) }) %>% setNames(players)

df4 = df3 %>% mutate(lineup = pmap(list(p1, p2, p3, p4, p5), c)) %>%
  mutate(!!!map(players, function(x) { quo(as.numeric(str_detect(lineup, !!x))) }) %>% setNames(players)) %>%
  select(-p1, -p2, -p3, -p4, -p5, -lineup) %>%
  mutate(scored = as.numeric(pts > 0)) %>%
  select(-pts) %>%
  select(scored, everything())

formula = as.formula(paste("scored ~ (", paste(players, collapse = " + "), ")^2"))

fit = stan_glm(formula, data = df4,
         family = binomial(link = "logit"),
         prior = normal(location = 0, 0.1), prior_intercept = normal(location = 0, 0.5),
         cores = 1, chains = 1, iter = 2000)

as.tibble(fit) %>%
  gather(name, effect) %>%
  group_by(name) %>%
  summarize(m = mean(effect),
            sd = sd(effect),
            u = quantile(effect, 0.975),
            l = quantile(effect, .025)) %>%
  ungroup() %>%
  arrange(desc(m)) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  ggplot(aes(name, m)) +
  geom_point() + 
  geom_linerange(aes(ymin = l, ymax = u)) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))

glm(formula, family = binomial(), data = df4)

# %>%
#  select(name) %>% unique %>% arrange(team)

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
