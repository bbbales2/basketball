library(tidyverse)
library(lubridate)
library(forcats)
library(cmdstanr)
library(posterior)

# This parses the NBA play by play data from bigdataball
csv_raw = read_csv("data/combined.csv")

team_levels = csv_raw %>% pull(team) %>% unique %>% sort
team_fct = factor(team_levels, levels = team_levels)

csv = csv_raw %>%
  mutate(game_id = as.numeric(str_extract(csv %>% pull(game_id), "(\\d)+")),
         team = factor(team, levels = team_levels))

csv %>%
  filter(!is.na(points)) %>%
  pull(event_type) %>% unique

away_teams = csv %>%
  group_by(game_id) %>%
  filter(away_score != lag(away_score)) %>%
  slice_head(n = 1) %>%
  mutate(away_team = team) %>%
  select(game_id, away_team)

home_teams = csv %>%
  group_by(game_id) %>%
  filter(home_score != lag(home_score)) %>%
  slice_head(n = 1) %>%
  mutate(home_team = team) %>%
  select(game_id, home_team)

games_df = csv %>%
  select(game_id, date, home_score, away_score) %>%
  group_by(game_id) %>%
  slice_tail(n = 1) %>%
  left_join(home_teams) %>%
  left_join(away_teams)

mydata = list(N = nrow(games_df),
              M = max(team_fct %>% as.integer()),
              diff = games_df$home_score - games_df$away_score,
              hteam = games_df$home_team %>% as.integer(),
              ateam = games_df$away_team %>% as.integer())

model = cmdstan_model("team_skill.stan")

fit = model$sample(data = mydata,
                   parallel_chains = 4)

rvars = fit$draws() %>%
  as_draws_rvars()

tibble(team = team_fct,
       skill = rvars$skills[as.integer(team_fct)]) %>%
  arrange(-mean(skill)) %>%
  print(n = 30)

(fit$draws("skills") %>%
    as_draws_matrix())[, c(1, 2, 3)] %>%
  pairs
