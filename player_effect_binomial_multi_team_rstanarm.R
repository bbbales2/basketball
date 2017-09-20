library(Matrix)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(stringr)
library(rstanarm)
library(rstan)
library(shinystan)
options(mc.cores = parallel::detectCores())

setwd("~/basketball")

df = readRDS("data/cleaned_lineup.Rdata")

which = "offense"
lineup_quo = if(which == "offense") quo(lineup) else quo(dlineup)
lineup_str_quo = if(which == "offense") quo(lineup_str) else quo(dlineup_str)
team_quo = if(which == "offense") quo(team) else quo(dteam)
df3 = df %>%
  filter((!!team_quo) %in% c('CLE', 'GSW')) %>%#, 'LAC'
  filter(three == TRUE) %>%
  mutate(scored = as.numeric(pts > 0)) %>%
  select(!!team_quo, scored, !!lineup_quo, !!lineup_str_quo) %>%
  rename(team = !!team_quo, lineup = !!lineup_quo, lineup_str = !!lineup_str_quo) %>%
  group_by(lineup_str) %>%
  summarize(team = team[1],
            lineup = lineup[1],
            n = n(),
            made = sum(scored)) %>% 
  ungroup() %>%
  arrange(desc(n))

players = df3 %>% pull(lineup) %>% reduce(union)

teams = df3 %>% pull(team) %>% unique()

df4 = df3 %>%
  mutate(!!!map(players, function(x) { quo(as.numeric(str_detect(lineup, !!x))) }) %>% setNames(players)) %>%
  select(made, everything())

formula = as.formula(paste("cbind(made, n - made) ~ (", paste(players, collapse = " + "), ")"))

prior = normal(location = 0, scale = 1.0)
fit = stan_glmer(formula, data = df4,
                family = binomial(link = "logit"), 
                prior = prior, prior_intercept = prior,  
                chains = 2, cores = 2, iter = 1000)

as.data.frame(fit) %>% as.tibble %>%# select(players) %>%
  gather(name, effect) %>%
  group_by(name) %>%
  summarize(m = median(effect),
            q1 = quantile(effect, 0.025),
            q2 = quantile(effect, 0.1667),
            q3 = quantile(effect, 0.6667),
            q4 = quantile(effect, 0.975)) %>%
  ungroup() %>%
  arrange(desc(m)) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  ggplot(data = ., aes(name, m)) + 
  geom_hline(aes(yintercept = 0.0), linetype = "dashed", col = "orangered") +
  geom_linerange(aes(ymin = q1, ymax = q2), col = "dodgerblue4") +
  geom_errorbar(aes(ymin = q2, ymax = q3)) +
  geom_linerange(aes(ymin = q3, ymax = q4), col = "dodgerblue4") +
  geom_point(size = 1) +
  ylab("effect") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("95% intervals in blue, 67% in black, median is point")

