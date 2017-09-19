library(Matrix)
library(tidyverse)
library(ggplot2)
library(rstan)
library(lubridate)
library(stringr)
library(purrr)
library(shinystan)
library(gridExtra)
options(mc.cores = parallel::detectCores())

setwd("~/basketball")

df = readRDS("data/cleaned_lineup.Rdata")

df3 = df %>%
  #filter(team %in% c('CLE', 'GSW', 'LAC')) %>%#sample_n(10000) %>% 
  mutate(scored = as.numeric(pts > 0)) %>%
  select(team, scored, dlineup, dlineup_str) %>%
  rename(lineup = dlineup, lineup_str = dlineup_str) %>%
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

formula = as.formula(paste(" ~ -1 + team + (", paste(players, collapse = " + "), ")"))

X = model.matrix(formula, df4)

fit = stan("models/team_effect_binomial_sparse_centered.stan",
           data = list(N = nrow(X),
                       Ns = df4 %>% pull(n),
                       M = ncol(X),
                       T = length(teams),
                       X = X,
                       NZ = nnzero(X),
                       y = df4 %>% pull(made)),
           cores = 1,
           chains = 1,
           iter = 1000)

fitdf = bind_rows(as.tibble(extract(fit, "beta")$beta) %>%
                    setNames(players) %>%
                    gather(name, effect) %>%
                    left_join(df4 %>% group_by(team) %>% summarize(name = list(reduce(lineup, union))) %>% unnest()) %>%
                    mutate(type = "player"))#,
#as.tibble(extract(fit, "gamma")$gamma) %>%
#  setNames(teams) %>%
#  gather(name, effect) %>%
#  mutate(team = name,
#         type = "team"))

plots = fitdf %>%
  group_by(name) %>%
  summarize(team = team[1],
            type = type[1],
            m = median(effect),
            q1 = quantile(effect, 0.025),
            q2 = quantile(effect, 0.1667),
            q3 = quantile(effect, 0.6667),
            q4 = quantile(effect, 0.975),
            psum = sum(effect * (effect > 0.0))) %>%
  ungroup() %>%
  arrange(desc(m)) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  group_by(team) %>%
  do(plot = ggplot(data = ., aes(name, m)) + 
       geom_hline(aes(yintercept = 0.0), linetype = "dashed", col = "orangered") +
       geom_linerange(aes(ymin = q1, ymax = q2), col = "dodgerblue4") +
       geom_errorbar(aes(ymin = q2, ymax = q3)) +
       geom_linerange(aes(ymin = q3, ymax = q4), col = "dodgerblue4") +
       geom_point(aes(colour = team, shape = type), size = 4) + 
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
       ggtitle("95% intervals in blue, 67% in black, median is point"))



fitdf %>%
  group_by(name) %>%
  summarize(team = team[1],
            type = type[1],
            m = median(effect),
            q1 = quantile(effect, 0.025),
            q2 = quantile(effect, 0.1667),
            q3 = quantile(effect, 0.6667),
            q4 = quantile(effect, 0.975),
            psum = sum(effect * (effect > 0.0))) %>%
  ungroup() %>%
  filter(q4 - q1 < 1.0) %>%
  arrange(desc(m)) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  ggplot(aes(name, m)) + 
  geom_hline(aes(yintercept = 0.0), linetype = "dashed", col = "orangered") +
  geom_linerange(aes(ymin = q1, ymax = q2), col = "dodgerblue4") +
  geom_errorbar(aes(ymin = q2, ymax = q3)) +
  geom_linerange(aes(ymin = q3, ymax = q4), col = "dodgerblue4") +
  geom_point(aes(colour = team, shape = type), size = 4) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("95% intervals in blue, 67% in black, median is point")

left_join(df3, as_tibble(extract(fit, "p")$p) %>%
            setNames(df3$lineup_str) %>%
            gather(lineup_str, p)) %>% group_by(lineup_str) %>%
  summarize(m = mean(p), sd = sd(p), n = n[[1]]) %>% arrange(desc(n)) %>%
  ungroup() %>% ggplot(aes(m)) +
  geom_histogram()

left_join(df3 %>%
            group_by(team) %>%
            top_n(2, n), as.tibble(extract(fit, "made")$made) %>%
            setNames(df3 %>% pull(lineup_str)) %>%
            gather(lineup_str, pmade)) %>%
  ggplot(aes(made / n)) +
  geom_histogram(aes(pmade / n, fill = team), bins = 15) +
  geom_vline(aes(xintercept = made / n), col = "red") +
  facet_grid(lineup_str ~ .)

df5 = as.tibble(extract(fit, "p")$p) %>% gather(g, p) %>% group_by(g) %>%
  summarize(pm = mean(p),
            pu = quantile(p, 0.975),
            pl = quantile(p, 0.025)) %>% ungroup()

dfcheck = bind_cols(df5, df3 %>% mutate(m = made / n,
                                        sd = sqrt(m * (1 - m) / n)))

dfcheck %>%
  ggplot(aes(lineup_str, m)) +
  geom_linerange(aes(ymin = pl, ymax = pu)) +
  geom_point(aes(lineup_str, pm)) +
  geom_point(col = "red") +
  #geom_linerange(aes(ymax = m + 2 * sd, ymin = m - 2 * sd), col = "red") +
  theme(axis.text.x  = element_text(angle=15, vjust=0.5))

