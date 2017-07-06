library(tidyverse)
library(ggplot2)
library(rstan)
library(shinystan)

setwd("~/basketball")

df = read_csv('data/rw_nba.csv')# %>% sample_n(1000)

sdata = list(N = nrow(df),
             K = 80,
             x = df$x,
             y = df$result)

fit = stan('models/rff_bernoulli.stan', data = sdata, chains = 4, cores = 4, iter = 2000)

a = as_tibble(extract(fit, "f")$f)
colnames(a) = 1:ncol(a)
b = as_tibble(list(idx = 1:nrow(df), time = df$elapsed))
out = inner_join(a %>% gather(idx, f) %>% mutate(idx = as.integer(idx)), b, by = "idx")

summary = out %>% group_by(time) %>%
  summarize(mean = mean(f),
            q1 = quantile(f, 0.025),
            q2 = quantile(f, 0.167),
            q3 = quantile(f, 0.833),
            q4 = quantile(f, 0.975)) %>%
  ungroup()

#write_csv(summary, "westbrook_rff2.csv")

summary %>% ggplot(aes(time, mean)) +
  geom_ribbon(aes(ymin = q1, ymax = q2), alpha = 0.75, fill = "dodgerblue2") +
  geom_ribbon(aes(ymin = q2, ymax = q3), alpha = 0.75, fill = "orangered1") +
  geom_ribbon(aes(ymin = q3, ymax = q4), alpha = 0.75, fill = "dodgerblue2") +
  geom_line() +
  geom_line(aes(time, q1), alpha = 1.0, size = 0.125) +
  geom_line(aes(time, q2), alpha = 1.0, size = 0.125) +
  geom_line(aes(time, q3), alpha = 1.0, size = 0.125) +
  geom_line(aes(time, q4), alpha = 1.0, size = 0.125) +
  xlab("Game time") +
  ylab("Shooting percentage") +
  ggtitle("Russell Westbrook's shooting percentage (w/ est. 95% conf. intervals)")
