library(MASS)
library(assist)
library(tidyverse)
library(rstan)
library(ggplot2)
library(shinystan)

setwd("~/basketball")

df = read_csv('~/basketball/data/rw_nba.csv') %>% sample_n(100)

sdata = list(N = nrow(df),
             x = df$x + 0.5,
             y = df$result)

fit = stan("models/bernoulli_bernoulli_yuedong.stan",
           data = sdata, chains = 4, cores = 4, iter = 2000)

launch_shinystan(fit)

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

summary %>% ggplot(aes(time, mean)) +
  geom_ribbon(aes(ymin = q1, ymax = q2), alpha = 0.75, fill = "dodgerblue2") +
  geom_ribbon(aes(ymin = q2, ymax = q3), alpha = 0.75, fill = "orangered1") +
  geom_ribbon(aes(ymin = q3, ymax = q4), alpha = 0.75, fill = "dodgerblue2") +
  geom_line() +
  geom_line(aes(time, q1), alpha = 1.0, size = 0.125) +
  geom_line(aes(time, q2), alpha = 1.0, size = 0.125) +
  geom_line(aes(time, q3), alpha = 1.0, size = 0.125) +
  geom_line(aes(time, q4), alpha = 1.0, size = 0.125) +
  #geom_point(data = out2, aes(time, f), size = 0.1, alpha = 0.01) +
  #geom_boxplot(data = summary2, aes(time, f, group = time)) +
  xlab("Game time") +
  ylab("Shooting percentage") +
  ggtitle("Russell Westbrook's shooting percentage (w/ est. 95% conf. intervals)")
