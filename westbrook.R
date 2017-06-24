library(tidyverse)
library(ggplot2)
library(rstan)
library(shinystan)

setwd("~/basketball")

df = read_csv('data/rw_nba.csv')# %>% sample_n(1000)

sdata = list(N = nrow(df),
             M = 10,
             scale = 0.20,
             x = df$x,
             y = df$result,
             sigma = 1.0)

fit = stan('models/approx_bernoulli_gp_fixed_sigma.stan', data = sdata, chains = 4, cores = 4, iter = 2000)

extract(fit, c("l", "log10error")) %>% as_tibble %>% gather(param, value, c("l", "log10error")) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_grid(. ~ param, scales = "free_x")

sdata = list(N = nrow(df),
             x = df$period,
             y = df$result)

fit_grouped = stan('models/bernoulli_grouped.stan', data = sdata, chains = 4, cores = 4, iter = 2000)


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

a = as_tibble(extract(fit_grouped, "f")$f)
colnames(a) = 1:ncol(a)
b = as_tibble(list(period = 1:4, time = ((1:4) - 1) * 12 + 6))
out_grouped = inner_join(a %>% gather(period, f) %>% mutate(period = as.integer(period)), b, by = "period")

summary_grouped = out_grouped %>% group_by(time) %>%
  summarize(mean = mean(f),
            m = quantile(f, 0.125),
            p = quantile(f, 0.875)) %>%
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
  geom_errorbar(data = summary_grouped, aes(time, ymin = m, ymax = p), width = 2.0) +
  geom_point(data = summary_grouped, aes(time, mean)) +
  xlab("Game time") +
  ylab("Shooting percentage") +
  ggtitle("Russell Westbrook's shooting percentage (w/ est. 95% conf. intervals)")

summary %>% arrange(time) %>% slice(c(1, n()))
