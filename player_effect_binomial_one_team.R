library(tidyverse)
library(ggplot2)
library(rstan)
library(lubridate)
library(stringr)
library(purrr)
options(mc.cores = parallel::detectCores())

setwd("~/basketball")

df = readRDS("data/cleaned_lineup.Rdata")

df3 = df %>% filter(team == "GSW") %>% filter(three == TRUE) %>% 
  mutate(scored = as.numeric(pts > 0)) %>%
  select(scored, lineup, lineup_str) %>%
  group_by(lineup_str) %>%
  summarize(lineup = lineup[1],
            n = n(),
            made = sum(scored))

players = df3 %>% pull(lineup) %>% reduce(union)

df4 = df3 %>%
  mutate(!!!map(players, function(x) { quo(as.numeric(str_detect(lineup, !!x))) }) %>% setNames(players)) %>%
  select(-lineup) %>%
  select(made, everything())

formula = as.formula(paste(" ~ -1 + (", paste(players, collapse = " + "), ")"))

X = model.matrix(formula, df4)

fit = stan("models/player_effect_binomial.stan",
           data = list(N = nrow(X),
                       Ns = df4 %>% pull(n),
                       M = ncol(X),
                       X = X,
                       y = df4 %>% pull(made)),
           cores = 4,
           chains = 4,
           iter = 2000)

#launch_shinystan(fit)
#fit = stan_glm(formula, data = df4,
#         family = binomial(link = "logit"),
#         prior = normal(location = 0, 0.1), prior_intercept = normal(location = 0, 0.5),
#         cores = 1, chains = 1, iter = 400)
fitdf = bind_cols(as.tibble(extract(fit, "beta")$beta) %>% setNames(colnames(X)),
                  as.tibble(extract(fit, "gamma")$gamma) %>% setNames("intercept"))

fitdf %>%
  gather(name, effect) %>%
  group_by(name) %>%
  summarize(m = mean(effect),
            sd = sd(effect),
            u = quantile(effect, 0.975),
            l = quantile(effect, .025),
            psum = sum(effect * (effect > 0.0))) %>%
  ungroup() %>%
  mutate(iso = factor(str_detect(name, ":") + 2 * str_detect(name, "intercept"))) %>%
  arrange(desc(m)) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  ggplot(aes(name, m, colour = iso)) +
  geom_point() + 
  geom_linerange(aes(ymin = l, ymax = u)) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))

left_join(df3, as_tibble(extract(fit, "p")$p) %>%
            setNames(df3$lineup_str) %>%
            gather(lineup_str, p)) %>% group_by(lineup_str) %>%
  summarize(m = mean(p), sd = sd(p), n = n[[1]]) %>% arrange(desc(n)) %>%
  ungroup() %>% ggplot(aes(m)) +
  geom_histogram()

left_join(df3 %>% filter(n > 20), as.tibble(extract(fit, "made")$made) %>%
            setNames(df3 %>% pull(lineup_str)) %>%
            gather(lineup_str, pmade)) %>%
  ggplot(aes(made / n)) +
  geom_histogram(aes(pmade / n), bins = 15) +
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
