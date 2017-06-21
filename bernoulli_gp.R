library(tidyverse)
library(ggplot2)
library(rstan)

setwd("~/basketball/")

N = 800
M = 10
scale = 0.25
x = seq(-0.5, 0.5, length = N)
y = sample(c(0, 1), N, replace = TRUE)

fit = stan("models/bernoulli_gp.stan",
     data = list(N = N, x = x, y = y),
     chains = 4, cores = 4, iter = 10000, warmup = 6000)

library(shinystan)

launch_shinystan(fit)

fit2 = stan("models/approx_bernoulli_gp.stan",
           data = list(N = N, M = M, scale = scale, x = x, y = y),
           chains = 4, cores = 4, iter = 2000, warmup = 1000
           control = list(metric = "dense_e"))

launch_shinystan(fit2)
