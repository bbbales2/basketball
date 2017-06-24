library(tidyverse)
library(ggplot2)
library(rstan)

setwd("~/basketball/")

N = 100
M = 10
scale = 0.25
x = seq(-0.5, 0.5, length = N)
y = rnorm(N) * 0.025 + 0.5

fit = stan("models/gp.stan",
           data = list(N = N, x = x, y = y),
           chains = 4, cores = 4, iter = 10000, warmup = 6000)

library(shinystan)

launch_shinystan(fit)

fit2 = stan("models/approx_gp.stan",
            data = list(N = N, M = M, scale = scale, x = x, y = y),
            chains = 4, cores = 4, iter = 2000, warmup = 1000)

launch_shinystan(fit2)
