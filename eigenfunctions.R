library(tidyverse)
library(rstan)
library(ggplot2)
library(reshape2)

approx_L = function(M, scale, x, sigma, l) {
  epsilon = sqrt(1 / (2 * l^2))
  alpha = 1 / scale
  beta = (1 + (2 * epsilon / alpha)^2)^0.25
  delta = sqrt(alpha^2 * (beta^2 - 1) / 2)
  
  N = length(x)
  Ht = matrix(0, nrow = N, ncol = M)
  xp = alpha * beta * x
  f = sqrt(epsilon^2 / (alpha^2 + delta^2 + epsilon^2))
  Ht[, 1] = sqrt(sqrt(alpha^2 / (alpha^2 + delta^2 + epsilon^2))) * sqrt(beta) * exp(-delta^2 * x * x)
  Ht[, 2] = f * sqrt(2) * xp * Ht[, 1]
  for(n in 3:M) {
    Ht[, n] = f * sqrt(2 / (n - 1)) * xp * Ht[, n - 1] - f^2 * sqrt((n - 2) / (n - 1)) * Ht[, n - 2]
  }
  
  sigma * Ht
}

cov_exp_quad = function(x, sigma, l) {
  outer(x, x, function(xi, xj) { sigma^2 * exp(-(xi - xj)^2 / (2 * l^2)) })
}

N = 50
M = 10
x = seq(-1.5, 1.5, length = N)
l = 0.25
sigma = 1.0

gete = function(scale) {
  L = approx_L(M, scale, x, sigma, l)
  
  max(log10(abs(L %*% t(L) - cov_exp_quad(x, sigma, l))))
}

# You can make plots of error four different scale parameters
scales = seq(0.1, 1.0, length = 100)
plot(scales, lapply(scales, gete))

# Or maybe just compare matrices
L = approx_L(M, 0.25, x, sigma, l)
LLT = L %*% t(L)
dimnames(LLT) = list(x, x)
K = cov_exp_quad(x, sigma, l)
dimnames(K) = list(x, x)
bind_rows(LLT %>% melt %>% mutate(which = "approximate"),
          K %>% melt %>% mutate(which = "exact")) %>%
  rename(x1 = Var1, x2 = Var2) %>%
  ggplot(aes(x1, x2, fill = value)) +
  geom_tile() +
  facet_grid(. ~ which) +
  coord_equal()

# Or maybe just plot some eigenfunctions
L = approx_L(M, 0.17, x, sigma, l)
bind_cols(as_tibble(L), as_tibble(x)) %>% rename(x = value) %>%
  gather(basis, value, 1:M) %>% ggplot(aes(x, value)) +
  geom_line(aes(group = basis, color = basis)) +
  ggtitle("Eigenfunctions (scaled by sqrt(eigenvalues))")
