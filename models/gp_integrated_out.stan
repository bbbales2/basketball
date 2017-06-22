data {
  int N;
  real x[N];
  vector[N] y;
}

parameters {
  real<lower = 0.0> sigma;
  real<lower = 0.0> l;
  real<lower = 0.0> sigmay;
}

model {
  real mu = 0.0;
  vector[N] muv = rep_vector(mu, N);
  matrix[N, N] Sigma = cov_exp_quad(x, sigma, l);
    
  for(n in 1:N)
    Sigma[n, n] = Sigma[n, n] + sigmay;
    
  mu ~ normal(0, 1);
  l ~ gamma(4, 4);
  sigma ~ normal(0, 1);
  sigmay ~ normal(0, 1);
  
  y ~ multi_normal(muv, Sigma);
}