data {
  int N;
  real x[N];
  int y[N];
}

parameters {
  vector[N] z;
  real<lower = 0.0> sigma;
  real<lower = 0.0> l;
}

transformed parameters {
  vector[N] f;
  
  {
    matrix[N, N] Sigma = cov_exp_quad(x, sigma, l);
    matrix[N, N] L;
    
    for(n in 1:N)
      Sigma[n, n] = Sigma[n, n] + 1e-12;
    
    L = cholesky_decompose(Sigma);
    f = inv_logit(L * z);
  }
}

model {
  l ~ gamma(4, 4);
  z ~ normal(0, 1);
  sigma ~ normal(0, 1);
  
  y ~ bernoulli(f);
}