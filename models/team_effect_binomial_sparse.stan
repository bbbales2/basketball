data {
  int<lower = 1> N;
  int<lower = 1> M;
  int<lower = 1> T;
  int<lower = 0> NZ;
  matrix[N, M] X;
  int y[N];
  int Ns[N];
}

transformed data {
  int P = M - T;
  vector[NZ] w = csr_extract_w(X);
  int v[NZ] = csr_extract_v(X);
  int u[N + 1] = csr_extract_u(X);
}

parameters {
  real<lower = 0.0> alpha_tau;
  vector<lower = 0.0>[P] beta_tau;
  real<lower = 0.0> gamma_tau;
  vector[N] alpha_raw;
  vector[P] beta_raw;
  vector[T] gamma_raw;
}

transformed parameters {
  vector[M] combined;
  vector[P] beta = beta_raw .* beta_tau;
  vector[T] gamma = gamma_raw * gamma_tau;
  vector[N] alpha;
  
  for(m in 1:P)
    combined[m + T] = beta[m];
  
  for(m in 1:T)
    combined[m] = gamma[m];
    
  alpha = csr_matrix_times_vector(N, M, w, v, u, combined) + alpha_raw * alpha_tau;
}

model {
  alpha_tau ~ cauchy(0.0, 1.0);
  beta_tau ~ cauchy(0.0, 1.0);
  gamma_tau ~ cauchy(0.0, 1.0);
  beta_raw ~ normal(0.0, 1.0);
  gamma_raw ~ normal(0.0, 1.0);
  alpha_raw ~ normal(0.0, 1.0);
  
  y ~ binomial_logit(Ns, alpha);
}

generated quantities {
  int made[N];
  vector[N] p = inv_logit(alpha);
  
  {
    for(n in 1:N)
      made[n] = binomial_rng(Ns[n], p[n]);
  }
}
