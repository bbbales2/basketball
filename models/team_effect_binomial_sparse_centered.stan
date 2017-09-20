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
  real<lower = 0.0> beta_tau;
  real<lower = 0.0> gamma_tau;
  vector[P] beta;
  vector[T] gamma;
}

transformed parameters {
  vector[M] combined;
  
  for(m in 1:P)
    combined[m + T] = beta[m];
  
  for(m in 1:T)
    combined[m] = gamma[m]; 
}

model {
  gamma_tau ~ normal(0.0, 0.05);
  beta_tau ~ normal(0.0, 0.05);
  beta ~ normal(0.0, beta_tau);
  gamma ~ normal(0.0, gamma_tau);
  
  y ~ binomial_logit(Ns, csr_matrix_times_vector(N, M, w, v, u, combined));
}

generated quantities {
  int made[N];
  vector[N] p = inv_logit(csr_matrix_times_vector(N, M, w, v, u, combined));
  
  {
    for(n in 1:N)
      made[n] = binomial_rng(Ns[n], p[n]);
  }
}
