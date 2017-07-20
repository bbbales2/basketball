data {
  int<lower = 1> N;
  int<lower = 1> M;
  int<lower = 0> NZ;
  matrix[N, M] X;
  int y[N];
  int Ns[N];
}

transformed data {
  vector[NZ] w = csr_extract_w(X);
  int v[NZ] = csr_extract_v(X);
  int u[N + 1] = csr_extract_u(X);
}

parameters {
  real<lower = 0.0> alpha_tau;
  real<lower = 0.0> beta_tau;
  vector[N] alpha;
  vector[M] beta;
}

model {
  alpha_tau ~ cauchy(0.0, 1.0);
  beta_tau ~ cauchy(0.0, 1.0);
  beta ~ normal(0.0, beta_tau);
  alpha ~ normal(csr_matrix_times_vector(N, M, w, v, u, beta), alpha_tau);
  
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
