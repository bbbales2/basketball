data {
  int<lower = 1> N;
  int<lower = 1> M;
  matrix[N, M] X;
  int y[N];
  int Ns[N];
}

parameters {
  real gamma;
  real<lower = 0.0> alpha_tau;
  real<lower = 0.0> beta_tau;
  vector[N] alpha;
  vector[M] beta;
  //vector<lower = 0.0>[M] lambda;
}

/*transformed parameters {
  vector[M] beta = beta_tau * beta_raw;
  vector[N] alpha = alpha_tau * alpha_raw + X * beta + gamma;
}*/

model {
  gamma ~ normal(0.0, 0.05);
  alpha_tau ~ cauchy(0.0, 1.0);
  beta_tau ~ cauchy(0.0, 1.0);
  beta ~ normal(0.0, beta_tau);
  alpha ~ normal(X * beta + gamma, alpha_tau);
  
  y ~ binomial_logit(Ns, alpha + logit(0.36));
}

generated quantities {
  int made[N];
  vector[N] p = inv_logit(alpha + logit(0.36));
  
  {
    for(n in 1:N)
      made[n] = binomial_rng(Ns[n], p[n]);
  }
}
