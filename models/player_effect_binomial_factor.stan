data {
  int<lower = 1> N;
  int<lower = 1> F;
  int lineup[N, F];
  int team[N];
  int y[N];
  int Ns[N];
}

transformed data {
  int M = 0;
  int T = 0;
  
  for(n in 1:N)
    for(f in 1:F)
      M = max(M, lineup[n, f]);
      
  T = max(team);
}

parameters {
  real<lower = 0.0> alpha_tau;
  real<lower = 0.0> beta_tau;
  real<lower = 0.0> gamma_tau;
  real nba;
  vector[N] alpha;
  vector[M] beta;
  vector[T] gamma;
}

model {
  nba ~ normal(0.0, 1.0);
  alpha_tau ~ cauchy(0.0, 1.0);
  beta_tau ~ cauchy(0.0, 1.0);
  gamma_tau ~ cauchy(0.0, 1.0);
  beta ~ normal(0.0, beta_tau);
  for(n in 1:N)
    alpha[n] ~ normal(sum(beta[lineup[n]]) + gamma[team[n]] + nba, alpha_tau);
  gamma ~ normal(0.0, gamma_tau);
  
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
