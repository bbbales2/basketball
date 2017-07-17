data {
  int<lower = 1> N;
  int<lower = 1> M;
  matrix[N, M] X;
  int y[N];
  int Ns[N];
}

parameters {
  real alpha;
  real<lower = 0.0> tau;
  vector[M] beta;
  //vector<lower = 0.0>[M] lambda;
}

model {
  tau ~ normal(0.0, 0.25);
  //lambda ~ cauchy(0.0, 1.0);
  beta ~ cauchy(0.0, tau);
  //beta ~ normal(0.0, tau);
  alpha ~ normal(0.0, 0.025);
  
  y ~ binomial_logit(Ns, X * beta + alpha + logit(0.36));
}

generated quantities {
  int made[N];
  {
    vector[N] tmp = inv_logit(X * beta + alpha + logit(0.36));

    for(n in 1:N)
      made[n] = binomial_rng(Ns[n], tmp[n]);
  }
}